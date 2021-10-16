-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Common; use Common;
with LoadData; use LoadData;
with Utils.UI; use Utils.UI;

package body RenameItems.UI is

   -- ****o* RenameItemsUI/RenameItemsUI.Toggle_Rename_Command
   -- FUNCTION
   -- Show or hide text entry to enter a new name for the item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleRename
   -- SOURCE
   function Toggle_Rename_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Rename_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextFrame: constant Ttk_Frame :=
        Get_Widget(".mainframe.textframe", Interp);
      Button: Ttk_Button := Get_Widget(TextFrame & ".closebutton", Interp);
      TextEntry: constant Ttk_Entry :=
        Get_Widget(TextFrame & ".textentry", Interp);
      Hunter_Rename_Exception: exception;
   begin
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(TextFrame & ".okbutton");
         configure(Button, "-command Rename");
         if Is_Directory(To_String(Current_Selected)) then
            Add
              (Button,
               Mc(Interp, "{Set a new name for the selected directory.}"));
            Add
              (TextEntry,
               Mc(Interp, "{Enter a new name for the selected directory.}"));
         else
            Add(Button, Mc(Interp, "{Set a new name for the selected file.}"));
            Add
              (TextEntry,
               Mc(Interp, "{Enter a new name for the selected file.}"));
         end if;
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Button, "selected");
         Unbind(TextEntry, "<KeyRelease>");
         Insert(TextEntry, "end", Simple_Name(To_String(Current_Selected)));
         Focus(TextEntry);
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
         New_Action := RENAME;
         Toggle_Tool_Buttons(New_Action);
      else
         if Invoke(Button) /= "" then
            raise Hunter_Rename_Exception
              with Mc(Interp, "{Can't hide rename item bar}");
         end if;
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Button, "!selected");
      end if;
      return TCL_OK;
   end Toggle_Rename_Command;

   -- ****o* RenameItemsUI/RenameItemsUI.Rename_Command
   -- FUNCTION
   -- Rename currently selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Rename
   -- SOURCE
   function Rename_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      -- ****
      TextEntry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
      NewName: Unbounded_String;
   begin
      NewName :=
        Common.Current_Directory & "/" & To_Unbounded_String(Get(TextEntry));
      if not Rename_Item(To_String(NewName), Interp) then
         return TCL_OK;
      end if;
      return Toggle_Rename_Command(ClientData, Interp, Argc, Argv);
   end Rename_Command;

   procedure AddCommands is
   begin
      Add_Command("ToggleRename", Toggle_Rename_Command'Access);
      Add_Command("Rename", Rename_Command'Access);
   end AddCommands;

end RenameItems.UI;
