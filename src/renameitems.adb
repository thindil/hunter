-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl; use Tcl;
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
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with RefreshData; use RefreshData;
with Utils; use Utils;

package body RenameItems is

   -- ****o* RenameItems/Toggle_Rename_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextFrame: Ttk_Frame;
      Button: Ttk_Button;
      TextEntry: Ttk_Entry;
      Hunter_Rename_Exception: exception;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Button.Interp := Interp;
      Button.Name := New_String(".mainframe.textframe.closebutton");
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(".mainframe.textframe.okbutton");
         configure(Button, "-command Rename");
         if Is_Directory(To_String(CurrentSelected)) then
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
         Insert(TextEntry, "end", Simple_Name(To_String(CurrentSelected)));
         Focus(TextEntry);
         TextFrame.Interp := Interp;
         TextFrame.Name := New_String(".mainframe.textframe");
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
         NewAction := RENAME;
         ToggleToolButtons(NewAction);
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

   -- ****o* RenameItems/Rename_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      -- ****
      TextEntry: Ttk_Entry;
      NewName, ActionBlocker: Unbounded_String;
      Success: Boolean;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      NewName := CurrentDirectory & "/" & To_Unbounded_String(Get(TextEntry));
      if Exists(To_String(NewName)) or
        Is_Symbolic_Link(To_String(NewName)) then
         if Is_Directory(To_String(NewName)) then
            ActionBlocker := To_Unbounded_String(Mc(Interp, "{directory}"));
         else
            ActionBlocker := To_Unbounded_String(Mc(Interp, "{file}"));
         end if;
         ShowMessage
           (Mc(Interp, "{You can't rename }") & To_String(CurrentSelected) &
            Mc(Interp, "{ to }") & To_String(NewName) &
            Mc(Interp, "{ because there exists }") & To_String(ActionBlocker) &
            Mc(Interp, "{ with that name}"));
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewName))) then
         ShowMessage
           (Mc(Interp, "{You don't have permissions to rename }") &
            To_String(NewName));
         return TCL_OK;
      end if;
      Rename_File(To_String(CurrentSelected), To_String(NewName), Success);
      if not Success then
         ShowMessage
           (Mc(Interp, "{Can't rename }") & To_String(CurrentSelected) & ".");
         return TCL_OK;
      end if;
      CurrentSelected := NewName;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      UpdateWatch(To_String(CurrentDirectory));
      return Toggle_Rename_Command(ClientData, Interp, Argc, Argv);
   end Rename_Command;

   procedure CreateRenameUI is
   begin
      AddCommand("ToggleRename", Toggle_Rename_Command'Access);
      AddCommand("Rename", Rename_Command'Access);
   end CreateRenameUI;

end RenameItems;
