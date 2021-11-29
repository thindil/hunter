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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with CArgv;
with Tcl.MsgCat.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip;
with Common; use Common;
with Utils.UI;

package body RenameItems.UI is

   -- ****o* RenameItemsUI/RenameItemsUI.Toggle_Rename_Command
   -- FUNCTION
   -- Show or hide text entry to enter a new name for the item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleRename
   -- SOURCE
   function Toggle_Rename_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Rename_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Directories;
      use Interfaces.C.Strings;
      use GNAT.OS_Lib;
      use Tcl.MsgCat.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkWidget;
      use Tcl.Tk.Ada.Winfo;
      use Tcl.Tklib.Ada.Tooltip;
      use Utils.UI;

      Text_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.textframe", Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget(pathName => Text_Frame & ".closebutton", Interp => Interp);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Text_Frame & ".textentry", Interp => Interp);
      Hunter_Rename_Exception: exception;
   begin
      if Winfo_Get(Widgt => Text_Entry, Info => "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
         Button.Name := New_String(Str => Text_Frame & ".okbutton");
         configure(Widgt => Button, options => "-command Rename");
         if Is_Directory(Name => To_String(Source => Current_Selected)) then
            Add
              (Widget => Button,
               Message =>
                 Mc
                   (Interp => Interp,
                    Src_String =>
                      "{Set a new name for the selected directory.}"));
            Add
              (Widget => Text_Entry,
               Message =>
                 Mc
                   (Interp => Interp,
                    Src_String =>
                      "{Enter a new name for the selected directory.}"));
         else
            Add
              (Widget => Button,
               Message =>
                 Mc
                   (Interp => Interp,
                    Src_String => "{Set a new name for the selected file.}"));
            Add
              (Widget => Text_Entry,
               Message =>
                 Mc
                   (Interp => Interp,
                    Src_String =>
                      "{Enter a new name for the selected file.}"));
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Widget => Button, StateSpec => "selected");
         Unbind(Widgt => Text_Entry, Sequence => "<KeyRelease>");
         Insert
           (TextEntry => Text_Entry, Index => "end",
            Text =>
              Simple_Name(Name => To_String(Source => Current_Selected)));
         Focus(Widgt => Text_Entry);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Text_Frame, Options => "-row 1 -columnspan 2 -sticky we");
         New_Action := RENAME;
         Toggle_Tool_Buttons(Action => New_Action);
      else
         if Invoke(Buttn => Button) /= "" then
            raise Hunter_Rename_Exception
              with Mc
                (Interp => Interp,
                 Src_String => "{Can't hide rename item bar}");
         end if;
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Widget => Button, StateSpec => "!selected");
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      -- ****
      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => ".mainframe.textframe.textentry", Interp => Interp);
      New_Name: Unbounded_String;
   begin
      New_Name := Common.Current_Directory & "/" & Get(Widgt => Text_Entry);
      if not Rename_Item
          (New_Name => To_String(Source => New_Name), Interp => Interp) then
         return TCL_OK;
      end if;
      return
        Toggle_Rename_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Rename_Command;

   procedure Add_Commands is
      use Utils;

   begin
      Add_Command
        (Name => "ToggleRename", Ada_Command => Toggle_Rename_Command'Access);
      Add_Command(Name => "Rename", Ada_Command => Rename_Command'Access);
   end Add_Commands;

end RenameItems.UI;
