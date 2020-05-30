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

with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Utils; use Utils;

package body RenameItems is

   function Toggle_Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* RenameItems/Start_Rename_Command
      -- FUNCTION
      -- Show text entry to enter a new name for the item
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Toggle_Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
      TextFrame: Ttk_Frame;
      Button: Ttk_Button;
      TextEntry: Ttk_Entry;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Button.Interp := Interp;
      Button.Name := New_String(".mainframe.textframe.closebutton");
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(".mainframe.textframe.okbutton");
         configure(Button, "-command Rename");
         Add(Button, "Set a new name for the selected item.");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Button, "selected");
         Add(TextEntry, "Enter a new name for the selected item.");
         Unbind(TextEntry, "<KeyRelease>");
         Focus(TextEntry);
         TextFrame.Interp := Interp;
         TextFrame.Name := New_String(".mainframe.textframe");
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
      else
         if Invoke(Button) /= "" then
            raise Program_Error with "Can't hide rename item bar";
         end if;
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.renamebutton");
         State(Button, "!selected");
      end if;
      return TCL_OK;
   end Toggle_Rename_Command;

   function Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* RenameItems/Rename_Command
      -- FUNCTION
      -- Rename currently selected file or directory
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Rename_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
   begin
      return TCL_OK;
   end Rename_Command;

   procedure CreateRenameUI is
   begin
      AddCommand("ToggleRename", Toggle_Rename_Command'Access);
      AddCommand("Rename", Rename_Command'Access);
   end CreateRenameUI;

end RenameItems;
