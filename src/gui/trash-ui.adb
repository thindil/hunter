-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Common; use Common;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Modules; use Modules;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body Trash.UI is

   function Show_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Button: Ttk_Button :=
        Get_Widget(".mainframe.toolbars.actiontoolbar.restorebutton");
   begin
      Load_Trash_Data;
      Update_Directory_List(True);
      if Items_List.Length = 0 then
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.deletebutton");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.separator3");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
      Bind_To_Main_Window
        (Interp, "<" & To_String(Accelerators(19)) & ">",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.restorebutton}");
      Execute_Modules
        (Interp, On_Enter_Trigger, "{" & To_String(Common.Current_Directory) & "}");
      return Show_Selected_Command(ClientData, Interp, Argc, Argv);
   end Show_Trash_Command;

   procedure CreateTrashUI is
   begin
      Add_Command("ShowTrash", Show_Trash_Command'Access);
   end CreateTrashUI;

end Trash.UI;
