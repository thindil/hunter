-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with RefreshData; use RefreshData;

package body RenameItems is

   function Rename_Item(NewName: String; Interp: Tcl_Interp) return Boolean is
      ActionBlocker: Unbounded_String;
      Success: Boolean;
   begin
      if Exists(NewName) or
        Is_Symbolic_Link(NewName) then
         ActionBlocker :=
           (if Is_Directory(NewName) then
              To_Unbounded_String(Mc(Interp, "{directory}"))
            else To_Unbounded_String(Mc(Interp, "{file}")));
         Show_Message
           (Mc(Interp, "{You can't rename}") & " " &
            To_String(Current_Selected) & " " & Mc(Interp, "{to}") & " " &
            NewName & " " & Mc(Interp, "{because there exists}") &
            " " & To_String(ActionBlocker) & " " &
            Mc(Interp, "{with that name}"));
         Tcl_SetResult(Interp, "0");
         return False;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(NewName)) then
         Show_Message
           (Mc(Interp, "{You don't have permissions to rename}") & " " &
            NewName);
         Tcl_SetResult(Interp, "0");
         return False;
      end if;
      Rename_File(To_String(Current_Selected), NewName, Success);
      if not Success then
         Show_Message
           (Mc(Interp, "{Can't rename}") & " " & To_String(Current_Selected) &
            ".");
         return False;
      end if;
      Current_Selected := To_Unbounded_String(NewName);
      Load_Directory(To_String(Common.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(Common.Current_Directory));
      return True;
   end Rename_Item;

end RenameItems;
