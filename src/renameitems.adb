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

   function Rename_Item(New_Name: String; Interp: Tcl_Interp) return Boolean is
      Action_Blocker: Unbounded_String := Null_Unbounded_String;
      Success: Boolean;
   begin
      if Exists(Name => New_Name) or Is_Symbolic_Link(Name => New_Name) then
         Action_Blocker :=
           (if Is_Directory(Name => New_Name) then
              To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "{directory}"))
            else To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "{file}")));
         Show_Message
           (Message =>
              Mc(Interp => Interp, Src_String => "{You can't rename}") & " " &
              To_String(Source => Current_Selected) & " " &
              Mc(Interp => Interp, Src_String => "{to}") & " " & New_Name &
              " " &
              Mc(Interp => Interp, Src_String => "{because there exists}") &
              " " & To_String(Source => Action_Blocker) & " " &
              Mc(Interp => Interp, Src_String => "{with that name}"));
         Tcl_SetResult(interp => Interp, str => "0");
         return False;
      end if;
      if not Is_Write_Accessible_File(Name => Containing_Directory(Name => New_Name)) then
         Show_Message
           (Message => Mc(Interp => Interp, Src_String => "{You don't have permissions to rename}") & " " &
            New_Name);
         Tcl_SetResult(interp => Interp, str => "0");
         return False;
      end if;
      Rename_File(Old_Name => To_String(Source => Current_Selected), New_Name => New_Name, Success => Success);
      if not Success then
         Show_Message
           (Message => Mc(Interp => Interp, Src_String => "{Can't rename}") & " " & To_String(Source => Current_Selected) &
            ".");
         return False;
      end if;
      Current_Selected := To_Unbounded_String(Source => New_Name);
      Load_Directory(Directory_Name => To_String(Source => Common.Current_Directory));
      Update_Directory_List(True);
      Update_Watch(To_String(Common.Current_Directory));
      return True;
   end Rename_Item;

end RenameItems;
