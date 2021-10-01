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

with Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Tcl.MsgCat.Ada;
with Messages.UI;

package body CreateItems is

   function Is_Creating_Possible
     (New_Item_Name, Item_Type: String; Interp: Tcl.Tcl_Interp)
      return Boolean is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use GNAT.OS_Lib;
      use Tcl.MsgCat.Ada;
      use Messages.UI;

      Action_String, Action_Blocker: Unbounded_String := Null_Unbounded_String;
   begin
      if Exists(Name => New_Item_Name) or
        Is_Symbolic_Link(Name => New_Item_Name) then
         Action_String :=
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interp, Src_String => "{create}") & " " &
                Item_Type & " " &
                Mc(Interp => Interp, Src_String => "{with}"));
         Action_Blocker :=
           (if Is_Directory(Name => New_Item_Name) then
              To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "directory"))
            else To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "file")));
         Show_Message
           (Message =>
              Mc(Interp => Interp, Src_String => "{You can't}") & " " &
              To_String(Source => Action_String) & " " &
              Mc(Interp => Interp, Src_String => "{name}") & " '" &
              New_Item_Name & "' " &
              Mc(Interp => Interp, Src_String => "{because there exists}") &
              " " & To_String(Source => Action_Blocker) & " " &
              Mc(Interp => Interp, Src_String => "{with that name.}"));
         return False;
      end if;
      if not Is_Write_Accessible_File
          (Name => Containing_Directory(Name => New_Item_Name)) then
         Show_Message
           (Message =>
              Mc
                (Interp => Interp,
                 Src_String => "{You don't have permissions to write to}") &
              " " & Containing_Directory(Name => New_Item_Name));
         return False;
      end if;
      return True;
   end Is_Creating_Possible;

end CreateItems;
