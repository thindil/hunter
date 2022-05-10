-- Copyright (c) 2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Shortcuts is

   function Shortcut_Keys(Key: Key_Code; AltKey: Boolean; Old_Location: UI_Locations) return UI_Locations is
      Key_Value: constant String := Key_Name(Key);
      New_Key: Unbounded_String := Null_Unbounded_String;
      Index: Natural := 0;
   begin
      if AltKey then
         New_Key := To_Unbounded_String("Alt-" & Key_Value);
      else
         if Key_Value(Key_Value'First) /= '^' then
            New_Key := To_Unbounded_String(Key_Value);
         else
            New_Key :=
              To_Unbounded_String
                ("Control-" & To_Lower(Key_Value(Key_Value'Last)));
         end if;
      end if;
      for I in Accelerators'Range loop
         if Accelerators(I) = New_Key then
            Index := I;
            break;
         end if;
         case Index is
            when 1 =>
            when others =>
               null;
         end case;
      end loop;
      return Old_Location;
   end Rename_Keys;

end Shortcuts;
