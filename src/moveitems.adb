-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with MainWindow; use MainWindow;
with Messages; use Messages;

package body MoveItems is

-- ****iv* MoveItems/MoveItemsList
-- SOURCE
   MoveItemsList: UnboundedString_Container.Vector;
-- ****

-- ****if* MoveItems/MoveData
-- SOURCE
   procedure MoveData(Object: access Gtkada_Builder_Record'Class) is
-- ****
   begin
      if MoveItemsList.Length > 0
        and then Containing_Directory(To_String(MoveItemsList(1))) =
          To_String(CurrentDirectory) then
         return;
      end if;
      if MoveItemsList.Length = 0 then
         MoveItemsList := SelectedItems;
         return;
      end if;
      if not Is_Write_Accessible_File(To_String(CurrentDirectory)) then
         ShowMessage
           ("You don't have permissions to move selected items here.");
         return;
      end if;
      for Name of MoveItemsList loop
         Rename
           (To_String(Name),
            To_String(CurrentDirectory) & "/" & Simple_Name(To_String(Name)));
      end loop;
      MoveItemsList.Clear;
      Reload(Object);
   end MoveData;

end MoveItems;
