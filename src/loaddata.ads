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

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Hunter/LoadData
-- FUNCTION
-- Provide code to load directories information.
-- SOURCE
package LoadData is
-- ****

   -- ****t* LoadData/Item_Record
   -- FUNCTION
   -- Data structure with information about files and directories
   -- OPTIONS
   -- Name        - The name of the file or directory
   -- Size        - For file size in bytes, for directory amount of items
   --               inside
   -- IsDirectory - If True, the item is a directory, otherwise false
   -- IsHidden    - If True, the item is hidden, otherwise false
   -- Modified    - Time of last modificiation of the item
   -- Image       - The name of the image used to show on the left to the
   --               item on the list
   -- HiddenItems - In directory, amount of hidden items
   -- Path        - The path to the selected item. Used in showing Trash
   --               content
   -- SOURCE
   type Item_Record is record
      Name: Unbounded_String;
      Size: Integer;
      IsDirectory: Boolean;
      IsHidden: Boolean;
      Modified: Time;
      Image: Unbounded_String;
      HiddenItems: Natural;
      Path: Unbounded_String;
   end record;
   -- ****

   type SortingOrder is
     (NameAsc, NameDesc, ModifiedAsc, ModifiedDesc, SizeAsc, SizeDesc);

   SortOrder: SortingOrder := NameAsc;

   function "<"(Left, Right: Item_Record) return Boolean;
   function "="(Left, Right: Item_Record) return Boolean;

   package Items_Container is new Vectors(Positive, Item_Record);
   package Items_Sorting is new Items_Container.Generic_Sorting;

   ItemsList, SecondItemsList: Items_Container.Vector;

   procedure AddItem(Path: String; List: in out Items_Container.Vector);
   procedure LoadDirectory(DirectoryName: String; Second: Boolean := False);

end LoadData;
