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
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* LoadData/LoadData
-- FUNCTION
-- Provide code to load directories information.
-- SOURCE
package LoadData is
-- ****

   -- ****t* LoadData/LoadData.Item_Size
   -- FUNCTION
   -- Used to store size of files and directories
   -- SOURCE
   type Item_Size is range -2 .. File_Size'Last;
   -- ****

   -- ****s* LoadData/LoadData.Item_Record
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
      Size: Item_Size;
      IsDirectory: Boolean;
      IsHidden: Boolean;
      Modified: Time;
      Image: Unbounded_String;
      HiddenItems: Natural;
      Path: Unbounded_String;
   end record;
   -- ****

   -- ****t* LoadData/LoadData.SortingOrder
   -- FUNCTION
   -- Possible options of sorting for items in directory view
   -- OPTIONS
   -- NameAsc      - Sort by name ascending
   -- NameDesc     - Sort by name descending
   -- ModifiedAsc  - Sort by modified time ascending
   -- ModifiedDesc - Sort by modified time descending
   -- SizeAsc      - Sort by size ascending
   -- SizeDesc     - Sort by size descending
   -- SOURCE
   type SortingOrder is
     (NameAsc, NameDesc, ModifiedAsc, ModifiedDesc, SizeAsc, SizeDesc);
   -- ****

   -- ****v* LoadData/LoadData.SortOrder
   -- FUNCTION
   -- Currently set sorting order for directory view. Default value is NameAsc
   -- SOURCE
   SortOrder: SortingOrder := NameAsc;
   -- ****

   -- ****f* LoadData/LoadData."<"
   -- FUNCTION
   -- Used in sorting items in directory view. Check if one item is smaller
   -- than other
   -- PARAMETERS
   -- Left  - First item in directory view to compare
   -- Right - Second item in directory view to compare
   -- RESULT
   -- True if Left is smaller than Right, otherwise false. Depends on
   -- currently selected sorting order
   -- SOURCE
   function "<"(Left, Right: Item_Record) return Boolean;
   -- ****

   -- ****f* LoadData/LoadData."="
   -- FUNCTION
   -- Used in sorting items in directory view. Check if one item is equal to
   -- another
   -- PARAMETERS
   -- Left  - First item in directory view to compare
   -- Right - Second item in directory view to compare
   -- RESULT
   -- True if items are equal, otherwise false. Depends on currently selected
   -- sorting order
   -- SOURCE
   function "="(Left, Right: Item_Record) return Boolean;
   -- ****

   -- ****t* LoadData/LoadData.Items_Container
   -- FUNCTION
   -- Used to store information about selected directories contents
   -- SOURCE
   package Items_Container is new Vectors(Positive, Item_Record);
   -- ****

   -- ****t* LoadData/LoadData.Items_Sorting
   -- FUNCTION
   -- Used to sort the selected directories contets
   -- SOURCE
   package Items_Sorting is new Items_Container.Generic_Sorting;
   -- ****

   -- ****v* LoadData/LoadData.ItemsList
   -- FUNCTION
   -- The content of the currently viewed directory
   -- SOURCE
   ItemsList: Items_Container.Vector;
   -- ****

   -- ****v* LoadData/LoadData.SecondItemsList
   -- FUNCTION
   -- The content of the currently previewed directory or destination
   -- directory for actions like moving, copying, creating links
   -- SOURCE
   SecondItemsList: Items_Container.Vector;
   -- ****

   -- ****f* LoadData/LoadData.AddItem
   -- FUNCTION
   -- Add the selected item to the selected list
   -- PARAMETERS
   -- Path - Full path to the file or directory which will be added
   -- List - The list to which a new item will be added
   -- SOURCE
   procedure AddItem(Path: String; List: in out Items_Container.Vector);
   -- ****

   -- ****f* LoadData/LoadData.LoadDirectory
   -- FUNCTION
   -- Load content of the selected directory to the proper list
   -- PARAMETERS
   -- DirectoryName - Full path to the directory which content will be added
   --                 to the proper list
   -- Second        - If true, add directory content to the SecondItemsList,
   --                 otherwise add to the ItemsList. Default value is false
   -- SOURCE
   procedure LoadDirectory(DirectoryName: String; Second: Boolean := False);
   -- ****

end LoadData;
