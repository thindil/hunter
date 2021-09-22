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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* CopyItems/CopyItems
-- FUNCTION
-- Provide code for copy selected files or directories.
-- SOURCE
package CopyItems is
-- ****

   -- ****t* CopyItems/CopyItems.UnboundedString_Container
   -- FUNCTION
   -- Used to store various Unbounded_String data in list.
   -- SOURCE
   package UnboundedString_Container is new Vectors
     (Index_Type => Positive, Element_Type => Unbounded_String);
   -- ****

   -- ****v* CopyItems/CopyItems.Copy_Items_List
   -- FUNCTION
   -- Stores names of all selected to copy files and directories
   -- SOURCE
   Copy_Items_List: UnboundedString_Container.Vector;
   -- ****

   -- ****f* CopyItems/CopyItems.Copy_Item
   -- FUNCTION
   -- Copy selected file or directory to new location
   -- PARAMETERS
   -- Name      - Full path to file or directory to copy
   -- Path      - Full path to new home location of file or directory
   -- Success   - True if item was successfully copied, otherwise False
   -- SOURCE
   procedure Copy_Item
     (Name: String; Path: Unbounded_String; Success: in out Boolean);
   -- ****

end CopyItems;
