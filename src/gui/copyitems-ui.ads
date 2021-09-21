-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* CopyItems/CopyItemsUI
-- FUNCTION
-- Provide code for copy selected files or directories.
-- SOURCE
package CopyItems.UI is
-- ****

   -- ****f* CopyItemsUI/CopyItemsUI.Copy_Item
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

   -- ****f* CopyItemsUI/CopyItemsUI.Copy_Selected
   -- FUNCTION
   -- Copy selected files and directories
   -- PARAMETERS
   -- Overwrite - If True, overwrite existing file or directory, otherwise
   --             ask for overwrite permission. Value is set to False if
   --             permission was only for one file or directory
   -- SOURCE
   procedure Copy_Selected(Overwrite: in out Boolean);
   -- ****

   -- ****f* CopyItemsUI/CopyItemsUI.Skip_Copying
   -- FUNCTION
   -- Skip copying current file and move to next
   -- SOURCE
   procedure Skip_Copying;
   -- ****

   -- ****f* CopyItemsUI/CopyItemsUI.Create_Copy_Ui
   -- FUNCTION
   -- Create UI for copying items
   -- SOURCE
   procedure Create_Copy_Ui;
   -- ****

end CopyItems.UI;
