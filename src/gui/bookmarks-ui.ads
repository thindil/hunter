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

-- ****h* Bookmarks/BUI
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks.UI is
-- ****

   -- ****f* BUI/BUI.Create_Bookmark_Menu
   -- FUNCTION
   -- Create bookmarks menu - show only existing bookmarks
   -- PARAMETERS
   -- Create_New - If True, create new menu, otherwise use existing menu
   -- SOURCE
   procedure Create_Bookmark_Menu(Create_New: Boolean := False);
   -- ****

   -- ****f* BUI/BUI.Set_Bookmark_Button
   -- FUNCTION
   -- Show proper bookmark button for directories
   -- SOURCE
   procedure Set_Bookmark_Button;
   -- ****

end Bookmarks.UI;
