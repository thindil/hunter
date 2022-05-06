-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
--## rule off REDUCEABLE_SCOPE
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
--## rule on REDUCEABLE_SCOPE
with MainWindow; use MainWindow;

-- ****h* Bookmarks/BUITUI
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks.UI is
-- ****

   -- ****f* BUITUI/BUITUI.Create_Bookmarks_List
   -- FUNCTION
   -- Create the user's bookmarks list
   -- SOURCE
   procedure Create_Bookmarks_List;
   -- ****

   -- ****f* BUITUI/BUITUI.Show_Bookmarks_Menu
   -- FUNCTION
   -- Show the bookmarks menu to the user
   -- RESULT
   -- The list of bookmarks as menu options
   -- SOURCE
   function Show_Bookmarks_Menu return Item_Array_Access;
   -- ****

   -- ****f* BUITUI/BUITUI.Go_To_Bookmark
   -- FUNCTION
   -- Change currently viewed directory to the selected bookmark
   -- PARAMETERS
   -- Bookmark - The name of the bookmark to the which will be view switched
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Go_To_Bookmark(Bookmark: String) return UI_Locations;
   -- ****

   -- ****f* BUITUI/BUITUI.Bookmarks_Form_Keys
   -- FUNCTION
   -- Handles keys events when the enter destination form is active element of
   -- UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Bookmarks_Form_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* BUITUI/BUITUI.Add_Bookmark
   -- FUNCTION
   -- Add the currently selected directory as a new the program's bookmark
   -- SOURCE
   procedure Add_Bookmark;
   -- ****

   -- ****f* BUITUI/BUITUI.Remove_Bookmark
   -- FUNCTION
   -- Remove the currently selected directory from the program's booksmark list
   -- SOURCE
   procedure Remove_Bookmark;
   -- ****

end Bookmarks.UI;
