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

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with MainWindow; use MainWindow;

-- ****h* BookmarksTUI/BookmarksTUI
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks is
-- ****

   -- ****t* BookmarksTUI/BookmarksTUI.Bookmarks_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Bookmarks_Container is new Indefinite_Hashed_Maps(String, String,
      Ada.Strings.Hash, "=");
   -- ****

   -- ****f* BookmarksTUI/BookmarksTUI.Create_Bookmarks_List
   -- FUNCTION
   -- Create the user's bookmarks list
   -- SOURCE
   procedure Create_Bookmarks_List;
   -- ****

   -- ****f* BookmarksTUI/BookmarksTUI.Show_Bookmarks_Menu
   -- FUNCTION
   -- Show the bookmarks menu to the user
   -- RESULT
   -- The list of bookmarks as menu options
   -- SOURCE
   function Show_Bookmarks_Menu return Item_Array_Access;
   -- ****

   -- ****f* BookmarksTUI/BookmarksTUI.Go_To_Bookmark
   -- FUNCTION
   -- Change currently viewed directory to the selected bookmark
   -- PARAMETERS
   -- Bookmark - The name of the bookmark to the which will be view switched
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Go_To_Bookmark(Bookmark: String) return UI_Locations;
   -- ****

end Bookmarks;
