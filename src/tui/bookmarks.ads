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

   -- ****f* BookmarksTUI/BookmarksTUI.Show_Bookmarks_Menu
   -- FUNCTION
   -- Show the bookmarks menu to the user
   -- SOURCE
   procedure Show_Bookmarks_Menu;
   -- ****

end Bookmarks;
