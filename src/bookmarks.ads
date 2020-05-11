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

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;

-- ****h* Hunter/Bookmarks
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks is
-- ****

   -- ****t* Bookmarks/Bookmarks_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Bookmarks_Container is new Indefinite_Hashed_Maps(String, String,
      Ada.Strings.Hash, "=");
   -- ****

   -- ****f* Bookmarks/CreateBookmarkMenu
   -- FUNCTION
   -- Create bookmarks menu - show only existing bookmarks
   -- PARAMETERS
   -- CreateNew - If True, create new menu, otherwise use existing menu
   -- SOURCE
   procedure CreateBookmarkMenu(CreateNew: Boolean := False);
   -- ****

   -- ****f* Bookmarks/SetBookmarkButton
   -- FUNCTION
   -- Show proper bookmark button for directories
   -- SOURCE
   procedure SetBookmarkButton;
   -- ****

end Bookmarks;
