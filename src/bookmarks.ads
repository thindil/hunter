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

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with CopyItems; use CopyItems;

-- ****h* Bookmarks/Bookmarks
-- FUNCTION
-- Provide code for manipulate the program bookmarks
-- SOURCE
package Bookmarks is
-- ****

   -- ****t* Bookmarks/Bookmarks.Bookmarks_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Bookmarks_Container is new Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => String, Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   -- ****

   --## rule off GLOBAL_REFERENCES
   -- ****iv* Bookmarks/Bookmarks.Bookmarks_List
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   Bookmarks_List: Bookmarks_Container.Map;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****iv* Bookmarks/Bookmarks.Xdg_Bookmarks_List
   -- FUNCTION
   -- List of default XDG bookmarked locations
   -- SOURCE
   Xdg_Bookmarks_List: UnboundedString_Container.Vector;
   -- ****

private

   -- ****f* Bookmarks/Bookmarks.Create_Bookmarks_List
   -- FUNCTION
   -- Fill the user's bookmarks list
   -- SOURCE
   procedure Fill_Bookmarks_List;
   -- ****

end Bookmarks;
