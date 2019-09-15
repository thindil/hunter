-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Hunter/Bookmarks
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks is
-- ****

   -- ****f* Bookmarks/GoHome
   -- FUNCTION
   -- Go to user home directory
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure GoHome(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Bookmarks/CreateBookmarkMenu
   -- FUNCTION
   -- Create bookmarks menu - show only existing bookmarks
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure CreateBookmarkMenu(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Bookmarks/SetBookmarkButton
   -- FUNCTION
   -- Show proper bookmark button for directories
   -- SOURCE
   procedure SetBookmarkButton;
   -- ****

   -- ****f* Bookmarks/CreateBookmarksUI
   -- FUNCTION
   -- Create bookmarks UI - mostly register proper procedures and functions
   -- for use in GTKAda Builder
   -- SOURCE
   procedure CreateBookmarksUI;
   -- ****

end Bookmarks;
