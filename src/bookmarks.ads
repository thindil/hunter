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

with Gtk.Menu; use Gtk.Menu;
with Gtk.Tool_Button; use Gtk.Tool_Button;

-- ****h* Hunter/Bookmarks
-- FUNCTION
-- Provide code for add, delete or go to locations bookmarks
-- SOURCE
package Bookmarks is
-- ****

   -- ****v* Bookmarks/BookmarksMenu
   -- FUNCTION
   -- Gtk_Menu with the user bookmarks
   -- SOURCE
   BookmarksMenu: Gtk_Menu;
   -- ****

   -- ****f* Bookmarks/GoHome
   -- FUNCTION
   -- Go to user home directory
   -- PARAMETERS
   -- Self - Gtk_Tool_Button clicked. Unused. Can be null.
   -- SOURCE
   procedure GoHome(Self: access Gtk_Tool_Button_Record'Class);
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

   -- ****f* Bookmarks/CreateBookmarksUI
   -- FUNCTION
   -- Create bookmarks UI - mostly register proper procedures and functions
   -- for use in GTKAda Builder
   -- SOURCE
   procedure CreateBookmarksUI;
   -- ****

end Bookmarks;
