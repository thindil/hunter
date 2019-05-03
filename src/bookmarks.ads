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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtkada.Builder; use Gtkada.Builder;

package Bookmarks is

   -- Data structure for bookmarks
   type Bookmark_Record is record
      MenuName: Unbounded_String;
      Path: Unbounded_String;
   end record;
   package Bookmarks_Container is new Vectors(Positive, Bookmark_Record);
-- ****v* Bookmarks/BookmarksList
-- FUNCTION
-- List of all bookmarked locations
-- SOURCE
   BookmarksList: Bookmarks_Container.Vector;
-- ****

-- ****f* Bookmarks/GoToBookmark
-- FUNCTION
-- Go to selected bookmark location
-- SOURCE
   procedure GoToBookmark(Self: access Gtk_Menu_Item_Record'Class);
-- ****
-- ****f* Bookmarks/GoHome
-- FUNCTION
-- Go to user home directory
-- SOURCE
   procedure GoHome(Object: access Gtkada_Builder_Record'Class);
-- ****

end Bookmarks;
