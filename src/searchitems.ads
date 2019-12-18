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

with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Glib.Object; use Glib.Object;

-- ****h* Hunter/SearchItems
-- FUNCTION
-- Provide code to show search UI and show search results.
-- SOURCE
package SearchItems is
-- ****

   SearchEntry: Gtk_Search_Entry;

   -- ****f* SearchItems/CreateSearchUI
   -- FUNCTION
   -- Create search UI - mostly register proper procedures and functions
   -- for use in GTKAda Builder
   -- SOURCE
   procedure CreateSearchUI;
   -- ****

end SearchItems;
