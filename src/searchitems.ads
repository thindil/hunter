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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtkada.Builder; use Gtkada.Builder;
with Glib.Object; use Glib.Object;

-- ****h* Hunter/SearchItems
-- FUNCTION
-- Provide code to show search UI and show search results.
-- SOURCE
package SearchItems is
-- ****

   -- ****f* SearchItems/ToggleSearch
   -- FUNCTION
   -- Show or hide search text entry
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* SearchItems/VisibleItems
   -- FUNCTION
   -- Check if selected file, directory or application should be visible,
   -- when user search for selected names.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains all files and directories in
   --         current directory or all available applications
   -- Iter  - Gtk_Tree_Iter to currently checked file or directory or
   --         application
   -- RESULT
   -- True if selected file, directory or application should be visible,
   -- otherwise false.
   -- SOURCE
   function VisibleItems
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean;
   -- ****

   -- ****f* SearchItems/SearchItem
   -- FUNCTION
   -- Search for files and directories or applications (depends on what user
   -- search) as user enter text in search entry
   -- PARAMETERS
   -- User_Data - Which search entry was used (for files or applications)
   -- SOURCE
   procedure SearchItem(User_Data: access GObject_Record'Class);
   -- ****

end SearchItems;
