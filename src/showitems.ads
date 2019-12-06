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

with Gtk.Stack; use Gtk.Stack;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Hunter/ShowItems
-- FUNCTION
-- Provide code to show informations and set some settings for selected files
-- or directories.
-- SOURCE
package ShowItems is
-- ****

   -- ****v* ShowItems/InfoStack
   -- FUNCTION
   -- Gtk_Stack which will show information about selected item
   -- SOURCE
   InfoStack: Gtk_Stack;
   -- ****

   -- ****f* ShowItems/PreviewItem
   -- FUNCTION
   -- Preview selected file or directory. If preview is not available, show
   -- info about selected file or directory.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure PreviewItem(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* ShowItems/ShowItem
   -- FUNCTION
   -- Show info about selected item or preview it.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ShowItem(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* ShowItems/CreateShowItemsUI
   -- FUNCTION
   -- Create UI related to show items and destination for moving/copying
   -- items.
   -- SOURCE
   procedure CreateShowItemsUI;
   -- ****

end ShowItems;
