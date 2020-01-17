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

with Gtk.Toolbar; use Gtk.Toolbar;

package Toolbars is

   -- ****v* Toolbars/ActionToolBar
   -- FUNCTION
   -- Gtk_Toolbar with actions buttons (copy, move, delete, new, etc)
   -- SOURCE
   ActionToolBar: Gtk_Toolbar;
   -- ****

   -- ****v* Toolbars/ItemToolBar
   -- FUNCTION
   -- Gtk_Toolbar with acions related to selected file or directory (preview,
   -- show info, open, etc)
   -- SOURCE
   ItemToolBar: Gtk_Toolbar;
   -- ****

   -- ****f* Toolbars/CreateActionToolbarUI
   -- FUNCTION
   -- Create ActionToolBar UI - add buttons and separators
   -- SOURCE
   procedure CreateActionToolbarUI;
   -- ****

   -- ****f* Toolbars/CreateItemToolbarUI
   -- FUNCTION
   -- Crete ItemToolBar UI - add buttons and separators
   -- SOURCE
   procedure CreateItemToolbarUI;
   -- ****

   -- ****f* Toolbars/SetToolbars
   -- FUNCTION
   -- Set position of toolbars - on the top or the left of the main window.
   -- SOURCE
   procedure SetToolbars;
   -- ****

end Toolbars;
