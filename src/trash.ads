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

with Gtk.Button; use Gtk.Button;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Trash/Trash
-- FUNCTION
-- Provide code to manipulate system Trash
-- SOURCE
package Trash is

   -- ****f* Trash/ClearTrash
   -- FUNCTION
   -- Show message to start clearing the trash.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ClearTrash(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Trash/ShowTrash
   -- FUNCTION
   -- Show content of the Trash
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ShowTrash(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Trash/RestoreItem
   -- FUNCTION
   -- Restore selected file or directory from the trash
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure RestoreItem(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Trash/PathClicked
   -- FUNCTION
   -- Go to selected location and show it in current directory view.
   -- PARAMETERS
   -- Self - Button which was clicked by user
   -- SOURCE
   procedure PathClicked(Self: access Gtk_Button_Record'Class);
   -- ****

end Trash;
