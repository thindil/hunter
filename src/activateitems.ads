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

with Gtk.GEntry; use Gtk.GEntry;
with Glib.Object; use Glib.Object;

-- ****h* Hunter/ActivateItems
-- FUNCTION
-- Provide code for open or execute selected files or directories.
-- SOURCE
package ActivateItems is
-- ****

   -- ****f* ActivateItems/OpenItemWith
   -- FUNCTION
   -- Open selected item or directory with entered by user command. That
   -- command can have argumets either.
   -- PARAMETERS
   -- Self     - Text entry with command to use
   -- Icon_Pos - Position of text entry icon which was pressed or if key
   --            Enter was pressed, simulate pressing proper icon
   -- SOURCE
   procedure OpenItemWith
     (Self: access Gtk_Entry_Record'Class; Icon_Pos: Gtk_Entry_Icon_Position);
   -- ****

   -- ****f* ActivateItems/CreateActivateUI
   -- FUNCTION
   -- Create activation UI - mostly register proper procedures and functions
   -- for use in GTKAda Builder
   -- SOURCE
   procedure CreateActivateUI;
   -- ****

end ActivateItems;
