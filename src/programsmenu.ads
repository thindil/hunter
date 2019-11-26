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

with Gtk.Popover; use Gtk.Popover;
with Gtk.Widget; use Gtk.Widget;

-- ****h* Hunter/ProgramsMenu
-- FUNCTION
-- Provides code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu is
-- ****

   -- ****f* ProgramsMenu/CreateProgramsMenu
   -- FUNCTION
   -- Create associated programs menu popover
   -- PARAMETERS
   -- Parent - Gtk_Widget button which will be parent for that menu
   -- RESULT
   -- Created popover menu
   -- SOURCE
   function CreateProgramsMenu(Parent: Gtk_Widget) return Gtk_Popover;
   -- ****

end ProgramsMenu;
