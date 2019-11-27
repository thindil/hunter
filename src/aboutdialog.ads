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

with Gtk.Window; use Gtk.Window;

-- ****h* Hunter/AboutDialog
-- FUNCTION
-- Provides code for showing About the program dialog
-- SOURCE
package AboutDialog is
-- ****

   -- ****f* AboutDialog/ShowAboutDialog
   -- FUNCTION
   -- Create and show About Dialog to the user
   -- PARAMETERS
   -- Parent - Gtk_Window which will be parent for the dialog. Should be always
   --          main window.
   -- SOURCE
   procedure ShowAboutDialog(Parent: Gtk_Window);
   -- ****

end AboutDialog;
