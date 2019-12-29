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

with Gtk.Tool_Button; use Gtk.Tool_Button;
with MainWindow; use MainWindow;

-- ****h* Hunter/MoveItems
-- FUNCTION
-- Provide code to move selected files or directories.
-- SOURCE
package MoveItems is
-- ****

   -- ****v* MoveItems/MoveItemsList
   -- FUNCTION
   -- Stores names of all selected to move files and directories
   -- SOURCE
   MoveItemsList: UnboundedString_Container.Vector;
   -- ****

   -- ****f* MoveItems/MoveData
   -- FUNCTION
   -- Move selected files/directories to new place
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was clicked. Unused.
   -- SOURCE
   procedure MoveData(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* MoveItems/MoveSelected
   -- FUNCTION
   -- Move selected files and directories
   -- PARAMETERS
   -- Overwrite - If True, overwrite existing file or directory, otherwise
   --             ask for overwrite permission. Value is set to False if
   --             permission was only for one file or directory
   -- SOURCE
   procedure MoveSelected(Overwrite: in out Boolean);
   -- ****

   -- ****f* MoveItems/SkipMoving
   -- FUNCTION
   -- Skip moving current file and move to next
   -- SOURCE
   procedure SkipMoving;
   -- ****

end MoveItems;
