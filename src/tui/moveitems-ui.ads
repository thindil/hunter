-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with CopyItems; use CopyItems;
with MainWindow; use MainWindow;

-- ****h* MoveItems/MoveItemsTUI
-- FUNCTION
-- Provide code to move selected files or directories.
-- SOURCE
package MoveItems.UI is
-- ****

   -- ****v* MoveItemsTUI/MoveItemsTUI.MoveItemsList
   -- FUNCTION
   -- Stores names of all selected to move files and directories
   -- SOURCE
   MoveItemsList: UnboundedString_Container.Vector;
   -- ****

   -- ****f* MoveItemsTUI/MoveItemsTUI.MoveSelected
   -- FUNCTION
   -- Move selected files and directories
   -- PARAMETERS
   -- Overwrite - If True, overwrite existing file or directory, otherwise
   --             ask for overwrite permission. Value is set to False if
   --             permission was only for one file or directory
   -- RESULT
   -- UI element which will be selected. If moving was finished, it will be
   -- directory view, if message about overwrite file was shown, it will be
   -- message form
   -- SOURCE
   function MoveSelected(Overwrite: in out Boolean) return UI_Locations;
   -- ****

   -- ****f* MoveItemsTUI/MoveItemsTUI.SkipMoving
   -- FUNCTION
   -- Skip moving current file and move to next
   -- RESULT
   -- UI element which will be selected. If moving was finished, it will be
   -- directory view, if message about overwrite file was shown, it will be
   -- message form
   -- SOURCE
   function SkipMoving return UI_Locations;
   -- ****

end MoveItems.UI;
