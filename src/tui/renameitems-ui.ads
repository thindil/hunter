-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with MainWindow; use MainWindow;

-- ****h* RenameItemsTUI/RenameItemsTUI
-- FUNCTION
-- Provide code to rename items UI and rename them.
-- SOURCE
package RenameItems.UI is
-- ****

   -- ****f* RenameItemsTUI/RenameItemsTUI.AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the renaming of items
   -- SOURCE
   procedure AddCommands;
   -- ****

   -- ****f* RenameItemsTUI/RenameItemsTUI.ShowRenameForm
   -- FUNCTION
   -- Show dialog to rename file or directory
   -- SOURCE
   procedure ShowRenameForm;
   -- ****

   -- ****f* RenameItemsTUI/RenameItemsTUI.Rename_Keys
   -- FUNCTION
   -- Handles keys events when the rename form is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Rename_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

end RenameItems.UI;
