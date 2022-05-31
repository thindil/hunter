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
with MainWindow; use MainWindow; --## rule line off REDUCEABLE_SCOPE

-- ****h* DeleteItems/DeleteItemsTUI
-- FUNCTION
-- Provide code to delete files and directories
-- SOURCE
package DeleteItems.UI is
-- ****

   -- ****f* DeleteItemsTUI/DeleteItemsTUI.Show_Delete_Form
   -- FUNCTION
   -- Show dialog to confirm deletion of items
   -- SOURCE
   procedure Show_Delete_Form;
   -- ****

   -- ****f* DeleteItemsTUI/DeleteItemsTUI.Delete_Keys
   -- FUNCTION
   -- Handles keys events when the deletion form is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Delete_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

end DeleteItems.UI;
