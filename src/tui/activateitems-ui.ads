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

-- ****h* ActivateItems/AUITUI
-- FUNCTION
-- Provide code for open or execute selected files or directories.
-- SOURCE
package ActivateItems.UI is
-- ****

   -- ****f* AUITUI/AUITUI.Create_Activate_Ui
   -- FUNCTION
   -- Create activation UI
   -- SOURCE
   procedure Create_Activate_Ui is null;
   -- ****

   -- ****f* AUITUI/AUITUI.Show_Execute_With_Dialog
   -- FUNCTION
   -- Show dialog to enter the command with which the selected item will be
   -- executed
   -- SOURCE
   procedure Show_Execute_With_Dialog;
   -- ****

   -- ****f* AUITUI/AUITUI.Execute_Form_Keys
   -- FUNCTION
   -- Handles keys events when the execute with form is active element of
   -- UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Execute_Form_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

end ActivateItems.UI;
