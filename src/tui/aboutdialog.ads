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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with MainWindow; use MainWindow;

-- ****h* AboutDialogTUI/AboutDialogTUI
-- FUNCTION
-- Provides code to show the dialog with information about the program
-- SOURCE
package AboutDialog is
-- ****

   -- ****f* AboutDialogTUI/AboutDialogTUI.Show_About_Dialog
   -- FUNCTION
   -- Show the dialog about the program
   -- SOURCE
   procedure Show_About_Dialog;
   -- ****

   -- ****f* AboutDialogTUI/AboutDialogTUI.About_Keys
   -- FUNCTION
   -- Handles keys events when the about dialog is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function About_View_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* AboutDialogTUI/AboutDialogTUI.Developers_Keys
   -- FUNCTION
   -- Handles keys events when the dialog with developers or translators is
   -- active element of UI
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Developers_Keys return UI_Locations;
   -- ****

end AboutDialog;
