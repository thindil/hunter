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

-- ****h* ProgramsMenuTUI/ProgramsMenuTUI
-- FUNCTION
-- Provides code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu is
-- ****

   -- ****f* ProgramsMenuTUI/ProgramsMenuTUI.CreateProgramsMenu
   -- FUNCTION
   -- Create associated programs menu list
   -- SOURCE
   procedure CreateProgramsMenu;
   -- ****

   -- ****f* ProgramsMenuTUI/ProgramsMenuTUI.GetProgramName
   -- FUNCTION
   -- Search for name of application associated with selected desktop file
   -- PARAMETERS
   -- DesktopFile - File name of .desktop file to search
   -- RESULT
   -- Name of the application associated with the selected desktop file or
   -- DesktopFile if application was not found
   -- SOURCE
   function GetProgramName(DesktopFile: String) return String;
   -- ****

   -- ****f* ProgramsMenuTUI/ProgramsMenuTUI.ShowProgramsMenu
   -- FUNCTION
   -- Show list of applications available for use with the selected item
   -- SOURCE
   procedure ShowProgramsMenu;
   -- ****

   -- ****f* ProgramsMenuTUI/ProgramsMenuTUI.Programs_Keys
   -- FUNCTION
   -- Handles keys events when the programs menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Programs_Keys(Key: Key_Code) return UI_Locations;
   -- ****

end ProgramsMenu;
