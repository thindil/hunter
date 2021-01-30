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

-- ****h* CreateItemsTUI/CreateItemsTUI
-- FUNCTION
-- Provide code for create files and directories.
-- SOURCE
package CreateItems is
-- ****

   -- ****f* CreateItemsTUI/CreateItemsTUI.AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the creation of items
   -- SOURCE
   procedure AddCommands;
   -- ****

   -- ****f* CreateItemsTUI/CreateItemsTUI.ShowCreateForm
   -- FUNCTION
   -- Show dialog to create file or directory
   -- PARAMETERS
   -- Create_Type - Type of item which will be created. Should be "file" or
   --               "directory"
   -- SOURCE
   procedure ShowCreateForm(Create_Type: String);
   -- ****

   -- ****f* CreateItemsTUI/CreateItemsTUI.Create_Keys
   -- FUNCTION
   -- Handles keys events when the create form is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Create_Keys(Key: Key_Code) return UI_Locations;
   -- ****

end CreateItems;
