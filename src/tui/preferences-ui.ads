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

-- ****h* Preferences/PreferencesUITUI
-- FUNCTION
-- Provide code to show and change the program's options
-- SOURCE
package Preferences.UI is
-- ****

   -- ****f* PreferencesUITUI/PreferencesUITUI.Show_Options
   -- FUNCTION
   -- Show the program's options to the user
   -- SOURCE
   procedure Show_Options;
   -- ****

   -- ****f* PreferencesUITUI/PreferencesUITUI.Select_Preferences_Keys
   -- FUNCTION
   -- Handles keys events when the preferences view menu is active element of
   -- UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Select_Preferences_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* PreferencesUITUI/PreferencesUITUI.Select_Seconds_Keys
   -- FUNCTION
   -- Handles keys events when the selecting time interval menu is active
   -- element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Select_Seconds_Keys(Key: Key_Code) return UI_Locations;
   -- ****

end Preferences.UI;
