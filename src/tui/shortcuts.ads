-- Copyright (c) 2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Shortcuts/Shortcuts
-- FUNCTION
-- Provide code to handle keyboard shortcuts
-- SOURCE
package Shortcuts is
-- ****

   -- ****f* Shortcuts/Shortcuts.Shortcuts_Keys
   -- FUNCTION
   -- Handles keyboard shortcuts in console version
   -- PARAMETERS
   -- Key           - Key pressed by the user
   -- AltKey        - If True, ALT key was pressed too
   -- Old_Localtion - The currently selected UI element
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Shortcuts_Keys
     (Key: Key_Code; AltKey: Boolean; Old_Location: Ui_Locations) return Ui_Locations;
   -- ****

end Shortcuts;
