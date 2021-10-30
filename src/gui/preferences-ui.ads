-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Preferences/PreferencesUI
-- FUNCTION
-- Provide code for save/restore and manipulate the program settings
-- SOURCE
package Preferences.UI is
-- ****

   -- ****f* PreferencesUI/PreferencesUI.Create_Preferences_Ui
   -- FUNCTION
   -- Create preferences UI and fill it with data from the program settings
   -- SOURCE
   procedure Create_Preferences_Ui;
   -- ****

   -- ****f* PreferencesUI/PreferencesUI.Clear_Add_Command
   -- FUNCTION
   -- Clear form for edit or add user defined commands
   -- SOURCE
   procedure Clear_Add_Command;
   -- ****

end Preferences.UI;
