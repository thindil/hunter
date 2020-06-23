-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Hunter/Trash
-- FUNCTION
-- Provide code to manipulate system Trash
-- SOURCE
package Trash is
-- ****

   -- ****f* Trash/RestoreItem
   -- FUNCTION
   -- Restore selected file or directory from the trash
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was clicked. Unused.
   -- SOURCE
--   procedure RestoreItem(Self: access Gtk_Tool_Button_Record'Class);
   -- ****

   -- ****f* Trash/PathClicked
   -- FUNCTION
   -- Go to selected location and show it in current directory view.
   -- PARAMETERS
   -- Self - Button which was clicked by user
   -- SOURCE
--   procedure PathClicked(Self: access Gtk_Button_Record'Class);
   -- ****

   -- ****f* Trash/CreateTrashUI
   -- FUNCTION
   -- Create trash UI
   -- SOURCE
   procedure CreateTrashUI;
   -- ****

end Trash;
