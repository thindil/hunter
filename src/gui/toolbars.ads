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

-- ****h* Toolbars/Toolbars
-- FUNCTION
-- Provide code for manipulate the program toolbars
-- SOURCE
package Toolbars is
-- ****

   -- ****f* Toolbars/Toolbars.SetToolbars
   -- FUNCTION
   -- Set proper orientation for the toolbars;
   -- SOURCE
   procedure SetToolbars;
   -- ****

   -- ****f* Toolbars/Toolbars.CreateActionToolbar
   -- FUNCTION
   -- Create ActionToolBar
   -- SOURCE
   procedure CreateActionToolbar;
   -- ****

   -- ****f* Toolbars/Toolbars.CreateItemToolbar
   -- FUNCTION
   -- Crete ItemToolBar
   -- SOURCE
   procedure CreateItemToolbar;
   -- ****

   -- ****f* Toolbars/Toolbars.SetActionsButtons
   -- FUNCTION
   -- Set visibility for buttons actions related to the currently selected item
   -- SOURCE
   procedure SetActionsButtons;
   -- ****

   -- ****f* Toolbars/Toolbars.SetUserCommandsMenu
   -- FUNCTION
   -- Set menu and button related to the user defined commands/actions
   -- SOURCE
   procedure SetUserCommandsMenu;
   -- ****

end Toolbars;