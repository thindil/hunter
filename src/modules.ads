-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with MainWindow; use MainWindow;

package Modules is

   -- ****v* Modules/Modules.Enabled_Modules
   -- FUNCTION
   -- Store paths to the enabled the program's modules
   -- SOURCE
   Enabled_Modules: UnboundedString_Container.Vector;
   -- ****

   type Triggers is (On_Quit, On_Activate, On_Enter);

   -- ****f* Modules/Modules.LoadModules
   -- FUNCTION
   -- Load all enabled modules at start the program
   -- SOURCE
   procedure LoadModules;
   -- ****

   procedure Execute_Modules(State: Triggers; Arguments: String := "");

end Modules;
