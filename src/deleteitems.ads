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

with Tcl; use Tcl;

-- ****h* DeleteItems/DeleteItems
-- FUNCTION
-- Provide code to delete files and directories
-- SOURCE
package DeleteItems is
-- ****

   -- ****f* DeleteItems/DeleteItems.Delete_Selected
   -- FUNCTION
   -- Delete selected files and directories
   -- RESULT
   -- True if current directory was deleted too, otherwise false
   -- SOURCE
   function Delete_Selected(Interpreter: Tcl_Interp) return Boolean;
   -- ****

end DeleteItems;