-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Trash/Trash
-- FUNCTION
-- Provide code to manipulate system Trash
-- SOURCE
package Trash is
-- ****

   -- ****f* Trash/Trash.Create_Trash
   -- FUNCTION
   -- Create trash related Tcl commands
   -- SOURCE
   procedure Create_Trash;
   -- ****

private

   --## rule off REDUCEABLE_SCOPE
   -- ****f* Trash/Load_Trash_Data
   -- FUNCTION
   -- Load the content of the Trash
   -- SOURCE
   procedure Load_Trash_Data;
   -- ****
   --## rule on REDUCEABLE_SCOPE

end Trash;
