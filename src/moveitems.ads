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

with CopyItems; use CopyItems;

package MoveItems is

   -- ****v* MoveItems/MoveItems.Move_Items_List
   -- FUNCTION
   -- Stores names of all selected to move files and directories
   -- SOURCE
   Move_Items_List: UnboundedString_Container.Vector;
   -- ****

end MoveItems;
