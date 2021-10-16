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

-- ****h* RenameItems/RenameItems
-- FUNCTION
-- Provide code to rename items.
-- SOURCE
package RenameItems is
-- ****

private

   -- ****f* RenameItems/RenameItems.Rename_Item
   -- FUNCTION
   -- Rename the selected file or directory
   -- NewName - The new name of the file or directory which will be renamed
   -- Interp  - Tcl Interpreter on which rename will be done
   -- RESULT
   -- True is rename was successful otherwise False
   -- SOURCE
   function Rename_Item(NewName: String; Interp: Tcl_Interp) return Boolean;
   -- ****

end RenameItems;
