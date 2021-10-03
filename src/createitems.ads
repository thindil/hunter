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

with Tcl;

-- ****h* CreateItems/CreateItems
-- FUNCTION
-- Provide code for create files and directories.
-- SOURCE
package CreateItems is
-- ****

private

   -- ****f* CreateItems/CreateItems.Is_Creating_Possible
   -- FUNCTION
   -- Check if creation of the selected file or directory is possible
   -- PARAMETERS
   -- New_Item_Name - The name of file or directory which will be created
   -- Interp        - The Tcl interpreter on which the creation will be check
   -- RESULT
   -- True if creation is possible, otherwise False
   -- SOURCE
   function Is_Creating_Possible
     (New_Item_Name: String; Interp: Tcl.Tcl_Interp) return Boolean;
   -- ****

end CreateItems;
