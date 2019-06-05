-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Hunter/LoadData
-- FUNCTION
-- Provide code to load directories information.
-- SOURCE
package LoadData is
-- ****

   -- ****f* LoadData/LoadDirectory
   -- FUNCTION
   -- Load selected directory with Name to Gtk_Store_List with ListName
   -- PARAMETERS
   -- Name     - Full path to the directory which content will be displayed
   -- ListName - Name of list which will be filled with data. Proper values
   --            are: "fileslist" for current directory and "fileslist1" for
   --            directory preview
   -- SOURCE
   procedure LoadDirectory(Name, ListName: String);
   -- ****

end LoadData;
