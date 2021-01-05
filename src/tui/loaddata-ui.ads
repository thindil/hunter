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

-- ****h* LoadData/LUITUI
-- FUNCTION
-- Provide code to load directories information interface.
-- SOURCE
package LoadData.UI is
-- ****

   -- ****f* LUITUI/LUITUI.LoadDirectory
   -- FUNCTION
   -- Load content of the selected directory to the proper list
   -- PARAMETERS
   -- DirectoryName - Full path to the directory which content will be added
   --                 to the proper list
   -- Second        - If true, add directory content to the SecondItemsList,
   --                 otherwise add to the ItemsList. Default value is false
   -- SOURCE
   procedure LoadDirectory(DirectoryName: String; Second: Boolean := False);
   -- ****

end LoadData.UI;
