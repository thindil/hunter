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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtkada.Builder; use Gtkada.Builder;

package CopyItems is

   -- ****f* CopyItems/CopyData
   -- FUNCTION
   -- Copy selected files/directories to new location
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure CopyData(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* CopyItems/CopyItem
   -- FUNCTION
   -- Copy selected file or directory to new location
   -- PARAMETERS
   -- Name    - Full path to file or directory to copy
   -- Path    - Full path to new home location of file or directory
   -- Success - True if item was successfully copied, otherwise False
   -- SOURCE
   procedure CopyItem(Name: String; Path: in out Unbounded_String; Success: in out Boolean);
   -- ****

end CopyItems;
