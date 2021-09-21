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

with MainWindow; use MainWindow;

-- ****h* CopyItems/CUITUI
-- FUNCTION
-- Provide code for copy selected files or directories.
-- SOURCE
package CopyItems.UI is
-- ****

   -- ****f* CopyItems/CUITUI.CopyItem
   -- FUNCTION
   -- Copy selected file or directory to new location
   -- PARAMETERS
   -- Name      - Full path to file or directory to copy
   -- Path      - Full path to new home location of file or directory
   -- Success   - True if item was successfully copied, otherwise False
   -- SOURCE
   procedure CopyItem
     (Name: String; Path: Unbounded_String; Success: in out Boolean);
   -- ****

   -- ****f* CopyItems/CUITUI.CopySelected
   -- FUNCTION
   -- Copy selected files and directories
   -- PARAMETERS
   -- Overwrite - If True, overwrite existing file or directory, otherwise
   --             ask for overwrite permission. Value is set to False if
   --             permission was only for one file or directory
   -- RESULT
   -- UI element which will be selected. If copying was finished, it will be
   -- directory view, if message about overwrite file was shown, it will be
   -- message form
   -- SOURCE
   function CopySelected(Overwrite: in out Boolean) return UI_Locations;
   -- ****

   -- ****f* CopyItems/CUITUI.SkipCopying
   -- FUNCTION
   -- Skip copying current file and move to next
   -- RESULT
   -- UI element which will be selected. If copying was finished, it will be
   -- directory view, if message about overwrite file was shown, it will be
   -- message form
   -- SOURCE
   function SkipCopying return UI_Locations;
   -- ****

end CopyItems.UI;
