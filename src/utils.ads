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

with Ada.Directories; use Ada.Directories;
with MainWindow; use MainWindow;

-- ****h* Hunter/Utils
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils is
-- ****

   -- ****f* Utils/GetMimeType
   -- FUNCTION
   -- Check MIME Type of selected file
   -- PARAMETERS
   -- FileName - full path to the selected file to check MIME Type
   -- RESULT
   -- String with MIME Type of selected file
   -- SOURCE
   function GetMimeType(FileName: String) return String;
   -- ****
   -- ****f* Utils/CanBeOpened
   -- FUNCTION
   -- Check if there is application associated to open selected MIME Type
   -- PARAMETERS
   -- MimeType - MIME Type to check
   -- RESULT
   -- Return True if there is application associated with selected MIME Type, otherwise False
   -- SOURCE
   function CanBeOpened(MimeType: String) return Boolean;
   -- ****
   -- ****f* Utils/CountFileSize
   -- FUNCTION
   -- Convert file size to human readable format
   -- PARAMETERS
   -- Size - Size of file in bytes
   -- RESULT
   -- Return formated string with info about file size (in MiB, GiB, etc)
   -- SOURCE
   function CountFileSize(Size: File_Size) return String;
   -- ****
   -- ****f* Utils/FindExecutable
   -- FUNCTION
   -- Find executable file with selected name in this same directory where
   -- the program is or in PATH variable
   -- PARAMETERS
   -- Name - Name of executable file to find
   -- RESULT
   -- Full path to the executable file or empty string and show message if
   -- file could not be found.
   -- SOURCE
   function FindExecutable(Name: String) return String;
   -- ****
   -- ****f* Utils/ToggleToolButtons
   -- FUNCTION
   -- Show or hide other tool bar buttons when user starts any action with
   -- files or directories
   -- PARAMETERS
   -- Action   - Action on files or directories started or finished.
   -- Finished - If true, action was finished. Default is False
   -- SOURCE
   procedure ToggleToolButtons
     (Action: ItemActions; Finished: Boolean := False);
   -- ****

end Utils;
