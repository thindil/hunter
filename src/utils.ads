-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Ada;

-- ****h* Utils/Utils
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils is
-- ****

   -- ****t* Utils/Utils.Width_Range
   -- FUNCTION
   -- Used to store the width of the widgets
   -- SOURCE
   type Width_Range is new Integer;
   -- ****

   -- ****d* Utils/Utils.No_Width
   -- FUNCTION
   -- Zero value for the widgets width
   -- SOURCE
   No_Width: constant Width_Range := 0;
   -- ****

   -- ****f* Utils/Utils.Height_Range
   -- FUNCTION
   -- Used to store the height of the widgets
   -- SOURCE
   type Height_Range is new Integer;
   -- ****

   -- ****d* Utils/Utils.No_Height
   -- FUNCTION
   -- Zero value fo the widgets height
   -- SOURCE
   No_Height: constant Height_Range := 0;
   -- ****

   -- ****t* Utils/Utils.CreateCommands
   -- FUNCTION
   -- Used to add new commands to Tcl
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command
     (ClientData => Integer);
   -- ****

   -- ****f* Utils/Utils.Get_Mime_Type
   -- FUNCTION
   -- Check MIME Type of selected file
   -- PARAMETERS
   -- File_Name - full path to the selected file to check MIME Type
   -- RESULT
   -- String with MIME Type of selected file
   -- SOURCE
   function Get_Mime_Type(File_Name: String) return String;
   -- ****

   -- ****f* Utils/Utils.Can_Be_Opened
   -- FUNCTION
   -- Check if there is application associated to open selected MIME Type
   -- PARAMETERS
   -- Mime_Type - MIME Type to check
   -- RESULT
   -- Return True if there is application associated with selected MIME Type, otherwise False
   -- SOURCE
   function Can_Be_Opened(Mime_Type: String) return Boolean;
   -- ****

   -- ****f* Utils/Utils.Count_File_Size
   -- FUNCTION
   -- Convert file size to human readable format
   -- PARAMETERS
   -- Size - Size of file in bytes
   -- RESULT
   -- Return formated string with info about file size (in MiB, GiB, etc)
   -- SOURCE
   function Count_File_Size(Size: File_Size) return String;
   -- ****

   -- ****f* Utils/Utils.Is_Text
   -- FUNCTION
   -- Check if the selected MimeType is text so it can be previewed
   -- PARAMTERS
   -- MimeType - MIME Type to check
   -- RESULT
   -- Return True if the selected MIME Type is text type, otherwise False
   -- SOURCE
   function Is_Text(MimeType: String) return Boolean;
   -- ****

end Utils;
