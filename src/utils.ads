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

with Ada.Directories; use Ada.Directories;
with Tcl; use Tcl;
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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Utils/Utils.No_Width
   -- FUNCTION
   -- Zero value for the widgets width
   -- SOURCE
   No_Width: constant Width_Range := 0;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Utils/Utils.Height_Range
   -- FUNCTION
   -- Used to store the height of the widgets
   -- SOURCE
   type Height_Range is new Integer;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Utils/Utils.No_Height
   -- FUNCTION
   -- Zero value fo the widgets height
   -- SOURCE
   No_Height: constant Height_Range := 0;
   -- ****
   --## rule on REDUCEABLE_SCOPE

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
   -- Mime_Type - MIME Type to check
   -- RESULT
   -- Return True if the selected MIME Type is text type, otherwise False
   -- SOURCE
   function Is_Text(Mime_Type: String) return Boolean;
   -- ****

   -- ****f* Utils/Utils.Find_Executable
   -- FUNCTION
   -- Find executable file with selected name in this same directory where
   -- the program is or in PATH variable
   -- PARAMETERS
   -- Name            - Name of executable file to find
   -- Display_Message - If true, show message that executable cannot be found.
   --                   Default value is true.
   -- RESULT
   -- Full path to the executable file or empty string and show message if
   -- file could not be found.
   -- SOURCE
   function Find_Executable
     (Name: String; Display_Message: Boolean := True) return String;
      -- ****

   -- ****f* Utils/Utils.Add_Command
   -- FUNCTION
   -- Add command to the Tcl interpreter
   -- PARAMETERS
   -- Name        - Name of the Tcl command which will be used to invoke the
   --               Ada code
   -- Ada_Command - Ada function which will be invoked
   -- SOURCE
   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc);
   -- ****

end Utils;
