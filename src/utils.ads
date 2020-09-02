-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with MainWindow; use MainWindow;

-- ****h* Utils/Utils
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils is
-- ****

   -- ****t* Utils/CreateCommands
   -- FUNCTION
   -- Used to add new commands to Tcl
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
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
   -- Name           - Name of executable file to find
   -- DisplayMessage - If true, show message that executable cannot be found.
   --                  Default value is true.
   -- RESULT
   -- Full path to the executable file or empty string and show message if
   -- file could not be found.
   -- SOURCE
   function FindExecutable
     (Name: String; DisplayMessage: Boolean := True) return String;
   -- ****

   -- ****f* Utils/SetProgressBar
   -- FUNCTION
   -- Set values for progress bar and show it to the user
   -- PARAMETERS
   -- Amount - Max amount of items - will be used to count progress
   -- SOURCE
   procedure SetProgressBar(Amount: Positive);
   -- ****

   -- ****f* Utils/UpdateProgressBar
   -- FUNCTION
   -- Update fraction of the progress bar
   -- SOURCE
   procedure UpdateProgressBar;
   -- ****

   -- ****f* Utils/SetDialog
   -- FUNCTION
   -- Set the selected dialog
   -- PARAMETERS
   -- Dialog      - Tk_Toplevel dialog to set
   -- DialogTitle - Title for the selected dialog
   -- Width       - Desired width for the selected dialog
   -- Height      - Desired height for the selected dialog
   -- SOURCE
   procedure SetDialog
     (Dialog: Tk_Toplevel; DialogTitle: String; Width, Height: Positive);
   -- ****

   -- ****f* Utils/AddCommand
   -- FUNCTION
   -- Add command to the Tcl interpreter
   -- PARAMETERS
   -- Name       - Name of the Tcl command which will be used to invoke the
   --              Ada code
   -- AdaCommand - Ada function which will be invoked
   -- SOURCE
   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc);
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
