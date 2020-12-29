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

with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with MainWindow; use MainWindow;

-- ****h* Utils/Utils
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils.UI is
-- ****

   -- ****f* Utils/Utils.FindExecutable
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

   -- ****f* Utils/Utils.SetProgressBar
   -- FUNCTION
   -- Set values for progress bar and show it to the user
   -- PARAMETERS
   -- Amount - Max amount of items - will be used to count progress
   -- SOURCE
   procedure SetProgressBar(Amount: Positive);
   -- ****

   -- ****f* Utils/Utils.UpdateProgressBar
   -- FUNCTION
   -- Update fraction of the progress bar
   -- SOURCE
   procedure UpdateProgressBar;
   -- ****

   -- ****f* Utils/Utils.SetDialog
   -- FUNCTION
   -- Set the selected dialog
   -- PARAMETERS
   -- Dialog      - Tk_Toplevel dialog to set
   -- DialogTitle - Title for the selected dialog
   -- Width       - Desired width for the selected dialog
   -- Height      - Desired height for the selected dialog
   -- SOURCE
   procedure SetDialog
     (Dialog: Tk_Toplevel; DialogTitle: String; Width: Width_Range;
      Height: Height_Range);
   -- ****

   -- ****f* Utils/Utils.AddCommand
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

   -- ****f* Utils/Utils.ToggleToolButtons
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

end Utils.UI;