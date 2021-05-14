-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Utils/UtilsTUI
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils.UI is
-- ****

   -- ****f* UtilsTUI/UtilsTUI.Find_Executable
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
   function Find_Executable
     (Name: String;
      Display_Message: Boolean := True) return String;
   -- ****

   -- ****f* UtilsTUI/UtilsTUI.Set_ProgressBar
   -- FUNCTION
   -- Set values for progress bar and show it to the user
   -- PARAMETERS
   -- Amount - Max amount of items - will be used to count progress
   -- SOURCE
   procedure Set_Progress_Bar(Amount: Positive);
   -- ****

   -- ****f* UtilsTUI/UtilsTUI.Update_Progress_Bar
   -- FUNCTION
   -- Update fraction of the progress bar
   -- SOURCE
   procedure Update_Progress_Bar;
   -- ****

   -- ****f* UtilsTUI/UtilsTUI.Add_Command
   -- FUNCTION
   -- Add command to the Tcl interpreter
   -- PARAMETERS
   -- Name        - Name of the Tcl command which will be used to invoke the
   --               Ada code
   -- Ada_Command - Ada function which will be invoked
   -- SOURCE
   procedure Add_Command
     (Name: String;
      Ada_Command: not null CreateCommands.Tcl_CmdProc);
   -- ****

   -- ****f* UtilsTUI/UtilsTUI.Toggle_Tool_Buttons
   -- FUNCTION
   -- Show or hide other tool bar buttons when user starts any action with
   -- files or directories
   -- PARAMETERS
   -- Action   - Action on files or directories started or finished.
   -- Finished - If true, action was finished. Default is False
   -- SOURCE
   procedure Toggle_Tool_Buttons
     (Action: ItemActions;
      Finished: Boolean := False);
   -- ****

end Utils.UI;
