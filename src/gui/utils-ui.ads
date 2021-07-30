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

with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with MainWindow; use MainWindow;

-- ****h* Utils/UtilsUI
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils.UI is
-- ****

   -- ****f* UtilsUI/UtilsUI.Find_Executable
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

     -- ****f* UtilsUI/UtilsUI.Update_Progress_Bar
     -- FUNCTION
     -- Update fraction of the progress bar or set it maximum value and show it
     -- to the user
     -- PARAMETERS
     -- Amount - The maximum value for the progress bar. Will be used to count
     --          the progress
     -- SOURCE
   procedure Update_Progress_Bar(Amount: Natural := 0);
   -- ****

   -- ****f* UtilsUI/UtilsUI.Set_Dialog
   -- FUNCTION
   -- Set the selected dialog
   -- PARAMETERS
   -- Dialog       - Tk_Toplevel dialog to set
   -- Dialog_Title - Title for the selected dialog
   -- Width        - Desired width for the selected dialog
   -- Height       - Desired height for the selected dialog
   -- SOURCE
   procedure Set_Dialog
     (Dialog: Tk_Toplevel; Dialog_Title: String; Width: Width_Range;
      Height: Height_Range);
   -- ****

   -- ****f* UtilsUI/UtilsUI.Add_Command
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

   -- ****f* UtilsUI/UtilsUI.Toggle_Tool_Buttons
   -- FUNCTION
   -- Show or hide other tool bar buttons when user starts any action with
   -- files or directories
   -- PARAMETERS
   -- Action   - Action on files or directories started or finished.
   -- Finished - If true, action was finished. Default is False
   -- SOURCE
   procedure Toggle_Tool_Buttons
     (Action: Item_Actions; Finished: Boolean := False);
   -- ****

end Utils.UI;
