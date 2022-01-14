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

with Common; use Common;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;

-- ****h* Utils/UtilsTUI
-- FUNCTION
-- Various utility subprograms.
-- SOURCE
package Utils.UI is
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

   -- ****f* UtilsTUI/UtilsTUI.Create_Dialog
   -- FUNCTION
   -- Create the selected dialog's window and set some default configuration
   -- for the selected dialog
   -- PARAMETERS
   -- DialogForm  - The dialog which will be configured
   -- FormWindow  - The dialog's window which will be created
   -- Form_Height - The height of the dialog's window
   -- Form_Length - The width of the dialog's window
   -- OUTPUT
   -- Parameters DialogForm as updated form, FormWindow as a newly created and
   -- updated height and width of the dialog's window
   -- SOURCE
   procedure Create_Dialog
     (DialogForm: in out Forms.Form; FormWindow: out Window;
      Form_Height: out Line_Position; Form_Length: out Column_Position);
   -- ****

   -- ****f* UtilsUI/UtilsUI.Delete_Dialog
   -- FUNCTION
   -- Delete the selected dialog and refresh the main window of the program
   -- PARAMETERS
   -- DialogForm - The dialog which will be destroyed
   -- Clear      - If True, the main directory preview list should be cleared.
   --              Default value is False.
   -- OUTPUT
   -- Parameter DialogForm as the deleted dialog
   -- SOURCE
   procedure Delete_Dialog
     (DialogForm: in out Forms.Form; Clear: Boolean := False);
   -- ****

   -- ****f* UtilsUI/UtilsUI.Go_Previous_Field
   -- FUNCTION
   -- Move cursor to the previous field in the dialog form
   -- PARAMETERS
   -- DialogFor - The dialog in which the cursor will be moved
   -- RESULT
   -- Form.Driver_Result of the move
   -- SOURCE
   function Go_Previous_Field
     (DialogForm: Forms.Form) return Forms.Driver_Result;
   -- ****

   -- ****f* UtilsUI/UtilsUI.Go_Next_Field
   -- FUNCTION
   -- Move cursor to the next field in the dialog form
   -- PARAMETERS
   -- DialogFor - The dialog in which the cursor will be moved
   -- RESULT
   -- Form.Driver_Result of the move
   -- SOURCE
   function Go_Next_Field(DialogForm: Forms.Form) return Forms.Driver_Result;
   -- ****

   -- ****f* UtilsTUI/UtilsTUI.Toggle_Tool_Buttons
   -- FUNCTION
   -- Show or hide other tool bar buttons when user starts any action with
   -- files or directories. This is null procedure, needed only to
   -- compatibility with the graphical version
   -- PARAMETERS
   -- Action   - Action on files or directories started or finished.
   -- Finished - If true, action was finished. Default is False
   -- SOURCE
   procedure Toggle_Tool_Buttons
     (Action: Item_Actions; Finished: Boolean := False) is null;
   -- ****

end Utils.UI;
