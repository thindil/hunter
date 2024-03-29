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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* ProgramsMenu/ProgramsMenuUI
-- FUNCTION
-- Provides code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu.UI is
-- ****

   -- ****o* ProgramsMenuUI/ProgramsMenuUI.Toggle_Applications_Menu_Command
   -- FUNCTION
   -- Show or hide menu which allow to set a application which can be used
   -- to execute the selected file or directory
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleApplicationsMenu
   -- SOURCE
   function Toggle_Applications_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* ProgramsMenuUI/ProgramsMenuUI.Create_Programs_Menu_Ui
      -- FUNCTION
      -- Create associated programs menu popover
      -- SOURCE
   procedure Create_Programs_Menu_Ui;
   -- ****

end ProgramsMenu.UI;
