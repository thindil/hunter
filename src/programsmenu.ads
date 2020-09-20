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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* ProgramsMenu/ProgramsMenu
-- FUNCTION
-- Provides code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu is
-- ****

   -- ****o* ProgramsMenu/Toggle_Applications_Menu_Command
   -- FUNCTION
   -- Show or hide menu which allow to set a application which can be used
   -- to execute the selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleApplicationsMenu
   -- SOURCE
   function Toggle_Applications_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* ProgramsMenu/CreateProgramsMenu
      -- FUNCTION
      -- Create associated programs menu popover
      -- SOURCE
   procedure CreateProgramsMenu;
   -- ****

   -- ****f* ProgramsMenu/GetProgramName
   -- FUNCTION
   -- Search for name of application associated with selected desktop file
   -- PARAMETERS
   -- DesktopFile - File name of .desktop file to search
   -- RESULT
   -- Name of the application associated with the selected desktop file or
   -- DesktopFile if application was not found
   -- SOURCE
   function GetProgramName(DesktopFile: String) return String;
   -- ****

end ProgramsMenu;
