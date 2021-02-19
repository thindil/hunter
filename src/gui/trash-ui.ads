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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* Trash/TUI
-- FUNCTION
-- Provide code to manipulate system Trash
-- SOURCE
package Trash.UI is
-- ****

   -- ****o* TUI/TUI.Show_Trash_Command
   -- FUNCTION
   -- Show content of the Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- COMMANDS
   -- ShowTrash
   -- SOURCE
   function Show_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   -- ****f* Trash/TUI.CreateTrashUI
   -- FUNCTION
   -- Create trash UI
   -- SOURCE
   procedure CreateTrashUI;
   -- ****

end Trash.UI;
