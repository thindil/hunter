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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Bookmarks.Commands.UI; use Bookmarks.Commands.UI;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Modules; use Modules;
with Utils.UI; use Utils.UI;

package body Bookmarks.Commands is

   -- ****o* Commands/Commands.GoToBookmark_Command
   -- FUNCTION
   -- Go to the selected bookmarked directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToBookmark path
   -- Path is the full path to the directory which will be set as current
   -- directory (and show to the user)
   -- SOURCE
   function GoToBookmark_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToBookmark_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      if NewAction /= MOVE then
         if NewAction = CLEARTRASH then
            NewAction := SHOWTRASH;
         end if;
         if NewAction = SHOWTRASH then
            ToggleToolButtons(NewAction, True);
         end if;
         NewAction := COPY;
      end if;
      CurrentDirectory :=
        To_Unbounded_String(Normalize_Pathname(CArgv.Arg(Argv, 1)));
      LoadDirectory(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      Execute_Modules(On_Enter, "{" & To_String(CurrentDirectory) & "}");
      return TCL_OK;
   end GoToBookmark_Command;

   procedure AddCommands is
   begin
      AddCommand("GoToBookmark", GoToBookmark_Command'Access);
      AddUICommands;
   end AddCommands;

end Bookmarks.Commands;
