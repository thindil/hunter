-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Utils; use Utils;

package body Modules.Commands is

   -- ****o* MCommands2/MCommands2.Toggle_Module_Command
   -- FUNCTION
   -- Enable or disable the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleModule path
   -- Path is the path to the module which will be enabled or disabled
   -- SOURCE
   function Toggle_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ModulePath: constant Unbounded_String :=
        To_Unbounded_String
          (Normalize_Pathname
             (CArgv.Arg(Argv, 1), Containing_Directory(Command_Name)));
   begin
      if Enabled_Modules.Contains(ModulePath) then
         Enabled_Modules.Delete(Enabled_Modules.Find_Index(ModulePath));
         Tcl_Eval(Interp, Simple_Name(To_String(ModulePath)) & "::on_disable");
         Tcl_Eval
           (Interp, "namespace delete " & Simple_Name(To_String(ModulePath)));
      else
         Enabled_Modules.Append(ModulePath);
         Tcl_EvalFile(Interp, To_String(ModulePath) & "/module.tcl");
         Tcl_Eval(Interp, Simple_Name(To_String(ModulePath)) & "::on_enable");
      end if;
      return TCL_OK;
   exception
      when Tcl_Error_Exception =>
         return TCL_ERROR;
   end Toggle_Module_Command;

   procedure AddCommands is
   begin
      AddCommand("ToggleModule", Toggle_Module_Command'Access);
   end AddCommands;

end Modules.Commands;
