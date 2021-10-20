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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Tcl.Ada; use Tcl.Ada;

package body Modules is

   procedure Load_Modules(Interpreter: Tcl_Interp) is
      use Ada.Command_Line;
      use GNAT.OS_Lib;

      Full_Path: Unbounded_String := Null_Unbounded_String;
   begin
      Load_Modules_Loop :
      for ModulePath of Enabled_Modules loop
         Full_Path :=
           To_Unbounded_String
             (Source =>
                Normalize_Pathname
                  (Name => To_String(Source => ModulePath),
                   Directory => Containing_Directory(Name => Command_Name)));
         Load_Module_Block :
         begin
            Tcl_EvalFile
              (interp => Interpreter,
               fileName => To_String(Source => Full_Path) & "/module.tcl");
            Tcl_Eval
              (interp => Interpreter,
               strng =>
                 Simple_Name(Name => To_String(Source => ModulePath)) &
                 "::on_start {" & To_String(Source => Full_Path) & "}");
         exception
            when Tcl_Error_Exception =>
               null;
         end Load_Module_Block;
      end loop Load_Modules_Loop;
   end Load_Modules;

   procedure Execute_Modules
     (Interpreter: Tcl_Interp; State: Triggers; Arguments: String := "") is

   begin
      Execute_Modules_Loop :
      for ModulePath of Enabled_Modules loop
         Execute_Module_Block :
         declare
            use Ada.Characters.Handling;
         begin
            Tcl_Eval
              (interp => Interpreter,
               strng =>
                 Simple_Name(Name => To_String(Source => ModulePath)) & "::" &
                 To_Lower(Item => Triggers'Image(State)) & " " & Arguments);
         exception
            when Tcl_Error_Exception | Constraint_Error =>
               null;
         end Execute_Module_Block;
      end loop Execute_Modules_Loop;
   end Execute_Modules;

end Modules;
