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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;

package body Modules is

   procedure LoadModules is
      FullPath: Unbounded_String;
   begin
      for ModulePath of Enabled_Modules loop
         FullPath :=
           To_Unbounded_String
             (Normalize_Pathname
                (To_String(ModulePath), Containing_Directory(Command_Name)));
         begin
            Tcl_EvalFile(Get_Context, To_String(FullPath) & "/module.tcl");
            Tcl_Eval
              (Get_Context,
               Simple_Name(To_String(ModulePath)) & "::on_start {" &
               To_String(FullPath) & "}");
         exception
            when Tcl_Error_Exception =>
               null;
         end;
      end loop;
   end LoadModules;

   procedure Execute_Modules(State: Triggers; Arguments: String := "") is
   begin
      for ModulePath of Enabled_Modules loop
         begin
            Tcl_Eval
              (Get_Context,
               Simple_Name(To_String(ModulePath)) & "::" &
               To_Lower(Triggers'Image(State)) & " " & Arguments);
         exception
            when Tcl_Error_Exception =>
               null;
         end;
      end loop;
   end Execute_Modules;

end Modules;
