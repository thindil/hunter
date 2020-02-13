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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with LoadData; use LoadData;

package body MainWindow.Commands is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      if CArgv.Arg(Argv, 1) = "name" then
         if SortOrder = NameAsc then
            SortOrder := NameDesc;
         else
            SortOrder := NameAsc;
         end if;
      elsif CArgv.Arg(Argv, 1) = "modified" then
         if SortOrder = ModifiedAsc then
            SortOrder := ModifiedDesc;
         else
            SortOrder := ModifiedAsc;
         end if;
      elsif CArgv.Arg(Argv, 1) = "size" then
         if SortOrder = SizeAsc then
            SortOrder := SizeDesc;
         else
            SortOrder := SizeAsc;
         end if;
      end if;
      Items_Sorting.Sort(ItemsList);
      Reload;
      return 0;
   end Sort_Command;

   procedure AddCommands is
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise Program_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
   begin
      AddCommand("Sort", Sort_Command'Access);
   end AddCommands;

end MainWindow.Commands;
