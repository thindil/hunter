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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Interfaces.C;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with MainWindow; use MainWindow;
with Messages; use Messages;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body UserCommands is

   -- ****o* UserCommands/UserCommands.Execute_Command_Command
   -- FUNCTION
   -- Execute the selected user command and show its output if needed
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteCommand menuentry
   -- Menuentry is the menu label of the command which will be executed
   -- SOURCE
   function Execute_Command_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Command_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Value, CommandName: Unbounded_String;
      SpaceIndex: Natural;
      Result: Expect_Match;
      ProcessDesc: Process_Descriptor;
      Arguments: Argument_List_Access;
      Success: Boolean := False;
   begin
      Value := UserCommandsList(CArgv.Arg(Argv, 1)).Command;
      SpaceIndex := Index(Value, " ");
      CommandName :=
        (if SpaceIndex > 0 then Unbounded_Slice(Value, 1, SpaceIndex - 1)
         else Value);
      CommandName :=
        To_Unbounded_String(Find_Executable(To_String(CommandName)));
      if CommandName = Null_Unbounded_String then
         ShowMessage
           (Mc(Interp, "{Can't find command:}") &
            " " &
            Slice(Value, 1, SpaceIndex));
         return TCL_OK;
      end if;
      if SpaceIndex > 0 then
         Arguments :=
           Argument_String_To_List(Slice(Value, SpaceIndex, Length(Value)));
      end if;
      Replace_Substitutes_Loop:
      for I in Arguments'Range loop
         if Arguments(I).all = "@1" then
            Arguments(I) :=
              new String'(To_String(MainWindow.Current_Directory));
         elsif Arguments(I).all = "@2" then
            Arguments(I) := new String'(To_String(Current_Selected));
         end if;
      end loop Replace_Substitutes_Loop;
      Non_Blocking_Spawn
        (ProcessDesc,
         Full_Name(To_String(CommandName)),
         Arguments.all);
      if UserCommandsList(CArgv.Arg(Argv, 1)).NeedOutput then
         ShowOutput;
         Update_Output_Loop:
         loop
            Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 300_000);
            exit Update_Output_Loop when Result /= 1;
            UpdateOutput(Expect_Out_Match(ProcessDesc) & LF);
            Success := True;
         end loop Update_Output_Loop;
      end if;
      Close(ProcessDesc);
      return TCL_OK;
   exception
      when Process_Died =>
         if not Success then
            ShowMessage
              (Mc(Interp, "{Can't execute command:}") &
               " " &
               Slice(Value, 1, SpaceIndex));
         end if;
         return TCL_OK;
   end Execute_Command_Command;

   procedure AddCommands is
   begin
      Add_Command("ExecuteCommand", Execute_Command_Command'Access);
   end AddCommands;

end UserCommands;
