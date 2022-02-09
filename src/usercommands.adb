-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Common; use Common;
with Messages.UI; use Messages.UI;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body UserCommands is

   -- ****o* UserCommands/UserCommands.Execute_Command_Command
   -- FUNCTION
   -- Execute the selected user command and show its output if needed
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteCommand menuentry
   -- Menuentry is the menu label of the command which will be executed
   -- SOURCE
   function Execute_Command_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Command_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Value, Command_Name: Unbounded_String;
      Space_Index: Natural;
      Result: Expect_Match := 1;
      Process_Desc: Process_Descriptor; --## rule line off IMPROPER_INITIALIZATION
      Arguments: Argument_List_Access;
      Success: Boolean := False;
   begin
      Value := User_Commands_List(CArgv.Arg(Argv => Argv, N => 1)).Command;
      Space_Index := Index(Source => Value, Pattern => " ");
      Command_Name :=
        To_Unbounded_String
          (Source =>
             Find_Executable
               (Name =>
                  To_String
                    (Source =>
                       (if Space_Index > 0 then
                          Unbounded_Slice
                            (Source => Value, Low => 1,
                             High => Space_Index - 1)
                        else Value))));
      if Command_Name = Null_Unbounded_String then
         Show_Message
           (Message =>
              Mc(Interp => Interp, Src_String => "{Can't find command:}") &
              " " & Slice(Source => Value, Low => 1, High => Space_Index));
         return TCL_OK;
      end if;
      if Space_Index > 0 then
         Arguments :=
           Argument_String_To_List
             (Arg_String =>
                Slice
                  (Source => Value, Low => Space_Index,
                   High => Length(Source => Value)));
      end if;
      Replace_Substitutes_Loop :
      for I in Arguments'Range loop --## rule line off SIMPLIFIABLE_STATEMENTS
         if Arguments(I).all = "@1" then
            Arguments(I) :=
              new String'(To_String(Source => Common.Current_Directory));
         elsif Arguments(I).all = "@2" then
            Arguments(I) := new String'(To_String(Source => Current_Selected));
         end if;
      end loop Replace_Substitutes_Loop;
      Non_Blocking_Spawn
        (Descriptor => Process_Desc,
         Command => Full_Name(Name => To_String(Source => Command_Name)),
         Args => Arguments.all);
      if User_Commands_List(CArgv.Arg(Argv => Argv, N => 1)).Need_Output then
         Show_Output;
         Update_Output_Loop :
         loop
            Expect
              (Descriptor => Process_Desc, Result => Result, Regexp => ".+",
               Timeout => 300_000);
            exit Update_Output_Loop when Result /= 1;
            Update_Output
              (Text_To_Append =>
                 Expect_Out_Match(Descriptor => Process_Desc) & LF);
            Success := True;
         end loop Update_Output_Loop;
      end if;
      Close(Descriptor => Process_Desc);
      return TCL_OK;
   exception
      when Process_Died =>
         if not Success then
            Show_Message
              (Message =>
                 Mc
                   (Interp => Interp,
                    Src_String => "{Can't execute command:}") &
                 " " & Slice(Source => Value, Low => 1, High => Space_Index));
         end if;
         return TCL_OK;
   end Execute_Command_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ExecuteCommand",
         Ada_Command => Execute_Command_Command'Access);
   end Add_Commands;

end UserCommands;
