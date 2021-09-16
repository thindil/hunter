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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Messages; use Messages;

package body Utils.UI is

   -- ****iv* UtilsTUI/UtilsTUI.ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Natural;
   -- ****

   function Find_Executable
     (Name: String; Display_Message: Boolean := True) return String is
      ExecutablePath: GNAT.OS_Lib.String_Access;
   begin
      if Exists(Containing_Directory(Command_Name) & "/" & Name) then
         return Containing_Directory(Command_Name) & "/" & Name;
      end if;
      ExecutablePath := Locate_Exec_On_Path(Name);
      if ExecutablePath = null then
         if Display_Message then
            Show_Message
              (Mc(Interpreter, "{Could not found executable:}") & " " & Name);
         end if;
         return "";
      end if;
      return ExecutablePath.all;
   end Find_Executable;

   procedure Set_Progress_Bar(Amount: Positive) is
      pragma Unreferenced(Amount);
   begin
      ProgressIndex := 0;
   end Set_Progress_Bar;

   procedure Update_Progress_Bar is
   begin
      ProgressIndex := ProgressIndex + 1;
   end Update_Progress_Bar;

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Interpreter, Name, Ada_Command, 0, null);
      if Command = null then
         raise Hunter_Add_Command_Exception
           with Mc(Interpreter, "{Can't add command}") & " " & Name;
      end if;
   end Add_Command;

   procedure Toggle_Tool_Buttons
     (Action: ItemActions; Finished: Boolean := False) is
   begin
      null;
   end Toggle_Tool_Buttons;

end Utils.UI;
