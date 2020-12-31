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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;

package body Utils.UI is

   -- ****iv* Utils/Utils.ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Natural;
   -- ****

   function FindExecutable
     (Name: String; DisplayMessage: Boolean := True) return String is
      ExecutablePath: GNAT.OS_Lib.String_Access;
   begin
      return "";
   end FindExecutable;

   procedure SetProgressBar(Amount: Positive) is
   begin
      ProgressIndex := 0;
   end SetProgressBar;

   procedure UpdateProgressBar is
   begin
      ProgressIndex := ProgressIndex + 1;
   end UpdateProgressBar;

   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      null;
   end AddCommand;

   procedure ToggleToolButtons
     (Action: ItemActions; Finished: Boolean := False) is
   begin
      null;
   end ToggleToolButtons;

end Utils.UI;
