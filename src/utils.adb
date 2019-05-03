-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Utils is

-- ****if* Utils/GetMimeType
-- SOURCE
   function GetMimeType(FileName: String) return String is
-- ****
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
   begin
      Non_Blocking_Spawn
        (ProcessDesc, "file",
         Argument_String_To_List("-b --mime-type " & FileName).all);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      case Result is
         when 1 =>
            declare
               MimeType: constant String := Expect_Out(ProcessDesc);
            begin
               Close(ProcessDesc);
               return MimeType;
            end;
         when others =>
            null;
      end case;
      Close(ProcessDesc);
      return "";
   end GetMimeType;

-- ****if* Utils/CanBeOpened
-- SOURCE
   function CanBeOpened(MimeType: String) return Boolean is
-- ****
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
   begin
      Non_Blocking_Spawn
        (ProcessDesc, Containing_Directory(Command_Name) & "/xdg-mime",
         Argument_String_To_List("query default " & MimeType).all);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      Close(ProcessDesc);
      return True;
   exception
      when Process_Died =>
         return False;
   end CanBeOpened;

end Utils;
