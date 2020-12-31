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

with Ada.Calendar.Formatting;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with CHelper; use CHelper;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;

package body MainWindow is

   procedure CreateMainWindow(Directory: String) is
   begin
      null;
   end CreateMainWindow;

   procedure UpdateDirectoryList
     (Clear: Boolean := False; FrameName: String := "directory") is
   begin
      null;
   end UpdateDirectoryList;

end MainWindow;
