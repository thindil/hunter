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

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with MainWindow; use MainWindow;
with Messages; use Messages;

package body DeleteItems is

   function DeleteSelected return Boolean is
      GoUp: Boolean := False;
   begin
      for Item of SelectedItems loop
         if Is_Directory(To_String(Item)) then
            Remove_Dir(To_String(Item), True);
            if Item = CurrentDirectory then
               GoUp := True;
            end if;
         else
            Delete_File(To_String(Item));
         end if;
      end loop;
      return GoUp;
   exception
      when An_Exception : USE_ERROR =>
         ShowMessage
           ("Could not delete selected files or directories. Reason: " &
            Exception_Message(An_Exception));
         return GoUp;
      when Directory_Error =>
         ShowMessage("Can't delete selected files or directories.");
         return GoUp;
   end DeleteSelected;

end DeleteItems;
