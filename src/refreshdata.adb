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

with Ada.Calendar; use Ada.Calendar;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with MainWindow; use MainWindow;
with ada.text_io;

package body RefreshData is

   task body RefreshTask is
      LastRun: Time := Clock;
      Directory: Search_Type;
      Item: Directory_Entry_Type;
   begin
      accept Start;
      loop
         Start_Search(Directory, To_String(CurrentDirectory), "");
         while More_Entries(Directory) loop
            Get_Next_Entry(Directory, Item);
            Ada.Text_IO.Put_Line(Simple_Name(Item));
         end loop;
         End_Search(Directory);
         LastRun := Clock;
         delay 10.0;
      end loop;
   end ;

end RefreshData;
