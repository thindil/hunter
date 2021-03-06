-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body LoadData.UI is

   procedure LoadDirectory(DirectoryName: String; Second: Boolean := False) is
      Directory: Dir_Type;
      FileName: String(1 .. 1_024);
      Last: Natural range 0 .. FileName'Last;
   begin
      if not Second then
         ItemsList.Clear;
      else
         SecondItemsList.Clear;
      end if;
      if not Is_Read_Accessible_File(DirectoryName) then
         return;
      end if;
      Open(Directory, DirectoryName);
      Read_Directory_Loop :
      loop
         Read(Directory, FileName, Last);
         exit Read_Directory_Loop when Last = 0;
         if FileName(1 .. Last) /= "." and FileName(1 .. Last) /= ".." then
            if not Second then
               AddItem(DirectoryName & "/" & FileName(1 .. Last), ItemsList);
            else
               AddItem
                 (DirectoryName & "/" & FileName(1 .. Last), SecondItemsList);
            end if;
         end if;
      end loop Read_Directory_Loop;
      Close(Directory);
      if not Second then
         Items_Sorting.Sort(ItemsList);
      else
         Items_Sorting.Sort(SecondItemsList);
      end if;
   end LoadDirectory;

end LoadData.UI;
