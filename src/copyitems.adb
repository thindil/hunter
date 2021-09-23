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

with Ada.Directories;
with GNAT.OS_Lib;
with Preferences;
with Utils.UI;

package body CopyItems is

   procedure Copy_Item
     (Name: String; Path: Unbounded_String; Success: in out Boolean) is
      use Ada.Directories;
      use GNAT.OS_Lib;
      use Preferences;
      use Utils.UI;

      New_Path: Unbounded_String := Path;
      procedure Copy_File(File_Name: String) is
         New_Name: Unbounded_String :=
           New_Path &
           To_Unbounded_String(Source => "/" & Simple_Name(Name => File_Name));
      begin
         if Exists(Name => To_String(Source => New_Name)) then
            if Settings.Overwrite_On_Exist then
               Delete_File(Name => To_String(Source => New_Name));
            else
               New_File_Name_Loop :
               loop
                  New_Name :=
                    New_Path &
                    To_Unbounded_String
                      (Source =>
                         "/" &
                         Base_Name(Name => To_String(Source => New_Name)) &
                         "_." &
                         Extension(Name => To_String(Source => New_Name)));
                  exit New_File_Name_Loop when not Exists
                      (Name => To_String(Source => New_Name));
               end loop New_File_Name_Loop;
            end if;
         end if;
         GNAT.OS_Lib.Copy_File
           (Name => File_Name, Pathname => To_String(Source => New_Name),
            Success => Success, Mode => Copy, Preserve => Full);
      end Copy_File;
      procedure Process_File(Item: Directory_Entry_Type) is
      begin
         Copy_File(File_Name => Full_Name(Directory_Entry => Item));
      end Process_File;
      procedure Process_Directory(Item: Directory_Entry_Type) is
      begin
         if Simple_Name(Directory_Entry => Item) not in "." | ".." then
            Copy_Item
              (Name => Full_Name(Directory_Entry => Item), Path => New_Path,
               Success => Success);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end Process_Directory;
   begin
      if Is_Directory(Name => Name) then
         Append
           (Source => New_Path, New_Item => "/" & Simple_Name(Name => Name));
         if Exists(Name => To_String(Source => New_Path)) and
           not Settings.Overwrite_On_Exist then
            New_Directory_Name_Loop :
            loop
               New_Path := New_Path & "_";
               exit New_Directory_Name_Loop when not Exists
                   (Name => To_String(Source => New_Path));
            end loop New_Directory_Name_Loop;
         end if;
         Create_Path(New_Directory => To_String(Source => New_Path));
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => False, others => True),
            Process => Process_File'Access);
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => True, others => False),
            Process => Process_Directory'Access);
      else
         Copy_File(File_Name => Name);
      end if;
      Update_Progress_Bar;
   end Copy_Item;

end CopyItems;
