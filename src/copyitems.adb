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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with MainWindow; use MainWindow;
with Messages; use Messages;

package body CopyItems is

-- ****iv* CopyItems/CopyItemsList
-- SOURCE
   CopyItemsList: UnboundedString_Container.Vector;
-- ****

-- ****if* CopyItems/CopyData
-- SOURCE
   procedure CopyData(Object: access Gtkada_Builder_Record'Class) is
-- ****
      Path: Unbounded_String;
      Success: Boolean := True;
      procedure CopyItem(Name: String) is
         procedure ProcessFile(Item: Directory_Entry_Type) is
         begin
            GNAT.OS_Lib.Copy_File
              (Full_Name(Item), To_String(Path) & "/" & Simple_Name(Item),
               Success, Copy, Full);
         end ProcessFile;
         procedure ProcessDirectory(Item: Directory_Entry_Type) is
         begin
            if Simple_Name(Item) /= "." and then Simple_Name(Item) /= ".." then
               CopyItem(Full_Name(Item));
            end if;
         exception
            when Ada.Directories.Name_Error =>
               null;
         end ProcessDirectory;
      begin
         if Is_Directory(Name) then
            Append(Path, "/" & Simple_Name(Name));
            Create_Path(To_String(Path));
            Search
              (Name, "", (Directory => False, others => True),
               ProcessFile'Access);
            Search
              (Name, "", (Directory => True, others => False),
               ProcessDirectory'Access);
         else
            GNAT.OS_Lib.Copy_File
              (Name, To_String(Path) & "/" & Simple_Name(Name), Success, Copy,
               Full);
         end if;
      end CopyItem;
   begin
      if CopyItemsList.Length > 0
        and then Containing_Directory(To_String(CopyItemsList(1))) =
          To_String(CurrentDirectory) then
         return;
      end if;
      if CopyItemsList.Length = 0 then
         CopyItemsList := SelectedItems;
         return;
      end if;
      if not Is_Write_Accessible_File(To_String(CurrentDirectory)) then
         ShowMessage
           ("You don't have permissions to copy selected items here.");
         return;
      end if;
      for Name of CopyItemsList loop
         Path := CurrentDirectory;
         CopyItem(To_String(Name));
         if not Success then
            exit;
         end if;
      end loop;
      CopyItemsList.Clear;
      Reload(Object);
   end CopyData;

end CopyItems;
