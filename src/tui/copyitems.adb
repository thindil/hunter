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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body CopyItems is

   -- ****iv* CopyItems/CopyItems.SourceDirectory
   -- FUNCTION
   -- Full path to the source directory of copied files and directories
   -- SOURCE
   SourceDirectory: Unbounded_String;
   -- ****

   procedure CopyItem
     (Name: String; Path: Unbounded_String; Success: in out Boolean) is
      NewPath: Unbounded_String := Path;
      procedure CopyFile(FileName: String) is
         NewName: Unbounded_String :=
           NewPath & To_Unbounded_String("/" & Simple_Name(FileName));
      begin
         if Exists(To_String(NewName)) then
            if Settings.Overwrite_On_Exist then
               Delete_File(To_String(NewName));
            else
               New_File_Name_Loop :
               loop
                  NewName :=
                    NewPath &
                    To_Unbounded_String
                      ("/" & Base_Name(To_String(NewName)) & "_." &
                       Extension(To_String(NewName)));
                  exit New_File_Name_Loop when not Exists(To_String(NewName));
               end loop New_File_Name_Loop;
            end if;
         end if;
         GNAT.OS_Lib.Copy_File
           (FileName, To_String(NewName), Success, Copy, Full);
      end CopyFile;
      procedure ProcessFile(Item: Directory_Entry_Type) is
      begin
         CopyFile(Full_Name(Item));
      end ProcessFile;
      procedure ProcessDirectory(Item: Directory_Entry_Type) is
      begin
         if Simple_Name(Item) /= "." and then Simple_Name(Item) /= ".." then
            CopyItem(Full_Name(Item), NewPath, Success);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end ProcessDirectory;
   begin
      if Is_Directory(Name) then
         Append(NewPath, "/" & Simple_Name(Name));
         if Exists(To_String(NewPath)) and not Settings.Overwrite_On_Exist then
            New_Directory_Name_Loop :
            loop
               NewPath := NewPath & "_";
               exit New_Directory_Name_Loop when not Exists
                   (To_String(NewPath));
            end loop New_Directory_Name_Loop;
         end if;
         Create_Path(To_String(NewPath));
         Search
           (Name, "", (Directory => False, others => True),
            ProcessFile'Access);
         Search
           (Name, "", (Directory => True, others => False),
            ProcessDirectory'Access);
      else
         CopyFile(Name);
      end if;
      UpdateProgressBar;
   end CopyItem;

   function CopySelected(Overwrite: in out Boolean) return UI_Locations is
      Path, ItemType: Unbounded_String;
      Success: Boolean := True;
   begin
      if DestinationDirectory = MainWindow.Current_Directory then
         Update_Directory_List(True);
         return DIRECTORY_VIEW;
      end if;
      Copy_Items_Loop :
      while CopyItemsList.Length > 0 loop
         Path := DestinationDirectory;
         if Exists
             (To_String(Path) & "/" &
              Simple_Name(To_String(CopyItemsList(1)))) and
           not Overwrite and Settings.Overwrite_On_Exist then
            ItemType :=
              (if
                 Is_Directory
                   (To_String(Path) & "/" &
                    Simple_Name(To_String(CopyItemsList(1))))
               then To_Unbounded_String(Mc(Interpreter, "{Directory}"))
               else To_Unbounded_String(Mc(Interpreter, "{File}")));
            ShowMessage
              (To_String(ItemType) & " " &
               Simple_Name(To_String(CopyItemsList(1))) & " " &
               Mc(Interpreter, "{exists. Do you want to overwrite it?}"),
               "question");
            return MESSAGE_FORM;
         end if;
         CopyItem(To_String(CopyItemsList(1)), Path, Success);
         exit Copy_Items_Loop when not Success;
         CopyItemsList.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
      end loop Copy_Items_Loop;
      CopyItemsList.Clear;
      if Settings.Show_Finished_Info then
         ShowMessage
           (Mc
              (Interpreter,
               "{All selected files and directories have been copied.}"),
            "message");
         return MESSAGE_FORM;
      end if;
      MainWindow.Current_Directory :=
        (if Settings.Stay_In_Old then SourceDirectory
         else DestinationDirectory);
      LoadDirectory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      return DIRECTORY_VIEW;
   end CopySelected;

   function SkipCopying return UI_Locations is
      OverwriteItem: Boolean := False;
   begin
      CopyItemsList.Delete(Index => 1);
      UpdateProgressBar;
      return CopySelected(OverwriteItem);
   end SkipCopying;

end CopyItems;
