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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with CopyItems; use CopyItems;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body MoveItems is

   -- ****iv* MoveItems/MoveItems.SourceDirectory
   -- FUNCTION
   -- Full path to the source directory of moved files and directories
   -- SOURCE
   SourceDirectory: Unbounded_String;
   -- ****

   function MoveSelected(Overwrite: in out Boolean) return UI_Locations is
      ItemType: Unbounded_String;
      Success: Boolean := True;
      NewName, FileExtension: Unbounded_String;
   begin
      Move_Items_Loop :
      while MoveItemsList.Length > 0 loop
         NewName :=
           DestinationDirectory & To_Unbounded_String("/") &
           Simple_Name(To_String(MoveItemsList(1)));
         if Exists(To_String(NewName)) then
            if not Overwrite and Settings.OverwriteOnExist then
               ItemType :=
                 (if Is_Directory(To_String(NewName)) then
                    To_Unbounded_String(Mc(Interpreter, "{Directory}"))
                  else To_Unbounded_String(Mc(Interpreter, "{File}")));
               ShowMessage
                 (To_String(ItemType) & " " &
                  Simple_Name(To_String(MoveItemsList(1))) & " " &
                  Mc(Interpreter, "{exists. Do you want to overwrite it?}"),
                  "question");
               return MESSAGE_FORM;
            end if;
            if not Settings.OverwriteOnExist then
               FileExtension :=
                 To_Unbounded_String(Extension(To_String(MoveItemsList(1))));
               New_File_Name_Loop :
               loop
                  NewName :=
                    DestinationDirectory &
                    To_Unbounded_String
                      ("/" & Ada.Directories.Base_Name(To_String(NewName)) &
                       "_");
                  if Length(FileExtension) > 0 then
                     NewName :=
                       NewName & To_Unbounded_String(".") & FileExtension;
                  end if;
                  exit New_File_Name_Loop when not Exists(To_String(NewName));
               end loop New_File_Name_Loop;
            end if;
         end if;
         Rename_File(To_String(MoveItemsList(1)), To_String(NewName), Success);
         if not Success then
            CopyItem
              (To_String(MoveItemsList(1)), DestinationDirectory, Success);
            if Success then
               if Is_Directory(To_String(MoveItemsList(1))) then
                  Remove_Dir(To_String(MoveItemsList(1)), True);
               else
                  Delete_File(To_String(MoveItemsList(1)));
               end if;
            else
               ShowMessage
                 (Mc(Interpreter, "{Can't move}") & " " &
                  To_String(MoveItemsList(1)) & ".");
               return MESSAGE_FORM;
            end if;
         end if;
         MoveItemsList.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
         UpdateProgressBar;
      end loop Move_Items_Loop;
      MoveItemsList.Clear;
      if Settings.ShowFinishedInfo then
         ShowMessage
           (Mc
              (Interpreter,
               "{All selected files and directories have been moved.}"),
            "message");
         return MESSAGE_FORM;
      end if;
      MainWindow.Current_Directory :=
        (if Settings.StayInOld then SourceDirectory else DestinationDirectory);
      Current_Selected :=
        MainWindow.Current_Directory & "/" &
        Simple_Name(To_String(Current_Selected));
      LoadDirectory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      return DIRECTORY_VIEW;
   end MoveSelected;

   function SkipMoving return UI_Locations is
      OverwriteItem: Boolean := False;
   begin
      MoveItemsList.Delete(Index => 1);
      UpdateProgressBar;
      return MoveSelected(OverwriteItem);
   end SkipMoving;

end MoveItems;
