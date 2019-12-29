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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Box; use Gtk.Box;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with CopyItems; use CopyItems;
with Messages; use Messages;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body MoveItems is

   -- ****iv* MoveItems/SourceDirectory
   -- FUNCTION
   -- Full path to the source directory of moved files and directories
   -- SOURCE
   SourceDirectory: Unbounded_String;
   -- ****

   procedure MoveData(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      OverwriteItem: Boolean := False;
   begin
      if Setting then
         return;
      end if;
      if MoveItemsList.Length > 0
        and then Containing_Directory(To_String(MoveItemsList(1))) =
          To_String(CurrentDirectory) then
         MoveItemsList.Clear;
         ToggleToolButtons(NewAction, True);
         CurrentSelected := Null_Unbounded_String;
         ShowItem(Get_Selection(DirectoryView));
         return;
      end if;
      if MoveItemsList.Length = 0 then
         MoveItemsList := SelectedItems;
         SourceDirectory := CurrentDirectory;
         ToggleToolButtons(MOVE);
         return;
      end if;
      if not Is_Write_Accessible_File(To_String(CurrentDirectory)) then
         ShowMessage
           (Gettext
              ("You don't have permissions to move selected items here."));
         return;
      end if;
      NewAction := MOVE;
      SetProgressBar(Positive(MoveItemsList.Length));
      MoveSelected(OverwriteItem);
   end MoveData;

   procedure MoveSelected(Overwrite: in out Boolean) is
      ItemType: Unbounded_String;
      Success: Boolean := True;
      NewName, FileExtension: Unbounded_String;
   begin
      while MoveItemsList.Length > 0 loop
         NewName :=
           DestinationPath & To_Unbounded_String("/") &
           Simple_Name(To_String(MoveItemsList(1)));
         if Exists(To_String(NewName)) then
            if not Overwrite and Settings.OverwriteOnExist then
               if Is_Directory(To_String(NewName)) then
                  ItemType := To_Unbounded_String(Gettext("Directory"));
               else
                  ItemType := To_Unbounded_String(Gettext("File"));
               end if;
               ShowMessage
                 (To_String(ItemType) & " " &
                  Simple_Name(To_String(MoveItemsList(1))) &
                  Gettext(" exists. Do you want to overwrite it?"),
                  Message_Question);
               return;
            end if;
            if not Settings.OverwriteOnExist then
               FileExtension :=
                 To_Unbounded_String(Extension(To_String(MoveItemsList(1))));
               loop
                  NewName :=
                    DestinationPath &
                    To_Unbounded_String
                      ("/" & Ada.Directories.Base_Name(To_String(NewName)) &
                       "_");
                  if Length(FileExtension) > 0 then
                     NewName :=
                       NewName & To_Unbounded_String(".") & FileExtension;
                  end if;
                  exit when not Exists(To_String(NewName));
               end loop;
            end if;
         end if;
         Rename_File(To_String(MoveItemsList(1)), To_String(NewName), Success);
         if not Success then
            CopyItem(To_String(MoveItemsList(1)), DestinationPath, Success);
            if Success then
               if Is_Directory(To_String(MoveItemsList(1))) then
                  Remove_Dir(To_String(MoveItemsList(1)), True);
               else
                  Delete_File(To_String(MoveItemsList(1)));
               end if;
            else
               ShowMessage
                 (Gettext("Can't move ") & To_String(MoveItemsList(1)) & ".");
               return;
            end if;
         end if;
         MoveItemsList.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
         UpdateProgressBar;
      end loop;
      MoveItemsList.Clear;
      Hide(Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 3));
      ToggleToolButtons(NewAction, True);
      if Settings.ShowFinishedInfo then
         ShowMessage
           (Gettext("All selected files and directories have been moved."),
            Message_Info);
      else
         CloseMessage(null);
      end if;
      if Settings.StayInOld then
         CurrentDirectory := SourceDirectory;
      end if;
      Reload(Builder);
   end MoveSelected;

   procedure SkipMoving is
      OverwriteItem: Boolean := False;
   begin
      MoveItemsList.Delete(Index => 1);
      UpdateProgressBar;
      MoveSelected(OverwriteItem);
   end SkipMoving;

end MoveItems;
