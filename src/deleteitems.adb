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
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtkada.Intl; use Gtkada.Intl;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;

package body DeleteItems is

   function DeleteSelected return Boolean is
      GoUp, Success: Boolean := False;
      Arguments: Argument_List := (new String'("-rf"), new String'(""));
      OldSetting: Boolean;
      procedure MoveToTrash(Name: Unbounded_String) is
         NewName: Unbounded_String;
         TrashFile: File_Type;
      begin
         NewName :=
           Trim
             (To_Unbounded_String
                (Hash_Type'Image
                   (Ada.Strings.Unbounded.Hash
                      (Name & To_Unbounded_String(Image(Clock))))),
              Both);
         Create
           (TrashFile, Out_File,
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/info/" & To_String(NewName) & ".trashinfo");
         Put_Line(TrashFile, "[Trash Info]");
         Put_Line(TrashFile, "Path=" & To_String(Name));
         Put_Line
           (TrashFile,
            "DeletionDate=" &
            Image(Date => Clock, Time_Zone => UTC_Time_Offset));
         Close(TrashFile);
         Rename_File
           (To_String(Name),
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/files/" & To_String(NewName),
            Success);
      end MoveToTrash;
      procedure AddTrash(SubDirectory: String) is
         Search: Search_Type;
         Item: Directory_Entry_Type;
      begin
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/" &
            SubDirectory,
            "*");
         while More_Entries(Search) loop
            Get_Next_Entry(Search, Item);
            if Simple_Name(Item) /= "." and Simple_Name(Item) /= ".." then
               SelectedItems.Append
                 (New_Item => To_Unbounded_String(Full_Name(Item)));
            end if;
         end loop;
         End_Search(Search);
      end AddTrash;
   begin
      if NewAction = CLEARTRASH then
         OldSetting := Settings.DeleteFiles;
         Settings.DeleteFiles := True;
         SelectedItems.Clear;
         AddTrash("info");
         AddTrash("files");
      end if;
      for Item of SelectedItems loop
         if Is_Directory(To_String(Item)) then
            Arguments(2) := new String'(To_String(Item));
            if Settings.DeleteFiles then
               Spawn(Locate_Exec_On_Path("rm").all, Arguments, Success);
               if not Success then
                  raise Directory_Error with To_String(Item);
               end if;
            else
               MoveToTrash(Item);
            end if;
            if Item = CurrentDirectory then
               GoUp := True;
            end if;
         else
            if Settings.DeleteFiles then
               Delete_File(To_String(Item));
            else
               MoveToTrash(Item);
            end if;
         end if;
      end loop;
      if NewAction = CLEARTRASH then
         Settings.DeleteFiles := OldSetting;
      end if;
      return GoUp;
   exception
      when An_Exception : Ada.Directories.Use_Error =>
         ShowMessage
           (Gettext
              ("Could not delete selected files or directories. Reason: ") &
            Exception_Message(An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         ShowMessage
           (Gettext("Can't delete selected directory: ") &
            Exception_Message(An_Exception));
         raise;
      when others =>
         ShowMessage
           (Gettext("Unknown error during deleting files or directories."));
         raise;
   end DeleteSelected;

   procedure DeleteItem(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      Message: Unbounded_String;
   begin
      if Settings.DeleteFiles then
         Message := To_Unbounded_String(Gettext("Delete?") & LF);
      else
         Message := To_Unbounded_String(Gettext("Move to trash?") & LF);
      end if;
      for I in SelectedItems.First_Index .. SelectedItems.Last_Index loop
         Append(Message, SelectedItems(I));
         if Is_Directory(To_String(SelectedItems(I))) then
            Append(Message, Gettext("(and its content)"));
         end if;
         if I /= SelectedItems.Last_Index then
            Append(Message, LF);
         end if;
      end loop;
      NewAction := DELETE;
      ToggleToolButtons(NewAction);
      ShowMessage(To_String(Message), Message_Question);
   end DeleteItem;

end DeleteItems;
