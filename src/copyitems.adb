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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Label; use Gtk.Label;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with LoadData; use LoadData;
with Messages; use Messages;
with ShowItems; use ShowItems;

package body CopyItems is

   procedure CopyData(Object: access Gtkada_Builder_Record'Class) is
      OverwriteItem: Boolean := False;
   begin
      if CopyItemsList.Length > 0
        and then Containing_Directory(To_String(CopyItemsList(1))) =
          To_String(CurrentDirectory) then
         CopyItemsList.Clear;
         Show_All(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
         Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
         Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
         CurrentSelected := Null_Unbounded_String;
         ShowItem(Object);
         return;
      end if;
      if CopyItemsList.Length = 0 then
         CopyItemsList := SelectedItems;
         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
         Hide(Gtk_Widget(Get_Object(Object, "itemtoolbar")));
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Object, "btntoolcancel")),
            Gettext("Stop copying files and directories [Escape]"));
         Show_All(Gtk_Widget(Get_Object(Object, "btntoolcancel")));
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblframe")),
            Gettext("Destination directory"));
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Object, "infostack")), "destination");
         return;
      end if;
      if not Is_Write_Accessible_File(To_String(CurrentDirectory)) then
         ShowMessage
           (Gettext
              ("You don't have permissions to copy selected items here."));
         return;
      end if;
      NewAction := COPY;
      CopySelected(OverwriteItem);
   end CopyData;

   procedure CopyItem
     (Name: String; Path: Unbounded_String; Success: in out Boolean) is
      NewPath: Unbounded_String := Path;
      procedure ProcessFile(Item: Directory_Entry_Type) is
      begin
         if Exists(To_String(NewPath) & "/" & Simple_Name(Item)) then
            Delete_File(To_String(NewPath) & "/" & Simple_Name(Item));
         end if;
         GNAT.OS_Lib.Copy_File
           (Full_Name(Item), To_String(NewPath) & "/" & Simple_Name(Item),
            Success, Copy, Full);
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
         Create_Path(To_String(NewPath));
         Search
           (Name, "", (Directory => False, others => True),
            ProcessFile'Access);
         Search
           (Name, "", (Directory => True, others => False),
            ProcessDirectory'Access);
      else
         if Exists(To_String(Path) & "/" & Simple_Name(Name)) then
            Delete_File(To_String(Path) & "/" & Simple_Name(Name));
         end if;
         GNAT.OS_Lib.Copy_File
           (Name, To_String(Path) & "/" & Simple_Name(Name), Success, Copy,
            Full);
      end if;
   end CopyItem;

   procedure CopySelected(Overwrite: in out Boolean) is
      Path, ItemType: Unbounded_String;
      Success: Boolean := True;
   begin
      while CopyItemsList.Length > 0 loop
         Path := DestinationPath;
         if Exists
             (To_String(Path) & "/" &
              Simple_Name(To_String(CopyItemsList(1)))) and
           not Overwrite then
            if Is_Directory
                (To_String(Path) & "/" &
                 Simple_Name(To_String(CopyItemsList(1)))) then
               ItemType := To_Unbounded_String(Gettext("Directory"));
            else
               ItemType := To_Unbounded_String(Gettext("File"));
            end if;
            ShowMessage
              (To_String(ItemType) & " " &
               Simple_Name(To_String(CopyItemsList(1))) &
               Gettext(" exists. Do you want to overwrite it?"),
               Message_Question);
            return;
         end if;
         CopyItem(To_String(CopyItemsList(1)), Path, Success);
         exit when not Success;
         CopyItemsList.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
      end loop;
      CopyItemsList.Clear;
      HideMessage(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
      Reload(Builder);
   end CopySelected;

   procedure SkipCopying is
      OverwriteItem: Boolean := False;
   begin
      CopyItemsList.Delete(Index => 1);
      CopySelected(OverwriteItem);
   end SkipCopying;

end CopyItems;
