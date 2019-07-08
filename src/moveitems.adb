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
with Gtk.Label; use Gtk.Label;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with CopyItems; use CopyItems;
with LoadData; use LoadData;
with Messages; use Messages;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body MoveItems is

   procedure MoveData(Object: access Gtkada_Builder_Record'Class) is
      OverwriteItem: Boolean := False;
   begin
      if MoveItemsList.Length > 0
        and then Containing_Directory(To_String(MoveItemsList(1))) =
          To_String(CurrentDirectory) then
         MoveItemsList.Clear;
         ToggleToolButtons(NewAction, True);
         Show_All(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
         Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
         Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
         CurrentSelected := Null_Unbounded_String;
         ShowItem(Object);
         return;
      end if;
      if MoveItemsList.Length = 0 then
         MoveItemsList := SelectedItems;
         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
         Hide(Gtk_Widget(Get_Object(Object, "itemtoolbar")));
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Object, "btntoolcancel")),
            Gettext("Stop moving files and directories [Escape]"));
         Show_All(Gtk_Widget(Get_Object(Object, "btntoolcancel")));
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblframe")),
            Gettext("Destination directory"));
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Object, "infostack")), "destination");
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
      MoveSelected(OverwriteItem);
   end MoveData;

   procedure MoveSelected(Overwrite: in out Boolean) is
      ItemType: Unbounded_String;
      Success: Boolean := True;
   begin
      while MoveItemsList.Length > 0 loop
         if Exists
             (To_String(DestinationPath) & "/" &
              Simple_Name(To_String(MoveItemsList(1)))) and
           not Overwrite then
            if Is_Directory
                (To_String(DestinationPath) & "/" &
                 Simple_Name(To_String(MoveItemsList(1)))) then
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
         Rename_File
           (To_String(MoveItemsList(1)),
            To_String(DestinationPath) & "/" &
            Simple_Name(To_String(MoveItemsList(1))),
            Success);
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
      end loop;
      MoveItemsList.Clear;
      ToggleToolButtons(NewAction, True);
      HideMessage(Builder);
      Show_All(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
      Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
      Reload(Builder);
   end MoveSelected;

   procedure SkipMoving is
      OverwriteItem: Boolean := False;
   begin
      MoveItemsList.Delete(Index => 1);
      MoveSelected(OverwriteItem);
   end SkipMoving;

end MoveItems;
