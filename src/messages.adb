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

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Glib.Object; use Glib.Object;
with MainWindow; use MainWindow;

package body Messages is

   procedure ShowMessage(Message: String;
      MessageType: Gtk_Message_Type := Message_Error) is
      InfoBar: constant GObject := Get_Object(Builder, "actioninfo");
   begin
      if MessageType /= Message_Question then
         Set_Show_Close_Button
           (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")), True);
      else
         Set_Show_Close_Button
           (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")), False);
      end if;
      Set_Message_Type(Gtk_Info_Bar(InfoBar), MessageType);
      Set_Text(Gtk_Label(Get_Object(Builder, "lblactioninfo")), Message);
      Show_All(Gtk_Widget(InfoBar));
      if MessageType /= Message_Question then
         Hide(Gtk_Widget(Get_Object(Builder, "actionbox")));
      end if;
   end ShowMessage;

   procedure HideMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "actioninfo")));
   end HideMessage;

   procedure MessageYes(Object: access Gtkada_Builder_Record'Class) is
   begin
      Response
        (Gtk_Info_Bar(Get_Object(Object, "actioninfo")),
         Gint(GTK_RESPONSE_YES));
   end MessageYes;

   procedure MessageNo(Object: access Gtkada_Builder_Record'Class) is
   begin
      Response
        (Gtk_Info_Bar(Get_Object(Object, "actioninfo")),
         Gint(GTK_RESPONSE_NO));
   end MessageNo;

   procedure MessageResponse(Self: access Gtk_Info_Bar_Record'Class;
      Response_Id: Gint) is
      pragma Unreferenced(Self);
      GoUp: Boolean := False;
   begin
      if Response_Id /= Gint(GTK_RESPONSE_YES) then
         HideMessage(Builder);
         return;
      end if;
      if NewAction = DELETE then
         for Item of SelectedItems loop
            if Is_Directory(To_String(Item)) then
               Remove_Dir(To_String(Item), True);
               if Item = CurrentDirectory then
                  GoUp := True;
               end if;
            else
               Delete_File(To_String(Item));
            end if;
         end loop;
      end if;
      HideMessage(Builder);
      if GoUp then
         CurrentDirectory :=
           To_Unbounded_String
             (Normalize_Pathname(To_String(CurrentDirectory) & "/.."));
      end if;
      Reload(Builder);
   exception
      when An_Exception : USE_ERROR =>
         if NewAction = DELETE then
            ShowMessage
              ("Could not delete selected files or directories. Reason: " &
               Exception_Message(An_Exception));
         end if;
      when Directory_Error =>
         ShowMessage("Can't delete selected files or directories.");
   end MessageResponse;

end Messages;
