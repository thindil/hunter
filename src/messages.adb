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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Glib.Main; use Glib.Main;
with Gtkada.Intl; use Gtkada.Intl;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with DeleteItems; use DeleteItems;
with MainWindow; use MainWindow;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with Utils; use Utils;
with Trash; use Trash;

package body Messages is

   -- ****iv* Messages/Source_Id
   -- FUNCTION
   -- ID of timer to hide messages
   -- SOURCE
   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   -- ****if* Messages/AutoHideMessage
   -- FUNCTION
   -- Auto hide message after selected amount of seconds
   -- RESULT
   -- Returns always False to stop timer
   -- SOURCE
   function AutoHideMessage return Boolean is
   -- ****
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "actioninfo")));
      Source_Id := No_Source_Id;
      return False;
   end AutoHideMessage;

   procedure ShowMessage
     (Message: String; MessageType: Gtk_Message_Type := Message_Error) is
      InfoBar: constant GObject := Get_Object(Builder, "actioninfo");
   begin
      if MessageType /= Message_Question then
         Set_Show_Close_Button
           (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")), True);
         if Source_Id /= No_Source_Id then
            Remove(Source_Id);
            Source_Id := No_Source_Id;
         end if;
         if Settings.AutoCloseMessagesTime > 0 then
            Source_Id :=
              Timeout_Add
                (Guint(Settings.AutoCloseMessagesTime) * 1000,
                 AutoHideMessage'Access);
         end if;
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
      if NewAction = DELETE or NewAction = CLEARTRASH or
        NewAction = DELETETRASH then
         Hide(Gtk_Widget(Get_Object(Builder, "btnnoall")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnyesall")));
      end if;
   end ShowMessage;

   -- ****if* Messages/HideMessage
   -- FUNCTION
   -- Hide message
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure HideMessage(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      Hide(Gtk_Widget(Get_Object(Object, "actioninfo")));
      if Source_Id /= No_Source_Id then
         Remove(Source_Id);
         Source_Id := No_Source_Id;
      end if;
   end HideMessage;

   -- ****if* Messages/SetResponse
   -- FUNCTION
   -- Set proper GTK Response for info bar buttons
   -- PARAMETERS
   -- User_Data - Which button to set
   -- SOURCE
   procedure SetResponse(User_Data: access GObject_Record'Class) is
      -- ****
      ResponseValue: Gint;
   begin
      YesForAll := False;
      if User_Data = Get_Object(Builder, "btnyes") then
         ResponseValue := Gint(Gtk_Response_Yes);
      elsif User_Data = Get_Object(Builder, "btnno") then
         ResponseValue := Gint(Gtk_Response_No);
      elsif User_Data = Get_Object(Builder, "btnyesall") then
         YesForAll := True;
         ResponseValue := Gint(Gtk_Response_Accept);
      elsif User_Data = Get_Object(Builder, "btnnoall") then
         ResponseValue := Gint(Gtk_Response_Reject);
      end if;
      Response(Gtk_Info_Bar(Get_Object(Builder, "actioninfo")), ResponseValue);
   end SetResponse;

   -- ****if* Messages/MessageResponse
   -- FUNCTION
   -- Hide message or do action, depends on the user response
   -- PARAMETERS
   -- Self        - Gtk_Info_Bar which contains the message. Unused
   -- Response_Id - Gtk_Response depends on which button user clicked
   -- SOURCE
   procedure MessageResponse
     (Self: access Gtk_Info_Bar_Record'Class; Response_Id: Gint) is
      pragma Unreferenced(Self);
      -- ****
      OverwriteItem: Boolean := True;
   begin
      case NewAction is
         when DELETE | CLEARTRASH | DELETETRASH =>
            if NewAction /= CLEARTRASH then
               SetProgressBar(Positive(SelectedItems.Length));
            end if;
            if Response_Id = Gint(Gtk_Response_Yes) then
               begin
                  if DeleteSelected then
                     CurrentDirectory :=
                       To_Unbounded_String
                         (Normalize_Pathname
                            (To_String(CurrentDirectory) & "/.."));
                  end if;
               exception
                  when others =>
                     Reload(Builder);
                     return;
               end;
               if NewAction = CLEARTRASH then
                  GoHome(Builder);
               elsif NewAction = DELETETRASH then
                  ShowTrash(Builder);
                  NewAction := DELETETRASH;
               else
                  Reload(Builder);
               end if;
            end if;
            ToggleToolButtons(NewAction, True);
            Hide(Gtk_Widget(Get_Object(Builder, "progressbar")));
            if Settings.ShowFinishedInfo then
               if NewAction = DELETE and not Settings.DeleteFiles then
                  ShowMessage
                    (Gettext
                       ("All selected files and directories have been moved to Trash."),
                     Message_Info);
               elsif NewAction = CLEARTRASH then
                  ShowMessage
                    (Gettext("Trash have been cleared."), Message_Info);
               else
                  ShowMessage
                    (Gettext
                       ("All selected files and directories have been deleted."),
                     Message_Info);
               end if;
            else
               HideMessage(Builder);
            end if;
         when COPY =>
            if Response_Id = Gint(Gtk_Response_Reject) then
               HideMessage(Builder);
               ToggleToolButtons(NewAction, True);
               Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
               Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
               Reload(Builder);
               return;
            elsif Response_Id = Gint(Gtk_Response_No) then
               SkipCopying;
               return;
            end if;
            CopySelected(OverwriteItem);
         when MOVE =>
            if Response_Id = Gint(Gtk_Response_Reject) then
               HideMessage(Builder);
               ToggleToolButtons(NewAction, True);
               Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
               Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
               Reload(Builder);
               return;
            elsif Response_Id = Gint(Gtk_Response_No) then
               SkipMoving;
               return;
            end if;
            MoveSelected(OverwriteItem);
         when others =>
            null;
      end case;
      if Response_Id = Gint(Gtk_Response_Close) then
         HideMessage(Builder);
      end if;
   end MessageResponse;

   procedure CreateMessagesUI is
   begin
      Register_Handler(Builder, "Hide_Message", HideMessage'Access);
      Register_Handler(Builder, "Set_Response", SetResponse'Access);
      On_Response
        (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")),
         MessageResponse'Access);
   end CreateMessagesUI;

end Messages;
