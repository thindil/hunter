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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Switch; use Gtk.Switch;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with MainWindow; use MainWindow;
with ShowItems; use ShowItems;

package body Preferences is

   procedure TogglePreferences(Object: access Gtkada_Builder_Record'Class) is
      Popup: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "poppreferences"));
   begin
      if Is_Visible(Popup) then
         Hide(Popup);
      else
         Show_All(Popup);
      end if;
   end TogglePreferences;

   procedure LoadSettings is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      function LoadBoolean return Boolean is
      begin
         if Value = To_Unbounded_String("Yes") then
            return True;
         end if;
         return False;
      end LoadBoolean;
   begin
      Settings :=
        (ShowHidden => True, ShowLastModified => False, ScaleImages => False,
         AutoCloseMessagesTime => 10, WindowWidth => 800, WindowHeight => 600,
         ShowPreview => True);
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") &
           "/.config/hunter/hunter.cfg") then
         return;
      end if;
      Open
        (ConfigFile, In_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      Setting := True;
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("ShowHidden") then
               Settings.ShowHidden := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchhidden")),
                  Settings.ShowHidden);
            elsif FieldName = To_Unbounded_String("ShowLastModified") then
               Settings.ShowLastModified := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchlastmodified")),
                  Settings.ShowLastModified);
            elsif FieldName = To_Unbounded_String("ScaleImages") then
               Settings.ScaleImages := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchscaleimages")),
                  Settings.ScaleImages);
            elsif FieldName = To_Unbounded_String("AutoCloseMessagesTime") then
               Settings.AutoCloseMessagesTime :=
                 Natural'Value(To_String(Value));
               Set_Value
                 (Gtk_Adjustment(Get_Object(Builder, "adjseconds")),
                  Gdouble(Settings.AutoCloseMessagesTime));
            elsif FieldName = To_Unbounded_String("WindowWidth") then
               Settings.WindowWidth := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowHeight") then
               Settings.WindowHeight := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ShowPreview") then
               Settings.ShowPreview := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchshowpreview")),
                  Settings.ShowPreview);
            end if;
         end if;
      end loop;
      Setting := False;
      Close(ConfigFile);
   end LoadSettings;

   function SaveSettings
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if Setting then
         return False;
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchhidden"))) /=
        Settings.ShowHidden then
         Settings.ShowHidden :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchhidden")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter1")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter2")));
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchlastmodified"))) /=
        Settings.ShowLastModified then
         Settings.ShowLastModified :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchlastmodified")));
         Set_Visible
           (Get_Column(Gtk_Tree_View(Get_Object(Builder, "treefiles")), 2),
            Settings.ShowLastModified);
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchscaleimages"))) /=
        Settings.ScaleImages then
         Settings.ScaleImages :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchscaleimages")));
         PreviewItem(Object);
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchshowpreview"))) /=
        Settings.ShowPreview then
         Settings.ShowPreview :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchshowpreview")));
         if NewAction /= COPY and NewAction /= MOVE and
           NewAction /= CREATELINK then
            Set_Visible
              (Gtk_Widget(Get_Object(Object, "boxsecond")),
               Settings.ShowPreview);
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnpreview")),
               Settings.ShowPreview);
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnfileinfo")),
               Settings.ShowPreview);
            if Settings.ShowPreview then
               Set_Position
                 (Gtk_Paned(Get_Object(Object, "filespaned")),
                  Gint
                    (Float
                       (Get_Allocated_Width
                          (Gtk_Widget(Get_Object(Object, "mainwindow")))) *
                     0.3));
               PreviewItem(Object);
            else
               Set_Position
                 (Gtk_Paned(Get_Object(Object, "filespaned")),
                  Get_Allocated_Width
                    (Gtk_Widget(Get_Object(Object, "mainwindow"))));
            end if;
         end if;
      end if;
      return False;
   end SaveSettings;

   procedure SaveSettingsProc(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjseconds")))) /=
        Settings.AutoCloseMessagesTime then
         Settings.AutoCloseMessagesTime :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjseconds"))));
      end if;
   end SaveSettingsProc;

   procedure SavePreferences is
      ConfigFile: File_Type;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         if Value then
            Put_Line(ConfigFile, Name & " = Yes");
         else
            Put_Line(ConfigFile, Name & " = No");
         end if;
      end SaveBoolean;
   begin
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") & "/.config/hunter") then
         Ada.Directories.Create_Path
           (Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      end if;
      Create
        (ConfigFile, Append_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      SaveBoolean(Settings.ShowHidden, "ShowHidden");
      SaveBoolean(Settings.ShowLastModified, "ShowLastModified");
      SaveBoolean(Settings.ScaleImages, "ScaleImages");
      Put_Line
        (ConfigFile,
         "AutoCloseMessagesTime =" &
         Natural'Image(Settings.AutoCloseMessagesTime));
      Put_Line
        (ConfigFile, "WindowWidth =" & Positive'Image(Settings.WindowWidth));
      Put_Line
        (ConfigFile, "WindowHeight =" & Positive'Image(Settings.WindowHeight));
      SaveBoolean(Settings.ShowPreview, "ShowPreview");
      Close(ConfigFile);
   end SavePreferences;

end Preferences;
