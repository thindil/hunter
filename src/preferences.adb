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
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Popover; use Gtk.Popover;
with Gtk.Scale; use Gtk.Scale;
with Gtk.Switch; use Gtk.Switch;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with MainWindow; use MainWindow;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body Preferences is

   Popup: Gtk_Popover;

   -- ****if* Preferences/TogglePreferences
   -- FUNCTION
   -- Show or hide the program preferences window
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure TogglePreferences(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      -- ****
   begin
      if Is_Visible(Gtk_Widget(Popup)) then
         Hide(Gtk_Widget(Popup));
      else
         Show_All(Gtk_Widget(Popup));
      end if;
   end TogglePreferences;

   procedure SetDeleteTooltip is
   begin
      if Settings.DeleteFiles then
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "btndelete")),
            Gettext("Delete selected file(s) or folder(s) [ALT-Delete]."));
      else

         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Builder, "btndelete")),
            Gettext
              ("Move selected file(s) or folder(s) to trash [ALT-Delete]."));
      end if;
   end SetDeleteTooltip;

   -- ****if* Preferences/SetToolbars
   -- FUNCTION
   -- Set position of toolbars - on the top or the left of the main window.
   -- SOURCE
   procedure SetToolbars is
      -- ****
      Header: constant GObject := Get_Object(Builder, "header");
      LeftBox: constant GObject := Get_Object(Builder, "leftbox");
      Toolbar: constant GObject := Get_Object(Builder, "toolbar");
      ItemToolbar: constant GObject := Get_Object(Builder, "itemtoolbar");
   begin
      if Settings.ToolbarsOnTop then
         if Get_Parent(Gtk_Widget(Toolbar)) = Gtk_Widget(Header) then
            return;
         end if;
         Remove(Gtk_Container(LeftBox), Gtk_Widget(Toolbar));
         Remove(Gtk_Container(LeftBox), Gtk_Widget(ItemToolbar));
         Pack_Start(Gtk_Header_Bar(Header), Gtk_Widget(Toolbar));
         Pack_End(Gtk_Header_Bar(Header), Gtk_Widget(ItemToolbar));
         Set_Orientation(Gtk_Toolbar(Toolbar), Orientation_Horizontal);
         Set_Orientation(Gtk_Toolbar(ItemToolbar), Orientation_Horizontal);
      else
         if Get_Parent(Gtk_Widget(Toolbar)) = Gtk_Widget(LeftBox) then
            return;
         end if;
         Remove(Gtk_Container(Header), Gtk_Widget(Toolbar));
         Remove(Gtk_Container(Header), Gtk_Widget(ItemToolbar));
         Pack_Start(Gtk_Box(LeftBox), Gtk_Widget(Toolbar));
         Pack_End(Gtk_Box(LeftBox), Gtk_Widget(ItemToolbar));
         Set_Orientation(Gtk_Toolbar(Toolbar), Orientation_Vertical);
         Set_Orientation(Gtk_Toolbar(ItemToolbar), Orientation_Vertical);
      end if;
   end SetToolbars;

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
      procedure LoadColorsList is
         Search: Search_Type;
         File: Directory_Entry_Type;
         ComboBox: constant Gtk_Combo_Box_Text :=
           Gtk_Combo_Box_Text(Get_Object(Builder, "cmbcolortheme"));
         ThemeName: Unbounded_String;
         Index: Gint := 0;
      begin
         Ada.Environment_Variables.Set
           ("HIGHLIGHT_DATADIR",
            Ada.Environment_Variables.Value("APPDIR", "") &
            "/usr/share/highlight");
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
            "/themes/base16",
            "*.theme");
         while More_Entries(Search) loop
            Get_Next_Entry(Search, File);
            ThemeName := To_Unbounded_String(Base_Name(Simple_Name(File)));
            Append_Text(ComboBox, To_String(ThemeName));
            if ThemeName = Settings.ColorTheme then
               Set_Active(ComboBox, Index);
            end if;
            Index := Index + 1;
         end loop;
         End_Search(Search);
      end LoadColorsList;
   begin
      Settings :=
        (ShowHidden => True, ShowLastModified => False, ScaleImages => False,
         AutoCloseMessagesTime => 10, WindowWidth => 800, WindowHeight => 600,
         ShowPreview => True, StayInOld => False, ColorText => True,
         ColorTheme => To_Unbounded_String("gruvbox-light-soft"),
         DeleteFiles => True, ClearTrashOnExit => False,
         ShowFinishedInfo => False, OverwriteOnExist => True,
         ToolbarsOnTop => True, AutoRefreshInterval => 10);
      if FindExecutable("highlight") = "" then
         Settings.ColorText := False;
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "switchcolortext")), False);
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "cmbcolortheme")), False);
         Set_Active
           (Gtk_Switch(Get_Object(Builder, "switchcolortext")),
            Settings.ColorText);
      end if;
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") &
           "/.config/hunter/hunter.cfg") then
         Setting := True;
         LoadColorsList;
         Setting := False;
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
            elsif FieldName = To_Unbounded_String("StayInOld") then
               Settings.StayInOld := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchstayinsource")),
                  Settings.StayInOld);
            elsif FieldName = To_Unbounded_String("ColorText") then
               Settings.ColorText := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchcolortext")),
                  Settings.ColorText);
            elsif FieldName = To_Unbounded_String("ColorTheme") then
               Settings.ColorTheme := Value;
            elsif FieldName = To_Unbounded_String("DeleteFiles") then
               Settings.DeleteFiles := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchdeletefiles")),
                  Settings.DeleteFiles);
               SetDeleteTooltip;
            elsif FieldName = To_Unbounded_String("ClearTrashOnExit") then
               Settings.ClearTrashOnExit := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchcleartrashonexit")),
                  Settings.ClearTrashOnExit);
            elsif FieldName = To_Unbounded_String("ShowFinishedInfo") then
               Settings.ShowFinishedInfo := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchshowfinishedinfo")),
                  Settings.ShowFinishedInfo);
            elsif FieldName = To_Unbounded_String("OverwriteOnExist") then
               Settings.OverwriteOnExist := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchoverwriteonexist")),
                  Settings.OverwriteOnExist);
            elsif FieldName = To_Unbounded_String("ToolbarsOnTop") then
               Settings.ToolbarsOnTop := LoadBoolean;
               Set_Active
                 (Gtk_Switch(Get_Object(Builder, "switchtoolbarsontop")),
                  Settings.ToolbarsOnTop);
               SetToolbars;
            elsif FieldName = To_Unbounded_String("AutoRefreshInterval") then
               Settings.AutoRefreshInterval :=
                 Positive'Value(To_String(Value));
               Set_Value
                 (Gtk_Adjustment(Get_Object(Builder, "adjrefresh")),
                  Gdouble(Settings.AutoRefreshInterval));
            end if;
         end if;
      end loop;
      LoadColorsList;
      Setting := False;
      Close(ConfigFile);
   end LoadSettings;

   -- ****if* Preferences/SaveSettings
   -- FUNCTION
   -- Save the program settings to file and update program to the new
   -- configuration if needed.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- RESULT
   -- Always False so default handler will be running too.
   -- SEE ALSO
   -- SaveSettingsProc
   -- SOURCE
   function SaveSettings
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   -- ****
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
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchstayinsource"))) /=
        Settings.StayInOld then
         Settings.StayInOld :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchstayinsource")));
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchcolortext"))) /=
        Settings.ColorText then
         Settings.ColorText :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchcolortext")));
         PreviewItem(Object);
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchdeletefiles"))) /=
        Settings.DeleteFiles then
         Settings.DeleteFiles :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchdeletefiles")));
         SetDeleteTooltip;
      end if;
      if Get_Active
          (Gtk_Switch(Get_Object(Object, "switchcleartrashonexit"))) /=
        Settings.ClearTrashOnExit then
         Settings.ClearTrashOnExit :=
           Get_Active
             (Gtk_Switch(Get_Object(Object, "switchcleartrashonexit")));
      end if;
      if Get_Active
          (Gtk_Switch(Get_Object(Object, "switchshowfinishedinfo"))) /=
        Settings.ShowFinishedInfo then
         Settings.ShowFinishedInfo :=
           Get_Active
             (Gtk_Switch(Get_Object(Object, "switchshowfinishedinfo")));
      end if;
      if Get_Active
          (Gtk_Switch(Get_Object(Object, "switchoverwriteonexist"))) /=
        Settings.OverwriteOnExist then
         Settings.OverwriteOnExist :=
           Get_Active
             (Gtk_Switch(Get_Object(Object, "switchoverwriteonexist")));
      end if;
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchtoolbarsontop"))) /=
        Settings.ToolbarsOnTop then
         Settings.ToolbarsOnTop :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchtoolbarsontop")));
         SetToolbars;
      end if;
      return False;
   end SaveSettings;

   -- ****if* Preferences/SaveSettingsProc
   -- FUNCTION
   -- Save the program settings to file and update program to the new
   -- configuration if needed. Some GTK elements need procedures instead
   -- of function.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SEE ALSO
   -- SaveSettings
   -- SOURCE
   procedure SaveSettingsProc(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      if Setting then
         return;
      end if;
      if Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjseconds")))) /=
        Settings.AutoCloseMessagesTime then
         Settings.AutoCloseMessagesTime :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjseconds"))));
      end if;
      if Get_Active_Text
          (Gtk_Combo_Box_Text(Get_Object(Object, "cmbcolortheme"))) /=
        To_String(Settings.ColorTheme) then
         Settings.ColorTheme :=
           To_Unbounded_String
             (Get_Active_Text
                (Gtk_Combo_Box_Text(Get_Object(Object, "cmbcolortheme"))));
         PreviewItem(Object);
      end if;
      if Natural
          (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjrefresh")))) /=
        Settings.AutoRefreshInterval then
         Settings.AutoRefreshInterval :=
           Natural
             (Get_Value(Gtk_Adjustment(Get_Object(Object, "adjrefresh"))));
         StartTimer;
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
         Create_Path
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
      SaveBoolean(Settings.StayInOld, "StayInOld");
      SaveBoolean(Settings.ColorText, "ColorText");
      Put_Line(ConfigFile, "ColorTheme = " & To_String(Settings.ColorTheme));
      SaveBoolean(Settings.DeleteFiles, "DeleteFiles");
      SaveBoolean(Settings.ClearTrashOnExit, "ClearTrashOnExit");
      SaveBoolean(Settings.ShowFinishedInfo, "ShowFinishedInfo");
      SaveBoolean(Settings.OverwriteOnExist, "OverwriteOnExist");
      SaveBoolean(Settings.ToolbarsOnTop, "ToolbarsOnTop");
      Put_Line
        (ConfigFile,
         "AutoRefreshInterval =" &
         Positive'Image(Settings.AutoRefreshInterval));
      Close(ConfigFile);
   end SavePreferences;

   procedure CreatePreferencesUI is
   begin
      Register_Handler
        (Builder, "Toggle_Preferences", TogglePreferences'Access);
      Register_Handler(Builder, "Save_Preferences", SaveSettings'Access);
      Register_Handler
        (Builder, "Save_Preferences_Proc", SaveSettingsProc'Access);
   end CreatePreferencesUI;

   procedure CreatePreferences(Parent: Gtk_Widget) is
      MenuBox: constant Gtk_Vbox := Gtk_Vbox_New;
      Label: Gtk_Label;
      Grid: Gtk_Grid;
      procedure AddFrame(Text: String) is
         Frame: constant Gtk_Frame := Gtk_Frame_New;
         FrameLabel: constant Gtk_Label := Gtk_Label_New;
      begin
         Set_Markup(FrameLabel, "<b>" & Text & "</b>");
         Set_Label_Widget(Frame, FrameLabel);
         Set_Label_Align(Frame, 0.5, 0.5);
         Set_Shadow_Type(Frame, Shadow_Out);
         Add(Frame, Grid);
         Pack_Start(MenuBox, Frame);
      end AddFrame;
      procedure NewGrid is
      begin
         Grid := Gtk_Grid_New;
         Set_Column_Homogeneous(Grid, True);
         Set_Column_Spacing(Grid, 10);
      end NewGrid;
      procedure NewLabel(Text: String; Row: Gint) is
      begin
         Label := Gtk_Label_New(Text);
         Set_Halign(Label, Align_Start);
         Attach(Grid, Label, 0, Row);
      end NewLabel;
      procedure NewSwitch(Active: Boolean; Row: Gint; Tooltip: String) is
         Switch: constant Gtk_Switch := Gtk_Switch_New;
      begin
         Set_Active(Switch, Active);
         Set_Tooltip_Text(Switch, Tooltip);
         Attach(Grid, Switch, 1, Row);
      end NewSwitch;
      procedure NewScale
        (Value: Integer; Min, Max, Step: Gdouble; Row: Gint;
         Tooltip: String) is
         Adjustment: constant Gtk_Adjustment :=
           Gtk_Adjustment_New(Gdouble(Value), Min, Max, Step, Step * 10.0);
         Scale: Gtk_Scale;
      begin
         Scale := Gtk_Hscale_New(Adjustment);
         Set_Digits(Scale, 0);
         Set_Tooltip_Text(Scale, Tooltip);
         Attach(Grid, Scale, 1, Row);
      end NewScale;
   begin
      LoadSettings;
      Popup := Gtk_Popover_New(Parent);
      NewGrid;
      NewLabel(Gettext("Show hidden files:"), 0);
      NewSwitch
        (Settings.ShowHidden, 0,
         Gettext
           ("Show hidden files and directories in directory listing and in directories preview."));
      NewLabel(Gettext("Show modification time:"), 1);
      NewSwitch
        (Settings.ShowLastModified, 1,
         Gettext
           ("Show the column with last modification date for files and directories."));
      NewLabel(Gettext("Auto refresh interval:"), 2);
      NewScale
        (Settings.AutoRefreshInterval, 0.0, 30.0, 1.0, 2,
         Gettext
           ("How often (in seconds) the program should check for changes in current directory. If set to zero, autorefresh will be disabled."));
      AddFrame(Gettext("Directory Listing"));
      NewGrid;
      NewLabel(Gettext("Show preview:"), 0);
      NewSwitch
        (Settings.ShowPreview, 0,
         Gettext
           ("Show second panel with preview of files and directories. If you disable this option, second panel will be visible only during copying and moving files or directories and during creating new link."));
      NewLabel(Gettext("Scale images:"), 1);
      NewSwitch
        (Settings.ScaleImages, 1,
         Gettext
           ("Scale images in preview. When disabled, images shows with natural size. When enabled, images are resized to the size of the preview window."));
      NewLabel(Gettext("Syntax highlightning:"), 2);
      NewSwitch
        (Settings.ColorText, 2,
         Gettext
           ("Color files syntax in files preview. Not all text (especially source code) files are supported. You may not be able to enable this option if you don't have installed the program ""highlight""."));
      NewLabel(Gettext("Syntax color theme:"), 3);
      declare
         Search: Search_Type;
         File: Directory_Entry_Type;
         ComboBox: constant Gtk_Combo_Box_Text := Gtk_Combo_Box_Text_New;
         ThemeName: Unbounded_String;
         Index: Gint := 0;
      begin
         Ada.Environment_Variables.Set
           ("HIGHLIGHT_DATADIR",
            Ada.Environment_Variables.Value("APPDIR", "") &
            "/usr/share/highlight");
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
            "/themes/base16",
            "*.theme");
         while More_Entries(Search) loop
            Get_Next_Entry(Search, File);
            ThemeName := To_Unbounded_String(Base_Name(Simple_Name(File)));
            Append_Text(ComboBox, To_String(ThemeName));
            if ThemeName = Settings.ColorTheme then
               Set_Active(ComboBox, Index);
            end if;
            Index := Index + 1;
         end loop;
         End_Search(Search);
         Set_Tooltip_Text
           (ComboBox,
            "Select color theme for coloring syntax in text files in preview. You may not be able to enable this option if you don't have installed the program ""highlight"".");
         Attach(Grid, ComboBox, 1, 3);
      end;
      AddFrame(Gettext("Preview"));
      Show_All(MenuBox);
      Add(Popup, MenuBox);
      Set_Modal(Popup, True);
      Set_Position(Popup, Pos_Bottom);
   end CreatePreferences;

end Preferences;
