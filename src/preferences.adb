-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Preferences.Commands; use Preferences.Commands;
with Utils; use Utils;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkNotebook; use Tcl.Tk.Ada.Widgets.TtkNotebook;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;

package body Preferences is

   procedure LoadSettings is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      KeyIndex: Positive := 1;
      function LoadBoolean return Boolean is
      begin
         if Value = To_Unbounded_String("Yes") then
            return True;
         end if;
         return False;
      end LoadBoolean;
   begin
      SetDefaultSettings;
      SetDefaultAccelerators;
      Open
        (ConfigFile, In_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("ShowHidden") then
               Settings.ShowHidden := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowLastModified") then
               Settings.ShowLastModified := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ScaleImages") then
               Settings.ScaleImages := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoCloseMessagesTime") then
               Settings.AutoCloseMessagesTime :=
                 Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowWidth") then
               Settings.WindowWidth := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowHeight") then
               Settings.WindowHeight := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ShowPreview") then
               Settings.ShowPreview := LoadBoolean;
            elsif FieldName = To_Unbounded_String("StayInOld") then
               Settings.StayInOld := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ColorText") then
               Settings.ColorText := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ColorTheme") then
               Settings.ColorTheme := Value;
            elsif FieldName = To_Unbounded_String("DeleteFiles") then
               Settings.DeleteFiles := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ClearTrashOnExit") then
               Settings.ClearTrashOnExit := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowFinishedInfo") then
               Settings.ShowFinishedInfo := LoadBoolean;
            elsif FieldName = To_Unbounded_String("OverwriteOnExist") then
               Settings.OverwriteOnExist := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ToolbarsOnTop") then
               Settings.ToolbarsOnTop := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoRefreshInterval") then
               Settings.AutoRefreshInterval :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("UITheme") then
               Settings.UITheme := Value;
            elsif FieldName = To_Unbounded_String("ToolbarsSize") then
               Settings.ToolbarsSize := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("MonospaceFont") then
               Settings.MonospaceFont := LoadBoolean;
            end if;
         end if;
      end loop;
      if FindExecutable("highlight") = "" then
         Settings.ColorText := False;
      end if;
      Close(ConfigFile);
      Open
        (ConfigFile, In_File,
         Ada.Environment_Variables.Value("HOME") & "/.config/hunter/keys.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            Accelerators(KeyIndex) := RawData;
            KeyIndex := KeyIndex + 1;
         end if;
      end loop;
      Close(ConfigFile);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end LoadSettings;

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
      Put_Line(ConfigFile, "UITheme = " & To_String(Settings.UITheme));
      Put_Line
        (ConfigFile, "ToolbarsSize =" & Positive'Image(Settings.ToolbarsSize));
      SaveBoolean(Settings.MonospaceFont, "MonospaceFont");
      Close(ConfigFile);
      Create
        (ConfigFile, Append_File,
         Ada.Environment_Variables.Value("HOME") & "/.config/hunter/keys.cfg");
      for Accel of Accelerators loop
         Put_Line(ConfigFile, To_String(Accel));
      end loop;
      Close(ConfigFile);
   end SavePreferences;

   procedure CreatePreferencesUI is
      LabelFrame: Ttk_LabelFrame;
      CheckButton: Ttk_CheckButton;
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      MainFrame: constant Ttk_Frame := Create(".preferencesframe");
      Notebook: constant Ttk_Notebook :=
        Create(Widget_Image(MainFrame) & ".notebook");
      PreferencesFrame: constant Ttk_Frame :=
        Create(Widget_Image(Notebook) & ".preferences");
      ColorsEnabled: constant Boolean :=
        (if FindExecutable("highlight")'Length > 0 then True else False);
      ShortcutsFrame: constant Ttk_Frame :=
        Create(Widget_Image(Notebook) & ".shortcuts");
      procedure AddButton
        (Name, Text: String; Value: Boolean; TooltipText, Command: String) is
         CheckButton: constant Ttk_CheckButton :=
           Create
             (Widget_Image(LabelFrame) & Name,
              "-text {" & Text & "} -command " & Command);
      begin
         if Value then
            Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
         else
            Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
         end if;
         Add(CheckButton, TooltipText);
         Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      end AddButton;
   begin
      Tcl.Tk.Ada.Pack.Pack(Notebook, "-fill both -expand true");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesFrame) & ".directory",
           "-text {" & Mc(Get_Context, "{Directory Listing}") & "}");
      AddButton
        (".showhidden", Mc(Get_Context, "{Show hidden files}"),
         Settings.ShowHidden,
         Mc
           (Get_Context,
            "{Show hidden files and directories in directory\nlisting and in directories preview.}"),
         "SetShowHiddenFiles");
      AddButton
        (".showmodificationtime", Mc(Get_Context, "{Show modification time}"),
         Settings.ShowLastModified,
         Mc
           (Get_Context,
            "{Show the column with last modification\ndate for files and directories.}"),
         "SetShowModificationTime");
      Tcl_SetVar
        (Get_Context, "updateinterval",
         Natural'Image(Settings.AutoRefreshInterval));
      Label :=
        Create
          (Widget_Image(LabelFrame) & ".intervallabel",
           "-text """ & Mc(Get_Context, "{Auto refresh every }") &
           "$updateinterval" & Mc(Get_Context, "{ seconds}") & """");
      Add
        (Label,
         Mc
           (Get_Context,
            "{How often (in seconds) the program should check\nfor changes in current directory.\nIf set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Widget_Image(LabelFrame) & ".intervalscale",
           "-from 0 -to 30 -variable updateinterval -orient horizontal -command {SetLabel directory.interval}");
      Add
        (Scale,
         Mc
           (Get_Context,
            "{How often (in seconds) the program should check\nfor changes in current directory.\nIf set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesFrame) & ".preview",
           "-text {" & Mc(Get_Context, "{Preview}") & "}");
      AddButton
        (".showpreview", Mc(Get_Context, "{Show preview}"),
         Settings.ShowPreview,
         Mc
           (Get_Context,
            "{Show second panel with preview of files and directories.\nIf you disable this option, second panel will be visible only during\ncopying and moving files or directories and during creating new link.}"),
         "SetShowPreview");
      AddButton
        (".scaleimages", Mc(Get_Context, "{Scale images}"),
         Settings.ScaleImages,
         Mc
           (Get_Context,
            "{Scale images in preview. When disabled, images shows with\nnatural size. When enabled, images are resized to the size of the\npreview window.}"),
         "SetScaleImages");
      CheckButton :=
        Create
          (Widget_Image(LabelFrame) & ".syntaxhighlightning",
           "-text {" & Mc(Get_Context, "{Syntax highlightning}") &
           "} -command {SetColorText}");
      if Settings.ColorText then
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
      else
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
      end if;
      if ColorsEnabled then
         State(CheckButton, "!disabled");
      else
         State(CheckButton, "disabled");
      end if;
      Add
        (CheckButton,
         Mc
           (Get_Context,
            "{Color files syntax in files preview. Not all text (especially source code)\nfiles are supported. You may not be able to enable this\noption if you don't have installed the program 'highlight'.}"));
      Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      declare
         Search: Search_Type;
         File: Directory_Entry_Type;
         ThemesName: Unbounded_String;
         ComboBox: Ttk_ComboBox;
         ColorFrame: constant Ttk_Frame :=
           Create(Widget_Image(LabelFrame) & ".colorframe");
      begin
         if not Ada.Environment_Variables.Exists("HIGHLIGHT_DATADIR") then
            Ada.Environment_Variables.Set
              ("HIGHLIGHT_DATADIR",
               Ada.Environment_Variables.Value("APPDIR", "") &
               "/usr/share/highlight");
         end if;
         if Exists
             (Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
              "/themes/base16") then
            Start_Search
              (Search,
               Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
               "/themes/base16",
               "*.theme");
            while More_Entries(Search) loop
               Get_Next_Entry(Search, File);
               Append(ThemesName, " " & Base_Name(Simple_Name(File)));
            end loop;
            End_Search(Search);
         end if;
         ComboBox :=
           Create
             (Widget_Image(ColorFrame) & ".highlighttheme",
              "-state readonly -values [list" & To_String(ThemesName) & "]");
         if ColorsEnabled then
            State(ComboBox, "!disabled");
         else
            State(ComboBox, "disabled");
         end if;
         Set(ComboBox, "{" & To_String(Settings.ColorTheme) & "}");
         Bind(ComboBox, "<<ComboboxSelected>>", "SetColorTheme");
         Add
           (ComboBox,
            Mc
              (Get_Context,
               "{Select color theme for coloring syntax in text files in preview. You may\nnot be able to enable this option if you don't have installed\nthe program 'highlight'.}"));
         Label :=
           Create
             (Widget_Image(ColorFrame) & ".themelabel",
              "-text {" & Mc(Get_Context, "{Color theme:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Select color theme for coloring syntax in text files in preview. You may\nnot be able to enable this option if you don't have installed\nthe program 'highlight'.}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ComboBox, "-column 1 -row 0");
         Tcl.Tk.Ada.Pack.Pack(ColorFrame, "-fill x");
      end;
      AddButton
        (".monospacefont", Mc(Get_Context, "{Use monospace font}"),
         Settings.MonospaceFont,
         Mc(Get_Context, "{Use monospace font in the preview of text files.}"),
         "SetMonospaceFont");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesFrame) & ".interface",
           "-text {" & Mc(Get_Context, "{Interface}") & "}");
      Tcl_SetVar
        (CheckButton.Interp, "messagesinterval",
         Natural'Image(Settings.AutoCloseMessagesTime));
      Label :=
        Create
          (Widget_Image(LabelFrame) & ".messageslabel",
           "-text """ & Mc(Get_Context, "{Hide messages after }") &
           "$messagesinterval" & Mc(Get_Context, "{ seconds}") & """");
      Add
        (Label,
         Mc
           (Get_Context,
            "{After that amount of seconds, all messages will be automatically closed by the\nprogram. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Widget_Image(LabelFrame) & ".messagesscale",
           "-from 0 -to 60 -variable messagesinterval -orient horizontal -command {SetLabel interface.messages}");
      Add
        (Scale,
         Mc
           (Get_Context,
            "{After that amount of seconds, all messages will be automatically closed by the\nprogram. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      AddButton
        (".stayinold", Mc(Get_Context, "{Stay in source directory}"),
         Settings.StayInOld,
         Mc
           (Get_Context,
            "{After copying, moving files and directories or creating new link, stay in old\ndirectory, don't automatically go to destination directory.}"),
         "SetStayInOld");
      AddButton
        (".showfinished", Mc(Get_Context, "{Show info about finished action}"),
         Settings.ShowFinishedInfo,
         Mc
           (Get_Context,
            "{Show information about finished copying, moving and\ndeleting files or directories.}"),
         "SetShowFinishedInfo");
      AddButton
        (".toolbarsontop", Mc(Get_Context, "{Toolbars on top}"),
         Settings.ToolbarsOnTop,
         Mc
           (Get_Context,
            "{If enabled, show toolbars for actions and information on top of the window.\nOtherwise, they will be at left side of the window.}"),
         "SetToolbarsOnTop");
      declare
         ThemeFrame: constant Ttk_Frame :=
           Create(Widget_Image(LabelFrame) & ".colorframe");
         ThemesNames: Unbounded_String := To_Unbounded_String(Theme_Names);
         ColorBox: constant Ttk_ComboBox :=
           Create(Widget_Image(ThemeFrame) & ".uitheme", "-state readonly");
      begin
         if Index(ThemesNames, "hunter-dark", 1) = 0 then
            Append(ThemesNames, " hunter-dark");
         end if;
         if Index(ThemesNames, "hunter-light", 1) = 0 then
            Append(ThemesNames, " hunter-light");
         end if;
         Widgets.configure
           (ColorBox, "-values [list " & To_String(ThemesNames) & "]");
         Bind(ColorBox, "<<ComboboxSelected>>", "SetUITheme");
         Add
           (ColorBox,
            Mc
              (Get_Context,
               "{Select other theme for the program. After selecting a new theme,\nyou will have to restart the program to apply all changes.}"));
         Label :=
           Create
             (Widget_Image(ThemeFrame) & ".themelabel",
              "-text {" & Mc(Get_Context, "{UI theme:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Select other theme for the program. After selecting a new theme,\nyou will have to restart the program to apply all changes.}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ColorBox, "-column 1 -row 0");
         Set(ColorBox, To_String(Settings.UITheme));
         Tcl.Tk.Ada.Pack.Pack(ThemeFrame, "-fill x");
      end;
      declare
         ToolbarFrame: constant Ttk_Frame :=
           Create(Widget_Image(LabelFrame) & ".toolbarframe");
         ToolbarBox: constant Ttk_ComboBox :=
           Create
             (Widget_Image(ToolbarFrame) & ".toolbarsize",
              "-state readonly -values [list {" & Mc(Get_Context, "{small}") &
              "} {" & Mc(Get_Context, "{medium}") & "} {" &
              Mc(Get_Context, "{large}") & "} {" & Mc(Get_Context, "{huge}") &
              "}]");
      begin
         Bind(ToolbarBox, "<<ComboboxSelected>>", "SetToolbarsSize");
         Add
           (ToolbarBox,
            Mc(Get_Context, "{Set the size of icons in toolbars}"));
         Label :=
           Create
             (Widget_Image(ToolbarFrame) & ".toolbarlabel",
              "-text {" & Mc(Get_Context, "{Toolbars size:}") & "}");
         Add(Label, Mc(Get_Context, "{Set the size of icons in toolbars}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ToolbarBox, "-column 1 -row 0");
         if Settings.ToolbarsSize < 24 then
            Set(ToolbarBox, Mc(Get_Context, "{small}"));
         elsif Settings.ToolbarsSize < 32 then
            Set(ToolbarBox, Mc(Get_Context, "{medium}"));
         elsif Settings.ToolbarsSize < 64 then
            Set(ToolbarBox, Mc(Get_Context, "{large}"));
         else
            Set(ToolbarBox, Mc(Get_Context, "{huge}"));
         end if;
         Tcl.Tk.Ada.Pack.Pack(ToolbarFrame, "-fill x");
      end;
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesFrame) & ".deleting",
           "-text {" & Mc(Get_Context, "{Deleting}") & "}");
      AddButton
        (".deletefiles", Mc(Get_Context, "{Delete files}"),
         Settings.DeleteFiles,
         Mc
           (Get_Context,
            "{Delete selected files and directories instead of moving them to Trash.}"),
         "SetDeleteFiles");
      AddButton
        (".cleartrash", Mc(Get_Context, "{Clear Trash on exit}"),
         Settings.ClearTrashOnExit,
         Mc
           (Get_Context,
            "{Automatically clear Trash on exit from the program.}"),
         "SetClearTrash");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesFrame) & ".copying",
           "-text {" & Mc(Get_Context, "{Copying or moving}") & "}");
      AddButton
        (".overwrite", Mc(Get_Context, "{Overwrite existing}"),
         Settings.OverwriteOnExist,
         Mc
           (Get_Context,
            "{If enabled, during copying or moving files and directories,\nif in destination directory exists file or directory with that\nsame name, the program will ask if overwrite it. If disabled, the\nprogram will quietly give add underscore to the name of moved or\ncopied file or directory.}"),
         "SetOverwrite");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      declare
         ButtonsFrame: constant Ttk_Frame :=
           Create(Widget_Image(PreferencesFrame) & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " &
              Widget_Image(PreferencesFrame) & "}");
         RestoreButton: constant Ttk_Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".restorebutton",
              "-text {" & Mc(Get_Context, "{Restore defaults}") &
              "} -command RestoreDefaults");
      begin
         Add
           (RestoreButton,
            Mc
              (Get_Context,
               "{Restore default settings for the program. You will have to restart\nthe program to apply all changes}"));
         Tcl.Tk.Ada.Pack.Pack(RestoreButton, "-side left");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
         Tcl.Tk.Ada.Pack.Pack(ButtonsFrame, "-fill x");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(PreferencesFrame),
         "-text {" & Mc(Get_Context, "{Preferences}") & "}");
      declare
         ButtonsFrame: constant Ttk_Frame :=
           Create(Widget_Image(ShortcutsFrame) & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " &
              Widget_Image(PreferencesFrame) & "}");
         RestoreButton: constant Ttk_Button :=
           Create
             (Widget_Image(ButtonsFrame) & ".restorebutton",
              "-text {" & Mc(Get_Context, "{Restore defaults}") &
              "} -command RestoreDefaultsShortcuts");
         KeysLabels: constant array(1 .. 17) of Unbounded_String :=
           (To_Unbounded_String(Mc(Get_Context, "{Quit from the program}")),
            To_Unbounded_String(Mc(Get_Context, "{Show bookmarks menu}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Search for the file or directory}")),
            To_Unbounded_String(Mc(Get_Context, "{Show add new item menu}")),
            To_Unbounded_String(Mc(Get_Context, "{Show delete menu}")),
            To_Unbounded_String
              (Mc
                 (Get_Context,
                  "{Show menu with information about the program}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Open selected file or directory}")),
            To_Unbounded_String
              (Mc
                 (Get_Context,
                  "{Select or unselect all files and directories}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Rename selected file or directory}")),
            To_Unbounded_String(Mc(Get_Context, "{Copy selected files}")),
            To_Unbounded_String(Mc(Get_Context, "{Move selected files}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Show the program preferences}")),
            To_Unbounded_String
              (Mc
                 (Get_Context,
                  "{Open selected file or directory with command}")),
            To_Unbounded_String
              (Mc(Get_Context, "{File or directory information}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Preview file or directory}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Add bookmark to this directory}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Remove bookmark from this directory}")));
         Label: Ttk_Label;
         Button: Ttk_Button;
         Image: Tk_Photo;
      begin
         Image :=
           Create
             ("refreshicon",
              "-file {../share/hunter/images/view-refresh.svg} -format ""svg -scaletoheight [expr {[font metrics DefaultFont -linespace]}]""");
         for I in KeysLabels'Range loop
            Label :=
              Create
                (Widget_Image(ShortcutsFrame) & ".label" &
                 Trim(Positive'Image(I), Left),
                 "-text {" & To_String(KeysLabels(I)) & ": }");
            Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
            Label :=
              Create
                (Widget_Image(ShortcutsFrame) & ".labelshortcut" &
                 Trim(Positive'Image(I), Left),
                 "-text {" & To_String(Accelerators(I)) & "}");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-sticky w -column 1 -row" & Natural'Image(I - 1));
            Button :=
              Create
                (Widget_Image(ShortcutsFrame) & ".button" &
                 Trim(Positive'Image(I), Left),
                 "-style Toolbutton -image " & Widget_Image(Image));
            Tcl.Tk.Ada.Grid.Grid
              (Button, "-sticky w -column 2 -row" & Natural'Image(I - 1));
         end loop;
         Add
           (RestoreButton,
            Mc
              (Get_Context,
               "{Restore default keyboard shortcuts for the program.}"));
         Tcl.Tk.Ada.Pack.Pack(RestoreButton, "-side left");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
         Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky we -columnspan 3");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(ShortcutsFrame),
         "-text {" & Mc(Get_Context, "{Keyboard shortcuts}") & "}");
      AddCommands;
   end CreatePreferencesUI;

   procedure SetDefaultSettings is
   begin
      Settings :=
        (ShowHidden => True, ShowLastModified => False, ScaleImages => False,
         AutoCloseMessagesTime => 10, WindowWidth => 800, WindowHeight => 600,
         ShowPreview => True, StayInOld => False, ColorText => True,
         ColorTheme => To_Unbounded_String("gruvbox-light-soft"),
         DeleteFiles => True, ClearTrashOnExit => False,
         ShowFinishedInfo => False, OverwriteOnExist => True,
         ToolbarsOnTop => True, AutoRefreshInterval => 10,
         UITheme => To_Unbounded_String("hunter-light"), ToolbarsSize => 24,
         MonospaceFont => False);
   end SetDefaultSettings;

   procedure SetDefaultAccelerators is
   begin
      Accelerators :=
        (To_Unbounded_String("Control-q"), To_Unbounded_String("Alt-h"),
         To_Unbounded_String("Alt-f"), To_Unbounded_String("Alt-n"),
         To_Unbounded_String("Control-Delete"), To_Unbounded_String("Alt-a"),
         To_Unbounded_String("Alt-o"), To_Unbounded_String("Control-a"),
         To_Unbounded_String("Control-r"), To_Unbounded_String("Alt-c"),
         To_Unbounded_String("Alt-m"), To_Unbounded_String("Alt-p"),
         To_Unbounded_String("Alt-w"), To_Unbounded_String("Alt-i"),
         To_Unbounded_String("Alt-v"), To_Unbounded_String("Alt-b"),
         To_Unbounded_String("Alt-r"));
   end SetDefaultAccelerators;

end Preferences;
