-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
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
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Modules; use Modules;
with Modules.Commands;
with Preferences.Commands;
with UserCommands; use UserCommands;
with UserCommands.UI; use UserCommands.UI;
with Utils; use Utils;

package body Preferences.UI is

   procedure Create_Preferences_Ui is
      Label_Frame: Ttk_LabelFrame;
      Check_Button: Ttk_CheckButton;
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      Main_Frame: constant Ttk_Frame :=
        Create(pathName => ".preferencesframe");
      Scroll_X: constant Ttk_Scrollbar :=
        Create
          (pathName => Main_Frame & ".scrollx",
           options =>
             "-orient horizontal -command [list " & Main_Frame &
             ".canvas xview]");
      Scroll_Y: constant Ttk_Scrollbar :=
        Create
          (pathName => Main_Frame & ".scrolly",
           options =>
             "-orient vertical -command [list " & Main_Frame &
             ".canvas yview]");
      Preferences_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Main_Frame & ".canvas",
           options =>
             "-xscrollcommand {" & Scroll_X & " set} -yscrollcommand {" &
             Scroll_Y & " set}");
      Notebook: constant Ttk_Notebook :=
        Create(pathName => Preferences_Canvas & ".notebook");
      Preferences_Frame: constant Ttk_Frame :=
        Create(pathName => Notebook & ".preferences");
      Colors_Enabled: constant Boolean :=
        (if
           Find_Executable(Name => "highlight", Display_Message => False)'
             Length >
           0
         then True
         else False);
      Shortcuts_Frame: constant Ttk_Frame :=
        Create(pathName => Notebook & ".shortcuts");
      Actions_Frame: constant Ttk_Frame :=
        Create(pathName => Notebook & ".actions");
      Modules_Frame: constant Ttk_Frame :=
        Create(pathName => Notebook & ".modules");
      procedure Add_Button
        (Name, Text: String; Value: Boolean; Tooltip_Text, Command: String) is
         New_Check_Button: constant Ttk_CheckButton :=
           Create
             (pathName => Label_Frame & Name,
              options => "-text {" & Text & "} -command " & Command);
      begin
         if Value then
            Tcl_SetVar
              (interp => New_Check_Button.Interp,
               varName => Widget_Image(Win => New_Check_Button),
               newValue => "1");
         else
            Tcl_SetVar
              (interp => New_Check_Button.Interp,
               varName => Widget_Image(Win => New_Check_Button),
               newValue => "0");
         end if;
         Add(Widget => New_Check_Button, Message => Tooltip_Text);
         Tcl.Tk.Ada.Pack.Pack(Slave => New_Check_Button, Options => "-fill x");
      end Add_Button;
   begin
      Autoscroll(Scroll => Scroll_X);
      Autoscroll(Scroll => Scroll_Y);
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Scroll_X, Options => "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Scroll_Y, Options => "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Preferences_Canvas,
         Options => "-side top -fill both -expand true");
      Label_Frame :=
        Create
          (pathName => Preferences_Frame & ".directory",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Directory Listing}") &
             "}");
      Add_Button
        (Name => ".showhidden",
         Text =>
           Mc(Interp => Get_Context, Src_String => "{Show hidden files}"),
         Value => Settings.Show_Hidden,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Show hidden files and directories in directory}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{listing and in directories preview.}"),
         Command => "SetShowHiddenFiles");
      Add_Button
        (Name => ".showmodificationtime",
         Text =>
           Mc(Interp => Get_Context, Src_String => "{Show modification time}"),
         Value => Settings.Show_Last_Modified,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show the column with last modification}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{date for files and directories.}"),
         Command => "SetShowModificationTime");
      Tcl_SetVar
        (interp => Get_Context, varName => "updateinterval",
         newValue => Natural'Image(Settings.Auto_Refresh_Interval));
      Label :=
        Create
          (pathName => Label_Frame & ".intervallabel",
           options =>
             "-text """ &
             Mc(Interp => Get_Context, Src_String => "{Auto refresh every}") &
             "$updateinterval " &
             Mc(Interp => Get_Context, Src_String => "{seconds}") & """");
      Add
        (Label,
         Mc(Get_Context, "{How often (in seconds) the program should check}") &
         LF & Mc(Get_Context, "{for changes in current directory.}") & LF &
         Mc(Get_Context, "{If set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Label_Frame & ".intervalscale",
           "-from 0 -to 30 -variable updateinterval -orient horizontal -command {SetLabel directory.interval}");
      Add
        (Scale,
         Mc(Get_Context, "{How often (in seconds) the program should check}") &
         LF & Mc(Get_Context, "{for changes in current directory.}") & LF &
         Mc(Get_Context, "{If set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
      Label_Frame :=
        Create
          (Preferences_Frame & ".preview",
           "-text {" & Mc(Get_Context, "{Preview}") & "}");
      Add_Button
        (".showpreview", Mc(Get_Context, "{Show preview}"),
         Settings.Show_Preview,
         Mc
           (Get_Context,
            "{Show second panel with preview of files and directories.}") &
         LF &
         Mc(Get_Context,
            "{If you disable this option, second panel will be visible only during}") &
         LF &
         Mc(Get_Context,
            "{copying and moving files or directories and during creating new link.}"),
         "SetShowPreview");
      Add_Button
        (".scaleimages", Mc(Get_Context, "{Scale images}"),
         Settings.Scale_Images,
         Mc
           (Get_Context,
            "{Scale images in preview. When disabled, images shows with}") &
         LF &
         Mc(Get_Context,
            "{natural size. When enabled, images are resized to the size of the}") &
         LF & Mc(Get_Context, "{preview window.}"),
         "SetScaleImages");
      Check_Button :=
        Create
          (Label_Frame & ".syntaxhighlightning",
           "-text {" & Mc(Get_Context, "{Syntax highlightning}") &
           "} -command {SetColorText}");
      if Settings.Color_Text then
         Tcl_SetVar(Check_Button.Interp, Widget_Image(Check_Button), "1");
      else
         Tcl_SetVar(Check_Button.Interp, Widget_Image(Check_Button), "0");
      end if;
      if Colors_Enabled then
         State(Check_Button, "!disabled");
      else
         State(Check_Button, "disabled");
      end if;
      Add
        (Check_Button,
         Mc
           (Get_Context,
            "{Color files syntax in files preview. Not all text (especially source code)}") &
         LF &
         Mc(Get_Context,
            "{files are supported. You may not be able to enable this}") &
         LF &
         Mc(Get_Context,
            "{option if you don't have installed the program 'highlight'.}"));
      Tcl.Tk.Ada.Pack.Pack(Check_Button, "-fill x");
      declare
         Search: Search_Type;
         File: Directory_Entry_Type;
         ThemesName: Unbounded_String;
         ComboBox: Ttk_ComboBox;
         ColorFrame: constant Ttk_Frame := Create(Label_Frame & ".colorframe");
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
            Create_Themes_List_Loop :
            while More_Entries(Search) loop
               Get_Next_Entry(Search, File);
               Append(ThemesName, " " & Base_Name(Simple_Name(File)));
            end loop Create_Themes_List_Loop;
            End_Search(Search);
         end if;
         ComboBox :=
           Create
             (ColorFrame & ".highlighttheme",
              "-state readonly -values [list" & To_String(ThemesName) & "]");
         if Colors_Enabled then
            State(ComboBox, "!disabled");
         else
            State(ComboBox, "disabled");
         end if;
         Set(ComboBox, "{" & To_String(Settings.Color_Theme) & "}");
         Bind(ComboBox, "<<ComboboxSelected>>", "SetColorTheme");
         Add
           (ComboBox,
            Mc
              (Get_Context,
               "{Select color theme for coloring syntax in text files in preview. You may}") &
            LF &
            Mc(Get_Context,
               "{not be able to enable this option if you don't have installed}") &
            LF & Mc(Get_Context, "{the program 'highlight'.}"));
         Label :=
           Create
             (ColorFrame & ".themelabel",
              "-text {" & Mc(Get_Context, "{Color theme:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Select color theme for coloring syntax in text files in preview. You may}") &
            LF &
            Mc(Get_Context,
               "{not be able to enable this option if you don't have installed}") &
            LF & Mc(Get_Context, "{the program 'highlight'.}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ComboBox, "-column 1 -row 0");
         Tcl.Tk.Ada.Pack.Pack(ColorFrame, "-fill x");
      end;
      Add_Button
        (".monospacefont", Mc(Get_Context, "{Use monospace font}"),
         Settings.Monospace_Font,
         Mc(Get_Context, "{Use monospace font in the preview of text files.}"),
         "SetMonospaceFont");
      Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
      Label_Frame :=
        Create
          (Preferences_Frame & ".interface",
           "-text {" & Mc(Get_Context, "{Interface}") & "}");
      Tcl_SetVar
        (Check_Button.Interp, "messagesinterval",
         Natural'Image(Settings.Auto_Close_Messages_Time));
      Label :=
        Create
          (Label_Frame & ".messageslabel",
           "-text """ & Mc(Get_Context, "{Hide messages after}") &
           "$messagesinterval " & Mc(Get_Context, "{seconds}") & """");
      Add
        (Label,
         Mc
           (Get_Context,
            "{After that amount of seconds, all messages will be automatically closed by the}") &
         LF &
         Mc(Get_Context,
            "{program. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Label_Frame & ".messagesscale",
           "-from 0 -to 60 -variable messagesinterval -orient horizontal -command {SetLabel interface.messages}");
      Add
        (Scale,
         Mc
           (Get_Context,
            "{After that amount of seconds, all messages will be automatically closed by the}") &
         LF &
         Mc(Get_Context,
            "{program. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      Add_Button
        (".stayinold", Mc(Get_Context, "{Stay in source directory}"),
         Settings.Stay_In_Old,
         Mc
           (Get_Context,
            "{After copying, moving files and directories or creating new link, stay in old}") &
         LF &
         Mc(Get_Context,
            "{directory, don't automatically go to destination directory.}"),
         "SetStayInOld");
      Add_Button
        (".showfinished", Mc(Get_Context, "{Show info about finished action}"),
         Settings.Show_Finished_Info,
         Mc
           (Get_Context,
            "{Show information about finished copying, moving and}") &
         LF & Mc(Get_Context, "{deleting files or directories.}"),
         "SetShowFinishedInfo");
      Add_Button
        (".toolbarsontop", Mc(Get_Context, "{Toolbars on top}"),
         Settings.Toolbars_On_Top,
         Mc
           (Get_Context,
            "{If enabled, show toolbars for actions and information on top of the window.}") &
         LF &
         Mc(Get_Context,
            "{Otherwise, they will be at left side of the window.}"),
         "SetToolbarsOnTop");
      declare
         ThemeFrame: constant Ttk_Frame := Create(Label_Frame & ".colorframe");
         ThemesNames: Unbounded_String := To_Unbounded_String(Theme_Names);
         ColorBox: constant Ttk_ComboBox :=
           Create(ThemeFrame & ".uitheme", "-state readonly");
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
               "{Select other theme for the program. After selecting a new theme,}") &
            LF &
            Mc(Get_Context,
               "{you will have to restart the program to apply all changes.}"));
         Label :=
           Create
             (ThemeFrame & ".themelabel",
              "-text {" & Mc(Get_Context, "{UI theme:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Select other theme for the program. After selecting a new theme,}") &
            LF &
            Mc(Get_Context,
               "{you will have to restart the program to apply all changes.}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ColorBox, "-column 1 -row 0");
         Set(ColorBox, To_String(Settings.Ui_Theme));
         Tcl.Tk.Ada.Pack.Pack(ThemeFrame, "-fill x");
      end;
      declare
         ToolbarFrame: constant Ttk_Frame :=
           Create(Label_Frame & ".toolbarframe");
         ToolbarBox: constant Ttk_ComboBox :=
           Create
             (ToolbarFrame & ".toolbarsize",
              "-state readonly -values [list {" & Mc(Get_Context, "{small}") &
              "} {" & Mc(Get_Context, "{medium}") & "} {" &
              Mc(Get_Context, "{large}") & "} {" & Mc(Get_Context, "{huge}") &
              "}]");
      begin
         Bind(ToolbarBox, "<<ComboboxSelected>>", "SetToolbars_Size");
         Add
           (ToolbarBox,
            Mc(Get_Context, "{Set the size of icons in toolbars}"));
         Label :=
           Create
             (ToolbarFrame & ".toolbarlabel",
              "-text {" & Mc(Get_Context, "{Toolbars size:}") & "}");
         Add(Label, Mc(Get_Context, "{Set the size of icons in toolbars}"));
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ToolbarBox, "-column 1 -row 0");
         if Settings.Toolbars_Size < 24 then
            Set(ToolbarBox, Mc(Get_Context, "{small}"));
         elsif Settings.Toolbars_Size < 32 then
            Set(ToolbarBox, Mc(Get_Context, "{medium}"));
         elsif Settings.Toolbars_Size < 64 then
            Set(ToolbarBox, Mc(Get_Context, "{large}"));
         else
            Set(ToolbarBox, Mc(Get_Context, "{huge}"));
         end if;
         Tcl.Tk.Ada.Pack.Pack(ToolbarFrame, "-fill x");
      end;
      Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
      Label_Frame :=
        Create
          (Preferences_Frame & ".deleting",
           "-text {" & Mc(Get_Context, "{Deleting}") & "}");
      Add_Button
        (".deletefiles", Mc(Get_Context, "{Delete files}"),
         Settings.Delete_Files,
         Mc
           (Get_Context,
            "{Delete selected files and directories instead of moving them to Trash.}"),
         "SetDeleteFiles");
      Add_Button
        (".cleartrash", Mc(Get_Context, "{Clear Trash on exit}"),
         Settings.Clear_Trash_On_Exit,
         Mc
           (Get_Context,
            "{Automatically clear Trash on exit from the program.}"),
         "SetClearTrash");
      Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
      Label_Frame :=
        Create
          (Preferences_Frame & ".copying",
           "-text {" & Mc(Get_Context, "{Copying or moving}") & "}");
      Add_Button
        (".overwrite", Mc(Get_Context, "{Overwrite existing}"),
         Settings.Overwrite_On_Exist,
         Mc
           (Get_Context,
            "{If enabled, during copying or moving files and directories,}") &
         LF &
         Mc(Get_Context,
            "{if in destination directory exists file or directory with that}") &
         LF &
         Mc(Get_Context,
            "{same name, the program will ask if overwrite it. If disabled, the}") &
         LF &
         Mc(Get_Context,
            "{program will quietly give add underscore to the name of moved or}") &
         LF & Mc(Get_Context, "{copied file or directory.}"),
         "SetOverwrite");
      Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
      declare
         ButtonsFrame: constant Ttk_Frame :=
           Create(Preferences_Frame & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & Preferences_Frame & "}");
         RestoreButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".restorebutton",
              "-text {" & Mc(Get_Context, "{Restore defaults}") &
              "} -command RestoreDefaults");
      begin
         Add
           (RestoreButton,
            Mc
              (Get_Context,
               "{Restore default settings for the program. You will have to restart}") &
            LF & Mc(Get_Context, "{the program to apply all changes}"));
         Tcl.Tk.Ada.Pack.Pack(RestoreButton, "-side left");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
         Tcl.Tk.Ada.Pack.Pack(ButtonsFrame, "-fill x");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(Preferences_Frame),
         "-text {" & Mc(Get_Context, "{Preferences}") & "}");
      -- Keyboard shortcuts settings
      declare
         ButtonsFrame: constant Ttk_Frame :=
           Create(Shortcuts_Frame & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & Preferences_Frame & "}");
         RestoreButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".restorebutton",
              "-text {" & Mc(Get_Context, "{Restore defaults}") &
              "} -command RestoreDefaultShortcuts");
         KeysLabels: constant array(Accelerators'Range) of Unbounded_String :=
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
              (Mc(Get_Context, "{Remove bookmark from this directory}")),
            To_Unbounded_String(Mc(Get_Context, "{Execute selected program}")),
            To_Unbounded_String
              (Mc
                 (Get_Context,
                  "{Restore deleted file or directory from Trash}")),
            To_Unbounded_String
              (Mc(Get_Context, "{Show the user defined actions}")));
         Label: Ttk_Label;
         Button: Ttk_Button;
         Image: constant Tk_Photo :=
           Create
             ("refreshicon",
              "-file {../share/hunter/images/document-edit.svg} -format ""svg -scaletoheight [expr {[font metrics DefaultFont -linespace]}]""");
      begin
         Create_Shortcuts_UI_Loop :
         for I in KeysLabels'Range loop
            Label :=
              Create
                (Shortcuts_Frame & ".label" & Trim(Positive'Image(I), Left),
                 "-text {" & To_String(KeysLabels(I)) & ": }");
            Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
            Label :=
              Create
                (Shortcuts_Frame & ".labelshortcut" &
                 Trim(Positive'Image(I), Left),
                 "-text {" & To_String(Accelerators(I)) & "} -wraplength 150");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-sticky w -column 1 -row" & Natural'Image(I - 1));
            Button :=
              Create
                (Shortcuts_Frame & ".button" & Trim(Positive'Image(I), Left),
                 "-style Toolbutton -image " & Widget_Image(Image) &
                 " -command {StartChangingShortcut" & Positive'Image(I) & "}");
            Add
              (Button,
               Mc(Get_Context, "{Change keyboard shortcut for}") & ":" & LF &
               To_String(KeysLabels(I)));
            Tcl.Tk.Ada.Grid.Grid
              (Button, "-sticky w -column 2 -row" & Natural'Image(I - 1));
         end loop Create_Shortcuts_UI_Loop;
         Add
           (RestoreButton,
            Mc
              (Get_Context,
               "{Restore default keyboard shortcuts for the program.}"));
         Tcl.Tk.Ada.Pack.Pack(RestoreButton, "-side left");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
         Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky we -columnspan 3");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Shortcuts_Frame, ButtonsFrame, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (Shortcuts_Frame, ButtonsFrame, "-weight 1");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(Shortcuts_Frame),
         "-text {" & Mc(Get_Context, "{Keyboard shortcuts}") & "}");
      -- Actions settings
      declare
         CloseButton: constant Ttk_Button :=
           Create
             (Actions_Frame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & Preferences_Frame & "}");
         Button: Ttk_Button;
         Label: Ttk_Label;
         Tentry: Ttk_Entry;
      begin
         Label_Frame :=
           Create
             (Actions_Frame & ".addframe",
              "-text {" & Mc(Get_Context, "{Add a new command}") & "}");
         Label :=
           Create
             (Label_Frame & ".titlelbl",
              "-text {" & Mc(Get_Context, "{Menu label:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
         Tentry := Create(Label_Frame & ".title");
         Add
           (Tentry,
            Mc
              (Get_Context,
               "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Tentry, "-row 0 -column 1");
         Label :=
           Create
             (Label_Frame & ".commandlbl",
              "-text {" & Mc(Get_Context, "{Command to execute:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Command to execute. That command must be a program not a shell command.}") &
            LF &
            Mc(Get_Context, "{@1 will be replaced by current directory}") &
            LF &
            Mc(Get_Context,
               "{@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
         Tentry := Create(Label_Frame & ".command");
         Add
           (Tentry,
            Mc
              (Get_Context,
               "{Command to execute. That command must be a program not a shell command.}") &
            LF &
            Mc(Get_Context, "{@1 will be replaced by current directory}") &
            LF &
            Mc(Get_Context,
               "{@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Tentry, "-row 1 -column 1");
         Check_Button :=
           Create
             (Label_Frame & ".output",
              "-text {" & Mc(Get_Context, "{Show command output in preview}") &
              "}");
         Add
           (Check_Button,
            Mc
              (Get_Context,
               "{If checked, the command output will be shown in preview window.}") &
            LF &
            Mc(Get_Context,
               "{Otherwise, the command output will be ignored.}"));
         Tcl.Tk.Ada.Grid.Grid(Check_Button, "-sticky we -columnspan 2");
         Button :=
           Create
             (Label_Frame & ".add",
              "-text {" & Mc(Get_Context, "{Add command}") &
              "} -command AddCommand");
         Add
           (Button,
            Mc
              (Get_Context,
               "{Add the selected command. If there is that same menu label, it will be replaced.}"));
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky w");
         Button :=
           Create
             (Label_Frame & ".reset",
              "-text {" & Mc(Get_Context, "{Reset}") &
              "} -command ResetCommand");
         Add(Button, Mc(Get_Context, "{Clear all settings.}"));
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky e -row 3 -column 1");
         Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
         Label_Frame :=
           Create
             (Actions_Frame & ".commandsframe",
              "-text {" & Mc(Get_Context, "{Defined commands}") & "}");
         Tcl.Tk.Ada.Pack.Pack(Label_Frame, "-fill x");
         UpdateUserCommandsList;
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right -anchor s");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(Actions_Frame),
         "-text {" & Mc(Get_Context, "{User commands}") & "}");
      -- The program modules settings
      declare
         CloseButton: constant Ttk_Button :=
           Create
             (Modules_Frame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & Preferences_Frame & "}");
         HeaderLabel: Ttk_Label;
      begin
         HeaderLabel :=
           Create
             (Modules_Frame & ".enabled",
              "-text {" & Mc(Get_Context, "{Enabled}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel);
         Tcl.Tk.Ada.Grid.Column_Configure
           (Modules_Frame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (Modules_Frame & ".name",
              "-text {" & Mc(Get_Context, "{Name}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 1 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Modules_Frame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (Modules_Frame & ".version",
              "-text {" & Mc(Get_Context, "{Version}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 2 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Modules_Frame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (Modules_Frame & ".description",
              "-text {" & Mc(Get_Context, "{Description}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 3 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Modules_Frame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (Modules_Frame & ".show",
              "-text {" & Mc(Get_Context, "{Show}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 4 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Modules_Frame, HeaderLabel, "-weight 1");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Grid.Grid(CloseButton, "-sticky se -columnspan 5");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(Modules_Frame),
         "-text {" & Mc(Get_Context, "{Modules}") & "}");
      Canvas_Create
        (Preferences_Canvas, "window", "0 0 -anchor nw -window " & Notebook);
      Tcl_Eval(Get_Context, "update");
      configure
        (Preferences_Canvas,
         "-scrollregion [list " & BBox(Preferences_Canvas, "all") & "]");
      Preferences.Commands.AddCommands;
      Modules.Commands.AddCommands;
   end Create_Preferences_Ui;

   procedure Clear_Add_Command is
      Tentry: Ttk_Entry :=
        Get_Widget(".preferencesframe.canvas.notebook.actions.addframe.title");
      Button: constant Ttk_Button :=
        Get_Widget(".preferencesframe.canvas.notebook.actions.addframe.add");
   begin
      Delete(Tentry, "0", "end");
      Tentry.Name :=
        New_String
          (".preferencesframe.canvas.notebook.actions.addframe.command");
      Delete(Tentry, "0", "end");
      Tcl_SetVar
        (Get_Context,
         ".preferencesframe.canvas.notebook.actions.addframe.output", "0");
      Widgets.configure
        (Button, "-text {" & Mc(Get_Context, "{Add command}") & "}");
   end Clear_Add_Command;

end Preferences.UI;
