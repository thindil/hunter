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
        (Widget => Label,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{How often (in seconds) the program should check}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{for changes in current directory.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{If set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Slave => Label, Options => "-fill x");
      Scale :=
        Create
          (pathName => Label_Frame & ".intervalscale",
           options =>
             "-from 0 -to 30 -variable updateinterval -orient horizontal -command {SetLabel directory.interval}");
      Add
        (Widget => Scale,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{How often (in seconds) the program should check}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{for changes in current directory.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{If set to zero, autorefresh will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Slave => Scale, Options => "-fill x");
      Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
      Label_Frame :=
        Create
          (pathName => Preferences_Frame & ".preview",
           options =>
             "-text {" & Mc(Interp => Get_Context, Src_String => "{Preview}") &
             "}");
      Add_Button
        (Name => ".showpreview",
         Text => Mc(Interp => Get_Context, Src_String => "{Show preview}"),
         Value => Settings.Show_Preview,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Show second panel with preview of files and directories.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{If you disable this option, second panel will be visible only during}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{copying and moving files or directories and during creating new link.}"),
         Command => "SetShowPreview");
      Add_Button
        (Name => ".scaleimages",
         Text => Mc(Interp => Get_Context, Src_String => "{Scale images}"),
         Value => Settings.Scale_Images,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Scale images in preview. When disabled, images shows with}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{natural size. When enabled, images are resized to the size of the}") &
           LF & Mc(Interp => Get_Context, Src_String => "{preview window.}"),
         Command => "SetScaleImages");
      Check_Button :=
        Create
          (pathName => Label_Frame & ".syntaxhighlightning",
           options =>
             "-text {" &
             Mc(Interp => Get_Context,
                Src_String => "{Syntax highlightning}") &
             "} -command {SetColorText}");
      if Settings.Color_Text then
         Tcl_SetVar
           (interp => Check_Button.Interp,
            varName => Widget_Image(Win => Check_Button), newValue => "1");
      else
         Tcl_SetVar
           (interp => Check_Button.Interp,
            varName => Widget_Image(Win => Check_Button), newValue => "0");
      end if;
      if Colors_Enabled then
         State(Widget => Check_Button, StateSpec => "!disabled");
      else
         State(Widget => Check_Button, StateSpec => "disabled");
      end if;
      Add
        (Widget => Check_Button,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Color files syntax in files preview. Not all text (especially source code)}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{files are supported. You may not be able to enable this}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{option if you don't have installed the program 'highlight'.}"));
      Tcl.Tk.Ada.Pack.Pack(Slave => Check_Button, Options => "-fill x");
      Select_Color_Theme_Block :
      declare
         Search: Search_Type;
         File: Directory_Entry_Type;
         Themes_Name: Unbounded_String := Null_Unbounded_String;
         Color_Frame: constant Ttk_Frame :=
           Create(pathName => Label_Frame & ".colorframe");
         Combo_Box: Ttk_ComboBox :=
           Get_Widget(pathName => Color_Frame & "highlighttheme");
      begin
         if not Ada.Environment_Variables.Exists
             (Name => "HIGHLIGHT_DATADIR") then
            Ada.Environment_Variables.Set
              (Name => "HIGHLIGHT_DATADIR",
               Value =>
                 Ada.Environment_Variables.Value
                   (Name => "APPDIR", Default => "") &
                 "/usr/share/highlight");
         end if;
         if Exists
             (Name =>
                Ada.Environment_Variables.Value(Name => "HIGHLIGHT_DATADIR") &
                "/themes/base16") then
            Start_Search
              (Search => Search,
               Directory =>
                 Ada.Environment_Variables.Value(Name => "HIGHLIGHT_DATADIR") &
                 "/themes/base16",
               Pattern => "*.theme");
            Create_Themes_List_Loop :
            while More_Entries(Search => Search) loop
               Get_Next_Entry(Search => Search, Directory_Entry => File);
               Append
                 (Source => Themes_Name,
                  New_Item =>
                    " " &
                    Base_Name(Name => Simple_Name(Directory_Entry => File)));
            end loop Create_Themes_List_Loop;
            End_Search(Search => Search);
         end if;
         Combo_Box :=
           Create
             (pathName => Color_Frame & ".highlighttheme",
              options =>
                "-state readonly -values [list" &
                To_String(Source => Themes_Name) & "]");
         if Colors_Enabled then
            State(Widget => Combo_Box, StateSpec => "!disabled");
         else
            State(Widget => Combo_Box, StateSpec => "disabled");
         end if;
         Set
           (ComboBox => Combo_Box,
            Value => "{" & To_String(Source => Settings.Color_Theme) & "}");
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script => "SetColorTheme");
         Add
           (Widget => Combo_Box,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Select color theme for coloring syntax in text files in preview. You may}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{not be able to enable this option if you don't have installed}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String => "{the program 'highlight'.}"));
         Label :=
           Create
             (pathName => Color_Frame & ".themelabel",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Color theme:}") &
                "}");
         Add
           (Widget => Label,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Select color theme for coloring syntax in text files in preview. You may}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{not be able to enable this option if you don't have installed}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String => "{the program 'highlight'.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Combo_Box, Options => "-column 1 -row 0");
         Tcl.Tk.Ada.Pack.Pack(Slave => Color_Frame, Options => "-fill x");
      end Select_Color_Theme_Block;
      Add_Button
        (Name => ".monospacefont",
         Text =>
           Mc(Interp => Get_Context, Src_String => "{Use monospace font}"),
         Value => Settings.Monospace_Font,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Use monospace font in the preview of text files.}"),
         Command => "SetMonospaceFont");
      Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
      Label_Frame :=
        Create
          (pathName => Preferences_Frame & ".interface",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Interface}") & "}");
      Tcl_SetVar
        (interp => Check_Button.Interp, varName => "messagesinterval",
         newValue => Natural'Image(Settings.Auto_Close_Messages_Time));
      Label :=
        Create
          (pathName => Label_Frame & ".messageslabel",
           options =>
             "-text """ &
             Mc(Interp => Get_Context, Src_String => "{Hide messages after}") &
             "$messagesinterval " &
             Mc(Interp => Get_Context, Src_String => "{seconds}") & """");
      Add
        (Widget => Label,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{After that amount of seconds, all messages will be automatically closed by the}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{program. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Slave => Label, Options => "-fill x");
      Scale :=
        Create
          (pathName => Label_Frame & ".messagesscale",
           options =>
             "-from 0 -to 60 -variable messagesinterval -orient horizontal -command {SetLabel interface.messages}");
      Add
        (Widget => Scale,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{After that amount of seconds, all messages will be automatically closed by the}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{program. If you set it to 0, this feature will be disabled.}"));
      Tcl.Tk.Ada.Pack.Pack(Slave => Scale, Options => "-fill x");
      Add_Button
        (Name => ".stayinold",
         Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Stay in source directory}"),
         Value => Settings.Stay_In_Old,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{After copying, moving files and directories or creating new link, stay in old}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{directory, don't automatically go to destination directory.}"),
         Command => "SetStayInOld");
      Add_Button
        (Name => ".showfinished",
         Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show info about finished action}"),
         Value => Settings.Show_Finished_Info,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Show information about finished copying, moving and}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{deleting files or directories.}"),
         Command => "SetShowFinishedInfo");
      Add_Button
        (Name => ".toolbarsontop",
         Text => Mc(Interp => Get_Context, Src_String => "{Toolbars on top}"),
         Value => Settings.Toolbars_On_Top,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{If enabled, show toolbars for actions and information on top of the window.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{Otherwise, they will be at left side of the window.}"),
         Command => "SetToolbarsOnTop");
      Set_Ui_Theme_Block :
      declare
         Theme_Frame: constant Ttk_Frame :=
           Create(pathName => Label_Frame & ".colorframe");
         Themes_Names: Unbounded_String :=
           To_Unbounded_String(Source => Theme_Names);
         Color_Box: constant Ttk_ComboBox :=
           Create
             (pathName => Theme_Frame & ".uitheme",
              options => "-state readonly");
      begin
         if Index
             (Source => Themes_Names, Pattern => "hunter-dark", From => 1) =
           0 then
            Append(Source => Themes_Names, New_Item => " hunter-dark");
         end if;
         if Index
             (Source => Themes_Names, Pattern => "hunter-light", From => 1) =
           0 then
            Append(Source => Themes_Names, New_Item => " hunter-light");
         end if;
         Widgets.configure
           (Widgt => Color_Box,
            options =>
              "-values [list " & To_String(Source => Themes_Names) & "]");
         Bind
           (Widgt => Color_Box, Sequence => "<<ComboboxSelected>>",
            Script => "SetUITheme");
         Add
           (Widget => Color_Box,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Select other theme for the program. After selecting a new theme,}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{you will have to restart the program to apply all changes.}"));
         Label :=
           Create
             (pathName => Theme_Frame & ".themelabel",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{UI theme:}") & "}");
         Add
           (Widget => Label,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Select other theme for the program. After selecting a new theme,}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{you will have to restart the program to apply all changes.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Color_Box, Options => "-column 1 -row 0");
         Set
           (ComboBox => Color_Box,
            Value => To_String(Source => Settings.Ui_Theme));
         Tcl.Tk.Ada.Pack.Pack(Slave => Theme_Frame, Options => "-fill x");
      end Set_Ui_Theme_Block;
      Set_Toolbar_Size_Block :
      declare
         Toolbar_Frame: constant Ttk_Frame :=
           Create(pathName => Label_Frame & ".toolbarframe");
         Toolbar_Box: constant Ttk_ComboBox :=
           Create
             (pathName => Toolbar_Frame & ".toolbarsize",
              options =>
                "-state readonly -values [list {" &
                Mc(Interp => Get_Context, Src_String => "{small}") & "} {" &
                Mc(Interp => Get_Context, Src_String => "{medium}") & "} {" &
                Mc(Interp => Get_Context, Src_String => "{large}") & "} {" &
                Mc(Interp => Get_Context, Src_String => "{huge}") & "}]");
      begin
         Bind
           (Widgt => Toolbar_Box, Sequence => "<<ComboboxSelected>>",
            Script => "SetToolbars_Size");
         Add
           (Widget => Toolbar_Box,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Set the size of icons in toolbars}"));
         Label :=
           Create
             (pathName => Toolbar_Frame & ".toolbarlabel",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Toolbars size:}") &
                "}");
         Add
           (Widget => Label,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Set the size of icons in toolbars}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Toolbar_Box, Options => "-column 1 -row 0");
         case Settings.Toolbars_Size is
            when 8 .. 23 =>
               Set
                 (ComboBox => Toolbar_Box,
                  Value => Mc(Interp => Get_Context, Src_String => "{small}"));
            when 24 .. 31 =>
               Set
                 (ComboBox => Toolbar_Box,
                  Value =>
                    Mc(Interp => Get_Context, Src_String => "{medium}"));
            when 32 .. 63 =>
               Set
                 (ComboBox => Toolbar_Box,
                  Value => Mc(Interp => Get_Context, Src_String => "{large}"));
            when 64 .. 128 =>
               Set
                 (ComboBox => Toolbar_Box,
                  Value => Mc(Interp => Get_Context, Src_String => "{huge}"));
         end case;
         Tcl.Tk.Ada.Pack.Pack(Slave => Toolbar_Frame, Options => "-fill x");
      end Set_Toolbar_Size_Block;
      Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
      Label_Frame :=
        Create
          (pathName => Preferences_Frame & ".deleting",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Deleting}") & "}");
      Add_Button
        (Name => ".deletefiles",
         Text => Mc(Interp => Get_Context, Src_String => "{Delete files}"),
         Value => Settings.Delete_Files,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Delete selected files and directories instead of moving them to Trash.}"),
         Command => "SetDeleteFiles");
      Add_Button
        (Name => ".cleartrash",
         Text =>
           Mc(Interp => Get_Context, Src_String => "{Clear Trash on exit}"),
         Value => Settings.Clear_Trash_On_Exit,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Automatically clear Trash on exit from the program.}"),
         Command => "SetClearTrash");
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
