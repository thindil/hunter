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

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkNotebook;
with Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip;
with Modules;
with Modules.Commands;
with Preferences.Commands;
with UserCommands.UI;
with Utils;

package body Preferences.UI is

   procedure Create_Preferences_Ui is
      use Ada.Characters.Latin_1;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
      use Tcl.Tk.Ada.Widgets.TtkNotebook;
      use Tcl.Tk.Ada.Widgets.TtkScale;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tk.Ada.Widgets.TtkWidget;
      use Tcl.Tklib.Ada.Autoscroll;
      use Tcl.Tklib.Ada.Tooltip;
      use Utils;

      Label_Frame: Ttk_LabelFrame; --## rule line off GLOBAL_REFERENCES
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
         use Ada.Directories;

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
         use Tcl.Tk.Ada.TtkStyle;

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
      Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
      Label_Frame :=
        Create
          (pathName => Preferences_Frame & ".copying",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Copying or moving}") &
             "}");
      Add_Button
        (Name => ".overwrite",
         Text =>
           Mc(Interp => Get_Context, Src_String => "{Overwrite existing}"),
         Value => Settings.Overwrite_On_Exist,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{If enabled, during copying or moving files and directories,}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{if in destination directory exists file or directory with that}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{same name, the program will ask if overwrite it. If disabled, the}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String =>
                "{program will quietly give add underscore to the name of moved or}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{copied file or directory.}"),
         Command => "SetOverwrite");
      Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
      Create_Buttons_Block :
      declare
         Buttons_Frame: constant Ttk_Frame :=
           Create(pathName => Preferences_Frame & ".buttonsframe");
         Close_Button: constant Ttk_Button :=
           Create
             (pathName => Buttons_Frame & ".closebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Close}") &
                "} -command {ClosePreferences " & Preferences_Frame & "}");
         Restore_Button: constant Ttk_Button :=
           Create
             (pathName => Buttons_Frame & ".restorebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Restore defaults}") &
                "} -command RestoreDefaults");
      begin
         Add
           (Widget => Restore_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Restore default settings for the program. You will have to restart}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String => "{the program to apply all changes}"));
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Restore_Button, Options => "-side left");
         Add
           (Widget => Close_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(Slave => Close_Button, Options => "-side right");
         Tcl.Tk.Ada.Pack.Pack(Slave => Buttons_Frame, Options => "-fill x");
      end Create_Buttons_Block;
      TtkNotebook.Add
        (Notebook => Notebook,
         WindowName => Widget_Image(Win => Preferences_Frame),
         Options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Preferences}") & "}");
      -- Keyboard shortcuts settings
      Set_Keyboard_Shortcuts_Block :
      declare
         use Ada.Strings;
         use Ada.Strings.Fixed;
         use Tcl.Tk.Ada.Image.Photo;

         Buttons_Frame: constant Ttk_Frame :=
           Create(pathName => Shortcuts_Frame & ".buttonsframe");
         Close_Button: constant Ttk_Button :=
           Create
             (pathName => Buttons_Frame & ".closebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Close}") &
                "} -command {ClosePreferences " & Preferences_Frame & "}");
         Restore_Button: constant Ttk_Button :=
           Create
             (pathName => Buttons_Frame & ".restorebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Restore defaults}") &
                "} -command RestoreDefaultShortcuts");
         Keys_Labels: constant array(Accelerators'Range) of Unbounded_String :=
           (1 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Quit from the program}")),
            2 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Show bookmarks menu}")),
            3 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Search for the file or directory}")),
            4 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Show add new item menu}")),
            5 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Show delete menu}")),
            6 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String =>
                        "{Show menu with information about the program}")),
            7 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Open selected file or directory}")),
            8 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String =>
                        "{Select or unselect all files and directories}")),
            9 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Rename selected file or directory}")),
            10 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Copy selected files}")),
            11 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Move selected files}")),
            12 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Show the program preferences}")),
            13 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String =>
                        "{Open selected file or directory with command}")),
            14 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{File or directory information}")),
            15 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Preview file or directory}")),
            16 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Add bookmark to this directory}")),
            17 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Remove bookmark from this directory}")),
            18 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Execute selected program}")),
            19 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String =>
                        "{Restore deleted file or directory from Trash}")),
            20 =>
              To_Unbounded_String
                (Source =>
                   Mc
                     (Interp => Get_Context,
                      Src_String => "{Show the user defined actions}")));
         Shortcut_Label: Ttk_Label := Get_Widget(pathName => ".");
         Button: Ttk_Button := Get_Widget(pathName => ".");
         Button_Image: constant Tk_Photo :=
           Create
             (pathName => "refreshicon",
              options =>
                "-file {../share/hunter/images/document-edit.svg} -format ""svg -scaletoheight [expr {[font metrics DefaultFont -linespace]}]""");
      begin
         Create_Shortcuts_UI_Loop :
         for I in Keys_Labels'Range loop
            Shortcut_Label :=
              Create
                (pathName =>
                   Shortcuts_Frame & ".label" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options =>
                   "-text {" & To_String(Source => Keys_Labels(I)) & ": }");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Shortcut_Label, Options => "-sticky w");
            Shortcut_Label :=
              Create
                (pathName =>
                   Shortcuts_Frame & ".labelshortcut" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options =>
                   "-text {" & To_String(Source => Accelerators(I)) &
                   "} -wraplength 150");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Shortcut_Label,
               Options => "-sticky w -column 1 -row" & Natural'Image(I - 1));
            Button :=
              Create
                (pathName =>
                   Shortcuts_Frame & ".button" &
                   Trim(Source => Positive'Image(I), Side => Left),
                 options =>
                   "-style Toolbutton -image " &
                   Widget_Image(Win => Button_Image) &
                   " -command {StartChangingShortcut" & Positive'Image(I) &
                   "}");
            Add
              (Widget => Button,
               Message =>
                 Mc
                   (Interp => Get_Context,
                    Src_String => "{Change keyboard shortcut for}") &
                 ":" & LF & To_String(Source => Keys_Labels(I)));
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button,
               Options => "-sticky w -column 2 -row" & Natural'Image(I - 1));
         end loop Create_Shortcuts_UI_Loop;
         Add
           (Widget => Restore_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Restore default keyboard shortcuts for the program.}"));
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Restore_Button, Options => "-side left");
         Add
           (Widget => Close_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(Slave => Close_Button, Options => "-side right");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Buttons_Frame, Options => "-sticky we -columnspan 3");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Shortcuts_Frame, Slave => Buttons_Frame,
            Options => "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (Master => Shortcuts_Frame, Slave => Buttons_Frame,
            Options => "-weight 1");
      end Set_Keyboard_Shortcuts_Block;
      TtkNotebook.Add
        (Notebook => Notebook,
         WindowName => Widget_Image(Win => Shortcuts_Frame),
         Options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Keyboard shortcuts}") &
           "}");
      -- Actions settings
      Actions_Settings_Block :
      declare
         use UserCommands.UI;

         Close_Button: constant Ttk_Button :=
           Create
             (pathName => Actions_Frame & ".closebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Close}") &
                "} -command {ClosePreferences " & Preferences_Frame & "}");
         Button: Ttk_Button;
         Shortcut_Label: Ttk_Label;
         Tentry: Ttk_Entry;
      begin
         Label_Frame :=
           Create
             (pathName => Actions_Frame & ".addframe",
              options =>
                "-text {" &
                Mc(Interp => Get_Context,
                   Src_String => "{Add a new command}") &
                "}");
         Shortcut_Label :=
           Create
             (pathName => Label_Frame & ".titlelbl",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Menu label:}") &
                "}");
         Add
           (Widget => Shortcut_Label,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Shortcut_Label, Options => "-sticky w");
         Tentry := Create(pathName => Label_Frame & ".title");
         Add
           (Widget => Tentry,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Tentry, Options => "-row 0 -column 1");
         Shortcut_Label :=
           Create
             (pathName => Label_Frame & ".commandlbl",
              options =>
                "-text {" &
                Mc(Interp => Get_Context,
                   Src_String => "{Command to execute:}") &
                "}");
         Add
           (Widget => Shortcut_Label,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Command to execute. That command must be a program not a shell command.}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String => "{@1 will be replaced by current directory}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Shortcut_Label, Options => "-sticky w");
         Tentry := Create(pathName => Label_Frame & ".command");
         Add
           (Widget => Tentry,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Command to execute. That command must be a program not a shell command.}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String => "{@1 will be replaced by current directory}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Tentry, Options => "-row 1 -column 1");
         Check_Button :=
           Create
             (pathName => Label_Frame & ".output",
              options =>
                "-text {" &
                Mc(Interp => Get_Context,
                   Src_String => "{Show command output in preview}") &
                "}");
         Add
           (Widget => Check_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{If checked, the command output will be shown in preview window.}") &
              LF &
              Mc(Interp => Get_Context,
                 Src_String =>
                   "{Otherwise, the command output will be ignored.}"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Check_Button, Options => "-sticky we -columnspan 2");
         Button :=
           Create
             (pathName => Label_Frame & ".add",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Add command}") &
                "} -command AddCommand");
         Add
           (Widget => Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Add the selected command. If there is that same menu label, it will be replaced.}"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-sticky w");
         Button :=
           Create
             (pathName => Label_Frame & ".reset",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Reset}") &
                "} -command ResetCommand");
         Add
           (Widget => Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Clear all settings.}"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-sticky e -row 3 -column 1");
         Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
         Label_Frame :=
           Create
             (pathName => Actions_Frame & ".commandsframe",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Defined commands}") &
                "}");
         Tcl.Tk.Ada.Pack.Pack(Slave => Label_Frame, Options => "-fill x");
         UpdateUserCommandsList;
         Add
           (Widget => Close_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Close_Button, Options => "-side right -anchor s");
      end Actions_Settings_Block;
      TtkNotebook.Add
        (Notebook => Notebook,
         WindowName => Widget_Image(Win => Actions_Frame),
         Options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{User commands}") & "}");
      -- The program modules settings
      Modules_Settings_Block :
      declare
         Close_Button: constant Ttk_Button :=
           Create
             (pathName => Modules_Frame & ".closebutton",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Close}") &
                "} -command {ClosePreferences " & Preferences_Frame & "}");
         Header_Label: Ttk_Label;
      begin
         Header_Label :=
           Create
             (pathName => Modules_Frame & ".enabled",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Enabled}") & "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Header_Label);
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Modules_Frame, Slave => Header_Label,
            Options => "-weight 1");
         Header_Label :=
           Create
             (pathName => Modules_Frame & ".name",
              options =>
                "-text {" & Mc(Interp => Get_Context, Src_String => "{Name}") &
                "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Header_Label, Options => "-column 1 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Modules_Frame, Slave => Header_Label,
            Options => "-weight 1");
         Header_Label :=
           Create
             (pathName => Modules_Frame & ".version",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Version}") & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Header_Label, Options => "-column 2 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Modules_Frame, Slave => Header_Label,
            Options => "-weight 1");
         Header_Label :=
           Create
             (pathName => Modules_Frame & ".description",
              options =>
                "-text {" &
                Mc(Interp => Get_Context, Src_String => "{Description}") &
                "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Header_Label, Options => "-column 3 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Modules_Frame, Slave => Header_Label,
            Options => "-weight 1");
         Header_Label :=
           Create
             (pathName => Modules_Frame & ".show",
              options =>
                "-text {" & Mc(Interp => Get_Context, Src_String => "{Show}") &
                "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Header_Label, Options => "-column 4 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Modules_Frame, Slave => Header_Label,
            Options => "-weight 1");
         Add
           (Widget => Close_Button,
            Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Back to the program}"));
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Close_Button, Options => "-sticky se -columnspan 5");
      end Modules_Settings_Block;
      TtkNotebook.Add
        (Notebook => Notebook,
         WindowName => Widget_Image(Win => Modules_Frame),
         Options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "{Modules}") &
           "}");
      Canvas_Create
        (Parent => Preferences_Canvas, Child_Type => "window",
         Options => "0 0 -anchor nw -window " & Notebook);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Preferences_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Preferences_Canvas, TagOrId => "all") & "]");
      Preferences.Commands.AddCommands;
      Modules.Commands.AddCommands;
   end Create_Preferences_Ui;

   procedure Clear_Add_Command is
      use Interfaces.C.Strings;

      Frame_Name: constant String :=
        ".preferencesframe.canvas.notebook.actions.addframe";
      Tentry: Ttk_Entry := Get_Widget(pathName => Frame_Name & ".title");
      Button: constant Ttk_Button :=
        Get_Widget(pathName => Frame_Name & ".add");
   begin
      Delete(TextEntry => Tentry, FirstIndex => "0", LastIndex => "end");
      Tentry.Name := New_String(Str => Frame_Name & ".command");
      Delete(TextEntry => Tentry, FirstIndex => "0", LastIndex => "end");
      Tcl_SetVar
        (interp => Get_Context, varName => Frame_Name & ".output",
         newValue => "0");
      Widgets.configure
        (Widgt => Button,
         options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Add command}") & "}");
   end Clear_Add_Command;

end Preferences.UI;
