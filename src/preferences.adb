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
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
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
with Utils; use Utils;

package body Preferences is

   procedure LoadSettings is
      Reader: Tree_Reader;
      DataFile: File_Input;
      SettingsData: Document;
      NodesList: Node_List;
      NodeName: Unbounded_String;
      DataNode: Node;
      function LoadBoolean(Value: String) return Boolean is
      begin
         if Value = "Yes" then
            return True;
         else
            return False;
         end if;
      end LoadBoolean;
   begin
      SetDefaultSettings;
      SetDefaultAccelerators;
      Open
        (Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml",
         DataFile);
      Parse(Reader, DataFile);
      Close(DataFile);
      SettingsData := Get_Tree(Reader);
      NodesList := Child_Nodes(First_Child(SettingsData));
      for I in 0 .. Length(NodesList) - 1 loop
         DataNode := Item(NodesList, I);
         NodeName := To_Unbounded_String(Node_Name(DataNode));
         -- The program settings
         if NodeName = To_Unbounded_String("setting") then
            if Get_Attribute(DataNode, "name") = "ShowHidden" then
               Settings.ShowHidden :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowLastModified" then
               Settings.ShowLastModified :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ScaleImages" then
               Settings.ScaleImages :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") =
              "AutoCloseMessagesTime" then
               Settings.AutoCloseMessagesTime :=
                 Natural'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "WindowWidth" then
               Settings.WindowWidth :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "WindowHeight" then
               Settings.WindowHeight :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowPreview" then
               Settings.ShowPreview :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "StayInOld" then
               Settings.StayInOld :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ColorText" then
               Settings.ColorText :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ColorTheme" then
               Settings.ColorTheme :=
                 To_Unbounded_String(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "DeleteFiles" then
               Settings.DeleteFiles :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ClearTrashOnExit" then
               Settings.ClearTrashOnExit :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowFinishedInfo" then
               Settings.ShowFinishedInfo :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "OverwriteOnExist" then
               Settings.OverwriteOnExist :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ToolbarsOnTop" then
               Settings.ToolbarsOnTop :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "AutoRefreshInterval" then
               Settings.AutoRefreshInterval :=
                 Natural'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "UITheme" then
               Settings.UITheme :=
                 To_Unbounded_String(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ToolbarsSize" then
               Settings.ToolbarsSize :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "MonospaceFont" then
               Settings.MonospaceFont :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            end if;
         -- The keyboard shortcuts
         elsif NodeName = To_Unbounded_String("accelerator") then
            Accelerators(Positive'Value(Get_Attribute(DataNode, "index"))) :=
              To_Unbounded_String(Get_Attribute(DataNode, "value"));
         -- The user defined commands
         elsif NodeName = To_Unbounded_String("command") then
            if Get_Attribute(DataNode, "needoutput") = "Yes" then
               UserCommandsList.Include
                 (Get_Attribute(DataNode, "menuentry"),
                  (NeedOutput => True,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(DataNode)))));
            else
               UserCommandsList.Include
                 (Get_Attribute(DataNode, "menuentry"),
                  (NeedOutput => False,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(DataNode)))));
            end if;
         -- The program modules
         elsif NodeName = To_Unbounded_String("module") then
            Enabled_Modules.Append
              (To_Unbounded_String(Get_Attribute(DataNode, "path")));
         end if;
      end loop;
      if FindExecutable("highlight") = "" then
         Settings.ColorText := False;
      end if;
   exception
      when Ada.Directories.Name_Error =>
         null;
   end LoadSettings;

   procedure SavePreferences is
      ConfigFile: File_Type;
      Configuration: DOM_Implementation;
      SettingNode, MainNode: DOM.Core.Element;
      SettingsData: Document;
      UserCommandNode: Text;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         if Value then
            Set_Attribute(SettingNode, "value", "Yes");
         else
            Set_Attribute(SettingNode, "value", "No");
         end if;
      end SaveBoolean;
      procedure SaveNumber(Value: Natural; Name: String) is
         RawValue: constant String :=
           Trim(Natural'Image(Value), Ada.Strings.Left);
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", RawValue);
      end SaveNumber;
      procedure SaveString(Value: Unbounded_String; Name: String) is
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", To_String(Value));
      end SaveString;
   begin
      SettingsData := Create_Document(Configuration);
      MainNode := Create_Element(SettingsData, "hunter");
      MainNode := Append_Child(SettingsData, MainNode);
      SaveBoolean(Settings.ShowHidden, "ShowHidden");
      SaveBoolean(Settings.ShowLastModified, "ShowLastModified");
      SaveBoolean(Settings.ScaleImages, "ScaleImages");
      SaveNumber(Settings.AutoCloseMessagesTime, "AutoCloseMessagesTime");
      SaveNumber(Settings.WindowWidth, "WindowWidth");
      SaveNumber(Settings.WindowHeight, "WindowHeight");
      SaveBoolean(Settings.ShowPreview, "ShowPreview");
      SaveBoolean(Settings.StayInOld, "StayInOld");
      SaveBoolean(Settings.ColorText, "ColorText");
      SaveString(Settings.ColorTheme, "ColorTheme");
      SaveBoolean(Settings.DeleteFiles, "DeleteFiles");
      SaveBoolean(Settings.ClearTrashOnExit, "ClearTrashOnExit");
      SaveBoolean(Settings.ShowFinishedInfo, "ShowFinishedInfo");
      SaveBoolean(Settings.OverwriteOnExist, "OverwriteOnExist");
      SaveBoolean(Settings.ToolbarsOnTop, "ToolbarsOnTop");
      SaveNumber(Settings.AutoRefreshInterval, "AutoRefreshInterval");
      SaveString(Settings.UITheme, "UITheme");
      SaveNumber(Settings.ToolbarsSize, "ToolbarsSize");
      SaveBoolean(Settings.MonospaceFont, "MonospaceFont");
      for I in Accelerators'Range loop
         SettingNode := Create_Element(SettingsData, "accelerator");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "index", Trim(Positive'Image(I), Left));
         Set_Attribute(SettingNode, "value", To_String(Accelerators(I)));
      end loop;
      for I in UserCommandsList.Iterate loop
         SettingNode := Create_Element(SettingsData, "command");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "menuentry", Commands_Container.Key(I));
         if UserCommandsList(I).NeedOutput then
            Set_Attribute(SettingNode, "needoutput", "Yes");
         else
            Set_Attribute(SettingNode, "needoutput", "No");
         end if;
         UserCommandNode :=
           Create_Text_Node
             (SettingsData, To_String(UserCommandsList(I).Command));
         UserCommandNode := Append_Child(SettingNode, UserCommandNode);
      end loop;
      for ModuleName of Enabled_Modules loop
         SettingNode := Create_Element(SettingsData, "module");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "path", To_String(ModuleName));
      end loop;
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      Create
        (ConfigFile, Out_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml");
      Write
        (Stream => Stream(ConfigFile), N => SettingsData,
         Pretty_Print => True);
      Close(ConfigFile);
   end SavePreferences;

   procedure CreatePreferencesUI is
      LabelFrame: Ttk_LabelFrame;
      CheckButton: Ttk_CheckButton;
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      MainFrame: constant Ttk_Frame := Create(".preferencesframe");
      ScrollX: constant Ttk_Scrollbar :=
        Create
          (MainFrame & ".scrollx",
           "-orient horizontal -command [list " & MainFrame &
           ".canvas xview]");
      ScrollY: constant Ttk_Scrollbar :=
        Create
          (MainFrame & ".scrolly",
           "-orient vertical -command [list " & MainFrame & ".canvas yview]");
      Canvas: constant Tk_Canvas :=
        Create
          (MainFrame & ".canvas",
           "-xscrollcommand {" & ScrollX & " set} -yscrollcommand {" &
           ScrollY & " set}");
      Notebook: constant Ttk_Notebook := Create(Canvas & ".notebook");
      PreferencesFrame: constant Ttk_Frame :=
        Create(Notebook & ".preferences");
      ColorsEnabled: constant Boolean :=
        (if FindExecutable("highlight", False)'Length > 0 then True
         else False);
      ShortcutsFrame: constant Ttk_Frame := Create(Notebook & ".shortcuts");
      ActionsFrame: constant Ttk_Frame := Create(Notebook & ".actions");
      ModulesFrame: constant Ttk_Frame := Create(Notebook & ".modules");
      procedure AddButton
        (Name, Text: String; Value: Boolean; TooltipText, Command: String) is
         CheckButton: constant Ttk_CheckButton :=
           Create
             (LabelFrame & Name, "-text {" & Text & "} -command " & Command);
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
      Autoscroll(ScrollX);
      Autoscroll(ScrollY);
      Tcl.Tk.Ada.Pack.Pack(ScrollX, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(ScrollY, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(Canvas, "-side top -fill both -expand true");
      LabelFrame :=
        Create
          (PreferencesFrame & ".directory",
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
          (LabelFrame & ".intervallabel",
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
          (LabelFrame & ".intervalscale",
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
          (PreferencesFrame & ".preview",
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
          (LabelFrame & ".syntaxhighlightning",
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
         ColorFrame: constant Ttk_Frame := Create(LabelFrame & ".colorframe");
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
             (ColorFrame & ".highlighttheme",
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
             (ColorFrame & ".themelabel",
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
          (PreferencesFrame & ".interface",
           "-text {" & Mc(Get_Context, "{Interface}") & "}");
      Tcl_SetVar
        (CheckButton.Interp, "messagesinterval",
         Natural'Image(Settings.AutoCloseMessagesTime));
      Label :=
        Create
          (LabelFrame & ".messageslabel",
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
          (LabelFrame & ".messagesscale",
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
         ThemeFrame: constant Ttk_Frame := Create(LabelFrame & ".colorframe");
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
               "{Select other theme for the program. After selecting a new theme,\nyou will have to restart the program to apply all changes.}"));
         Label :=
           Create
             (ThemeFrame & ".themelabel",
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
           Create(LabelFrame & ".toolbarframe");
         ToolbarBox: constant Ttk_ComboBox :=
           Create
             (ToolbarFrame & ".toolbarsize",
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
             (ToolbarFrame & ".toolbarlabel",
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
          (PreferencesFrame & ".deleting",
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
          (PreferencesFrame & ".copying",
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
           Create(PreferencesFrame & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & PreferencesFrame & "}");
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
               "{Restore default settings for the program. You will have to restart\nthe program to apply all changes}"));
         Tcl.Tk.Ada.Pack.Pack(RestoreButton, "-side left");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right");
         Tcl.Tk.Ada.Pack.Pack(ButtonsFrame, "-fill x");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(PreferencesFrame),
         "-text {" & Mc(Get_Context, "{Preferences}") & "}");
      -- Keyboard shortcuts settings
      declare
         ButtonsFrame: constant Ttk_Frame :=
           Create(ShortcutsFrame & ".buttonsframe");
         CloseButton: constant Ttk_Button :=
           Create
             (ButtonsFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & PreferencesFrame & "}");
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
         for I in KeysLabels'Range loop
            Label :=
              Create
                (ShortcutsFrame & ".label" & Trim(Positive'Image(I), Left),
                 "-text {" & To_String(KeysLabels(I)) & ": }");
            Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
            Label :=
              Create
                (ShortcutsFrame & ".labelshortcut" &
                 Trim(Positive'Image(I), Left),
                 "-text {" & To_String(Accelerators(I)) & "} -wraplength 150");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-sticky w -column 1 -row" & Natural'Image(I - 1));
            Button :=
              Create
                (ShortcutsFrame & ".button" & Trim(Positive'Image(I), Left),
                 "-style Toolbutton -image " & Widget_Image(Image) &
                 " -command {StartChangingShortcut" & Positive'Image(I) & "}");
            Add
              (Button,
               Mc(Get_Context, "{Change keyboard shortcut for}") & ":\n" &
               To_String(KeysLabels(I)));
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
         Tcl.Tk.Ada.Grid.Column_Configure
           (ShortcutsFrame, ButtonsFrame, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (ShortcutsFrame, ButtonsFrame, "-weight 1");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(ShortcutsFrame),
         "-text {" & Mc(Get_Context, "{Keyboard shortcuts}") & "}");
      -- Actions settings
      declare
         CloseButton: constant Ttk_Button :=
           Create
             (ActionsFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & PreferencesFrame & "}");
         Button: Ttk_Button;
         Label: Ttk_Label;
         Tentry: Ttk_Entry;
      begin
         LabelFrame :=
           Create
             (ActionsFrame & ".addframe",
              "-text {" & Mc(Get_Context, "{Add a new command}") & "}");
         Label :=
           Create
             (LabelFrame & ".titlelbl",
              "-text {" & Mc(Get_Context, "{Menu label:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
         Tentry := Create(LabelFrame & ".title");
         Add
           (Tentry,
            Mc
              (Get_Context,
               "{Text which will be shown in user actions menu.}"));
         Tcl.Tk.Ada.Grid.Grid(Tentry, "-row 0 -column 1");
         Label :=
           Create
             (LabelFrame & ".commandlbl",
              "-text {" & Mc(Get_Context, "{Command to execute:}") & "}");
         Add
           (Label,
            Mc
              (Get_Context,
               "{Command to execute. That command must be a program not a shell command.\n@1 will be replaced by current directory\n@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
         Tentry := Create(LabelFrame & ".command");
         Add
           (Tentry,
            Mc
              (Get_Context,
               "{Command to execute. That command must be a program not a shell command.\n@1 will be replaced by current directory\n@2 will be replaced by currently selected item on list.}"));
         Tcl.Tk.Ada.Grid.Grid(Tentry, "-row 1 -column 1");
         CheckButton :=
           Create
             (LabelFrame & ".output",
              "-text {" & Mc(Get_Context, "{Show command output in preview}") &
              "}");
         Add
           (CheckButton,
            Mc
              (Get_Context,
               "{If checked, the command output will be shown in preview window.\nOtherwise, the command output will be ignored.}"));
         Tcl.Tk.Ada.Grid.Grid(CheckButton, "-sticky we -columnspan 2");
         Button :=
           Create
             (LabelFrame & ".add",
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
             (LabelFrame & ".reset",
              "-text {" & Mc(Get_Context, "{Reset}") &
              "} -command ResetCommand");
         Add(Button, Mc(Get_Context, "{Clear all settings.}"));
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky e -row 3 -column 1");
         Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
         LabelFrame :=
           Create
             (ActionsFrame & ".commandsframe",
              "-text {" & Mc(Get_Context, "{Defined commands}") & "}");
         Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
         UpdateUserCommandsList;
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Pack.Pack(CloseButton, "-side right -anchor s");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(ActionsFrame),
         "-text {" & Mc(Get_Context, "{User commands}") & "}");
      -- The program modules settings
      declare
         CloseButton: constant Ttk_Button :=
           Create
             (ModulesFrame & ".closebutton",
              "-text {" & Mc(Get_Context, "{Close}") &
              "} -command {ClosePreferences " & PreferencesFrame & "}");
         HeaderLabel: Ttk_Label;
      begin
         HeaderLabel :=
           Create
             (ModulesFrame & ".enabled",
              "-text {" & Mc(Get_Context, "{Enabled}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel);
         Tcl.Tk.Ada.Grid.Column_Configure
           (ModulesFrame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (ModulesFrame & ".name",
              "-text {" & Mc(Get_Context, "{Name}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 1 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (ModulesFrame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (ModulesFrame & ".version",
              "-text {" & Mc(Get_Context, "{Version}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 2 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (ModulesFrame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (ModulesFrame & ".description",
              "-text {" & Mc(Get_Context, "{Description}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 3 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (ModulesFrame, HeaderLabel, "-weight 1");
         HeaderLabel :=
           Create
             (ModulesFrame & ".show",
              "-text {" & Mc(Get_Context, "{Show}") & "}");
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 4 -row 0");
         Tcl.Tk.Ada.Grid.Column_Configure
           (ModulesFrame, HeaderLabel, "-weight 1");
         Add(CloseButton, Mc(Get_Context, "{Back to the program}"));
         Tcl.Tk.Ada.Grid.Grid(CloseButton, "-sticky se -columnspan 5");
      end;
      TtkNotebook.Add
        (Notebook, Widget_Image(ModulesFrame),
         "-text {" & Mc(Get_Context, "{Modules}") & "}");
      Canvas_Create(Canvas, "window", "0 0 -anchor nw -window " & Notebook);
      Tcl_Eval(Get_Context, "update");
      configure(Canvas, "-scrollregion [list " & BBox(Canvas, "all") & "]");
      Preferences.Commands.AddCommands;
      Modules.Commands.AddCommands;
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
         To_Unbounded_String("Alt-d"), To_Unbounded_String("Alt-e"),
         To_Unbounded_String("Alt-s"), To_Unbounded_String("Alt-t"));
   end SetDefaultAccelerators;

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

end Preferences;
