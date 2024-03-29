-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;
with GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip;
with Bookmarks.UI;
with Common; use Common;
with Preferences; use Preferences;
with UserCommands;
with Utils;

package body Toolbars is

   procedure Set_Toolbars is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use GNAT.String_Split;
      use Bookmarks.UI;

      Fill: constant Character :=
        (if Settings.Toolbars_On_Top then 'y' else 'x');
      Main_Frame: constant Ttk_Frame := Get_Widget(pathName => ".mainframe");
      Toolbar: Ttk_Frame :=
        Get_Widget(pathName => Main_Frame & ".toolbars.actiontoolbar");
      Button: Ttk_Button := Get_Widget(pathName => ".");
      Label: constant Ttk_Label :=
        Get_Widget(pathName => Main_Frame & ".toolbars.label");
      Tokens: Slice_Set;
      Side: constant String :=
        (if Settings.Toolbars_On_Top then "left" else "top");
      Direction: constant String :=
        (if Settings.Toolbars_On_Top then "below" else "right");
      Orientation: constant String :=
        (if Settings.Toolbars_On_Top then "vertical" else "horizontal");
   begin
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Pack.Pack_Slaves(Master => Toolbar),
         Separators => " ");
      Set_Actions_Loop :
      for I in 1 .. Slice_Count(S => Tokens) loop
         Button.Name := New_String(Str => Slice(S => Tokens, Index => I));
         if Winfo_Get(Widgt => Button, Info => "class") = "TMenubutton" then
            configure(Widgt => Button, options => "-direction " & Direction);
         end if;
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Slave => Button, Options => "-side " & Side);
      end loop Set_Actions_Loop;
      Toolbar := Get_Widget(pathName => Main_Frame & ".toolbars.itemtoolbar");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Pack.Pack_Slaves(Master => Toolbar),
         Separators => " ");
      Set_Info_Loop :
      for I in 1 .. Slice_Count(S => Tokens) loop
         Button.Name := New_String(Str => Slice(S => Tokens, Index => I));
         if Winfo_Get(Widgt => Button, Info => "class") = "TMenubutton" then
            configure(Widgt => Button, options => "-direction " & Direction);
         end if;
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Slave => Button, Options => "-side " & Side);
      end loop Set_Info_Loop;
      Set_Action_Separators_Loop :
      for I in 1 .. 3 loop
         Button.Name :=
           New_String
             (Str =>
                Main_Frame & ".toolbars.actiontoolbar.separator" &
                Trim(Source => Positive'Image(I), Side => Both));
         configure(Widgt => Button, options => "-orient " & Orientation);
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Slave => Button,
            Options => "-side " & Side & " -pad" & Fill & " 5 -fill " & Fill);
      end loop Set_Action_Separators_Loop;
      Set_Info_Separators_Loop :
      for I in 1 .. 2 loop
         Button.Name :=
           New_String
             (Str =>
                Main_Frame & ".toolbars.itemtoolbar.separator" &
                Trim(Source => Positive'Image(I), Side => Both));
         configure(Widgt => Button, options => "-orient " & Orientation);
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Slave => Button,
            Options => "-side " & Side & " -pad" & Fill & " 5 -fill " & Fill);
      end loop Set_Info_Separators_Loop;
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(Str => Main_Frame & ".toolbars.itemtoolbar");
      if Settings.Toolbars_On_Top then
         Grid_Configure
           (Slave => Toolbar, Options => "-column 2 -row 0 -sticky e");
      else
         Grid_Configure
           (Slave => Toolbar, Options => "-column 0 -row 2 -sticky s");
      end if;
      Toolbar.Name := New_String(Str => Main_Frame & ".toolbars");
      if Settings.Toolbars_On_Top then
         Grid_Configure
           (Slave => Toolbar, Options => "-sticky we -row 0 -columnspan 2");
      else
         Grid_Configure
           (Slave => Toolbar,
            Options => "-sticky ns -row 3 -column 0 -columnspan 1");
         Column_Configure
           (Master => Main_Frame, Slave => Toolbar, Options => "-weight 0");
         Row_Configure
           (Master => Main_Frame, Slave => Toolbar, Options => "-weight 0");
      end if;
      Toolbar.Name := New_String(Str => Main_Frame & ".toolbars");
      if Settings.Toolbars_On_Top then
         Column_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 1");
         Row_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 0");
      else
         Column_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 0");
         Row_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 1");
      end if;
      if Current_Selected /= Null_Unbounded_String then
         Set_Actions_Buttons;
         Set_Bookmark_Button;
      end if;
   end Set_Toolbars;

   -- ****if* Toolbars/Toolbars.Set_Button
   -- FUNCTION
   -- Configure selected button on toolbars
   -- PARAMETERS
   -- Button       - Button to configure
   -- Tooltip_Text - Text which will be displayed as tooltip
   -- Image_Name   - Name of image which will be used as icon for button
   -- SOURCE
   procedure Set_Button
     (Button: Tk_Widget'Class; Tooltip_Text, Image_Name: String) is
      -- ****
      use Tcl.Tk.Ada.Image.Photo;
      use Tcl.Tklib.Ada.Tooltip;

      Button_Image: constant Tk_Photo :=
        Create
          (pathName => Image_Name & "icon",
           options =>
             "-file {../share/hunter/images/" & Image_Name &
             ".svg} -format {svg -scaletoheight" &
             Natural'Image(Settings.Toolbars_Size) & "}");
      pragma Unreferenced(Button_Image);
   begin
      Add(Widget => Button, Message => Tooltip_Text);
      configure
        (Widgt => Button,
         options =>
           "-style Toolbutton -image " & Image_Name & "icon -takefocus 0");
   end Set_Button;

   procedure Create_Action_Toolbar is
      use Ada.Characters.Latin_1;
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;

      Tool_Menu_Button: Ttk_MenuButton;
      Toolbars_Frame: constant Ttk_Frame :=
        Create(pathName => ".mainframe.toolbars");
      Toolbar: constant Ttk_Frame :=
        Create(pathName => Toolbars_Frame & ".actiontoolbar");
      Tool_Button: Ttk_Button;
      Separator: Ttk_Separator;
      Label: constant Ttk_Label :=
        Create(pathName => Toolbars_Frame & ".label");
      Button_Menu: Tk_Menu;
      Tool_Check_Button: Ttk_CheckButton;
   begin
      Tool_Button :=
        Create
          (pathName => Toolbar & ".quitbutton", options => "-command exit");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Quit from the program.}") &
           " \[" & To_String(Source => Accelerators(1)) & "\]",
         Image_Name => "quit");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Separator := Create(pathName => Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Slave => Separator);
      Tool_Menu_Button := Create(pathName => Toolbar & ".bookmarksbutton");
      Set_Button
        (Button => Tool_Menu_Button,
         Tooltip_Text =>
           Mc(Interp => Get_Context, Src_String => "{Show bookmarks menu}") &
           " \[" & To_String(Source => Accelerators(2)) & "\]",
         Image_Name => "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Menu_Button);
      Tool_Check_Button :=
        Create
          (pathName => Toolbar & ".searchbutton",
           options => "-command ToggleSearch");
      Set_Button
        (Button => Tool_Check_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Search for the file or directory}") &
           " \[" & To_String(Source => Accelerators(3)) & "\]",
         Image_Name => "edit-find");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Check_Button);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".selectbutton",
           options => "-command ToggleSelection");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Select or unselect all files and directories.}") &
           " \[" & To_String(Source => Accelerators(8)) & "\]",
         Image_Name => "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Separator := Create(pathName => Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Slave => Separator);
      Tool_Menu_Button := Create(pathName => Toolbar & ".userbutton");
      Set_Button
        (Button => Tool_Menu_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show user actions menu}") &
           " \[" & To_String(Source => Accelerators(20)) & "\]",
         Image_Name => "run-build");
      Button_Menu :=
        Create(pathName => ".actionsmenu", options => "-tearoff false");
      configure
        (Widgt => Tool_Menu_Button,
         options => "-menu " & Button_Menu & " -direction right");
      Set_User_Commands_Menu;
      Tool_Menu_Button := Create(pathName => Toolbar & ".newbutton");
      Set_Button
        (Button => Tool_Menu_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show add new item menu}") &
           " \[" & To_String(Source => Accelerators(4)) & "\]",
         Image_Name => "document-new");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Menu_Button);
      Button_Menu :=
        Create(pathName => ".newmenu", options => "-tearoff false");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{New directory}") &
           "} -command {ShowCreate directory}");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" & Mc(Interp => Get_Context, Src_String => "{New file}") &
           "} -command {ShowCreate file}");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" & Mc(Interp => Get_Context, Src_String => "{New link}") &
           "} -command {ShowCreate link}");
      configure(Widgt => Tool_Menu_Button, options => "-menu " & Button_Menu);
      Tool_Check_Button :=
        Create
          (pathName => Toolbar & ".renamebutton",
           options => "-command ToggleRename");
      Set_Button
        (Button => Tool_Check_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Rename selected file or directory}") &
           " \[" & To_String(Source => Accelerators(9)) & "\]",
         Image_Name => "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Check_Button);
      Tool_Check_Button :=
        Create
          (pathName => Toolbar & ".copybutton",
           options => "-command CopyData");
      Set_Button
        (Button => Tool_Check_Button,
         Tooltip_Text =>
           Mc(Interp => Get_Context, Src_String => "{Copy selected files}") &
           " \[" & To_String(Source => Accelerators(10)) & "\]." & LF &
           Mc(Interp => Get_Context,
              Src_String => "{Pressed button means start copying}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{currently selected files or directories.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{Press again to copy them.}"),
         Image_Name => "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Check_Button);
      Tool_Check_Button :=
        Create
          (pathName => Toolbar & ".movebutton",
           options => "-command MoveData");
      Set_Button
        (Button => Tool_Check_Button,
         Tooltip_Text =>
           Mc(Interp => Get_Context, Src_String => "{Move selected files}") &
           " \[" & To_String(Source => Accelerators(11)) & "\]." & LF &
           Mc(Interp => Get_Context,
              Src_String => "{Pressed button means start moving}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{currently selected files or directories.}") &
           LF &
           Mc(Interp => Get_Context,
              Src_String => "{Press again to move them.}"),
         Image_Name => "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Check_Button);
      Tool_Menu_Button := Create(pathName => Toolbar & ".deletebutton");
      Set_Button
        (Button => Tool_Menu_Button,
         Tooltip_Text =>
           Mc(Interp => Get_Context, Src_String => "{Show delete menu}") &
           " \[" & To_String(Source => Accelerators(5)) & "\]",
         Image_Name => "edit-delete");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Menu_Button);
      Button_Menu :=
        Create(pathName => ".deletemenu", options => "-tearoff false");
      if Settings.Delete_Files then
         Menu.Add
           (MenuWidget => Button_Menu, EntryType => "command",
            Options =>
              "-label {" &
              Mc(Interp => Get_Context, Src_String => "{Delete selected}") &
              "} -command StartDeleting");
      else
         Menu.Add
           (MenuWidget => Button_Menu, EntryType => "command",
            Options =>
              "-label {" &
              Mc(Interp => Get_Context,
                 Src_String => "{Move selected to Trash}") &
              "} -command StartDeleting");
      end if;
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Show Trash}") &
           "} -command ShowTrash");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Empty Trash}") &
           "} -command ClearTrash");
      configure(Widgt => Tool_Menu_Button, options => "-menu " & Button_Menu);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".cancelbutton",
           options => "-command CancelAction");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Discard all changes and back to files list \[Escape\]}"),
         Image_Name => "dialog-cancel");
      Tool_Button :=
        Create
          (pathName => Toolbar & ".restorebutton",
           options => "-command RestoreItems");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Restore selected file or directory from the trash}") &
           " \[" & To_String(Source => Accelerators(19)) & "\]",
         Image_Name => "document-revert");
      Separator := Create(pathName => Toolbar & ".separator3");
      Tcl.Tk.Ada.Pack.Pack(Slave => Separator);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".optionsbutton",
           options => "-command ShowPreferences");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show the program preferences}") &
           " \[" & To_String(Source => Accelerators(12)) & "\]",
         Image_Name => "configure");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Tool_Menu_Button := Create(pathName => Toolbar & ".aboutbutton");
      Set_Button
        (Button => Tool_Menu_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Show menu with information about the program}") &
           " \[" & To_String(Source => Accelerators(6)) & "\]",
         Image_Name => "help-about");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Menu_Button);
      Button_Menu :=
        Create(pathName => ".aboutmenu", options => "-tearoff false");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{About the program}") &
           "} -command ShowAbout");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Show README}") &
           "} -command {ShowFile README.md}");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Show list of changes}") &
           "} -command {ShowFile CHANGELOG.md}");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Get involved}") &
           "} -command {ShowFile CONTRIBUTING.md}");
      Menu.Add
        (MenuWidget => Button_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Show modding guide}") &
           "} -command {ShowFile MODDING.md}");
      configure(Widgt => Tool_Menu_Button, options => "-menu " & Button_Menu);
      Tcl.Tk.Ada.Grid.Grid(Slave => Toolbar, Options => "-sticky w");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Tcl.Tk.Ada.Grid.Grid(Slave => Toolbars_Frame);
   end Create_Action_Toolbar;

   procedure Create_Item_Toolbar is
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;

      Toolbar: constant Ttk_Frame :=
        Create(pathName => ".mainframe.toolbars.itemtoolbar");
      Tool_Button: Ttk_Button;
      Separator: Ttk_Separator;
      Tool_Radio_Button: Ttk_RadioButton;
   begin
      Tool_Button :=
        Create
          (pathName => Toolbar & ".runbutton", options => "-command Execute");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Execute selected program}") &
           " \[" & To_String(Source => Accelerators(18)) & "\]",
         Image_Name => "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".openbutton",
           options => "-command ActivateItem");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Open selected file or directory}") &
           " \[" & To_String(Source => Accelerators(7)) & "\]",
         Image_Name => "document-open");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".openwithbutton",
           options => "-command ToggleExecuteWith");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Open selected file or directory with command}") &
           " \[" & To_String(Source => Accelerators(13)) & "\]",
         Image_Name => "system-run");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Separator := Create(pathName => Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Slave => Separator);
      Tool_Radio_Button :=
        Create
          (pathName => Toolbar & ".previewbutton",
           options =>
             "-variable previewtype -value preview -command ShowPreviewOrInfo");
      Set_Button
        (Button => Tool_Radio_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Preview file or directory}") &
           " \[" & To_String(Source => Accelerators(15)) & "\]",
         Image_Name => "document-preview");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Radio_Button);
      Tool_Radio_Button :=
        Create
          (pathName => Toolbar & ".infobutton",
           options =>
             "-variable previewtype -value info -command ShowPreviewOrInfo");
      Set_Button
        (Button => Tool_Radio_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{File or directory information}") &
           " \[" & To_String(Source => Accelerators(14)) & "\]",
         Image_Name => "document-properties");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Radio_Button);
      Separator := Create(pathName => Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Slave => Separator);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".addbutton",
           options => "-command AddBookmark");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Add bookmark to this directory}") &
           " \[" & To_String(Source => Accelerators(16)) & "\]",
         Image_Name => "list-add");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Tool_Button :=
        Create
          (pathName => Toolbar & ".deletebutton",
           options => "-command RemoveBookmark");
      Set_Button
        (Button => Tool_Button,
         Tooltip_Text =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Remove bookmark from this directory}") &
           " \[" & To_String(Source => Accelerators(17)) & "\]",
         Image_Name => "list-remove");
      Tcl.Tk.Ada.Pack.Pack(Slave => Tool_Button);
      Tcl.Tk.Ada.Grid.Grid(Slave => Toolbar);
   end Create_Item_Toolbar;

   procedure Set_Actions_Buttons is
      use GNAT.OS_Lib;
      use Utils;

      Button: Ttk_Button :=
        Get_Widget(pathName => ".mainframe.toolbars.itemtoolbar.runbutton");
      Side: constant String :=
        (if Settings.Toolbars_On_Top then "left" else "top");
   begin
      if Is_Executable_File(Name => To_String(Source => Current_Selected)) then
         if Winfo_Get(Widgt => Button, Info => "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Slave => Button,
               Options =>
                 "-before .mainframe.toolbars.itemtoolbar.openwithbutton -side " &
                 Side);
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      end if;
      Button.Name :=
        New_String(Str => ".mainframe.toolbars.itemtoolbar.openbutton");
      if Can_Be_Opened
          (Mime_Type =>
             Get_Mime_Type
               (File_Name => To_String(Source => Current_Selected))) then
         if Winfo_Get(Widgt => Button, Info => "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Slave => Button,
               Options =>
                 "-before .mainframe.toolbars.itemtoolbar.openwithbutton -side " &
                 Side);
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      end if;
   end Set_Actions_Buttons;

   procedure Set_User_Commands_Menu is
      use UserCommands;

      Actions_Menu: constant Tk_Menu := Get_Widget(pathName => ".actionsmenu");
      Actions_Button: constant Ttk_MenuButton :=
        Get_Widget(pathName => ".mainframe.toolbars.actiontoolbar.userbutton");
      Side: constant String :=
        (if Settings.Toolbars_On_Top then "left" else "top");
   begin
      Delete(MenuWidget => Actions_Menu, StartIndex => "0", EndIndex => "end");
      if User_Commands_List.Is_Empty then
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Actions_Button);
      else
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Actions_Button,
            Options =>
              "-after .mainframe.toolbars.actiontoolbar.separator2 -side " &
              Side);
      end if;
      Add_User_Commands_Menu_Loop :
      for I in User_Commands_List.Iterate loop
         Menu.Add
           (MenuWidget => Actions_Menu, EntryType => "command",
            Options =>
              "-label {" & Commands_Container.Key(Position => I) &
              "} -command {ExecuteCommand {" &
              Commands_Container.Key(Position => I) & "}}");
      end loop Add_User_Commands_Menu_Loop;
   end Set_User_Commands_Menu;

end Toolbars;
