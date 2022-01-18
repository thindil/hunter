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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Common; use Common;
with Bookmarks.UI; use Bookmarks.UI;
with Preferences; use Preferences;
with UserCommands; use UserCommands;
with Utils; use Utils;

package body Toolbars is

   procedure Set_Toolbars is
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
         configure(Button, "-orient " & Orientation);
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
      if not Settings.Toolbars_On_Top then
         Column_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 0");
         Row_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 1");
      else
         Column_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 1");
         Row_Configure
           (Master => Toolbar, Slave => Label, Options => "-weight 0");
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
      Image: constant Tk_Photo :=
        Create
          (Image_Name & "icon",
           "-file {../share/hunter/images/" & Image_Name &
           ".svg} -format {svg -scaletoheight" &
           Natural'Image(Settings.Toolbars_Size) & "}");
      pragma Unreferenced(Image);
      -- ****
   begin
      Add(Button, Tooltip_Text);
      configure
        (Button,
         "-style Toolbutton -image " & Image_Name & "icon -takefocus 0");
   end Set_Button;

   procedure Create_Action_Toolbar is
      ToolMenuButton: Ttk_MenuButton;
      ToolbarsFrame: constant Ttk_Frame := Create(".mainframe.toolbars");
      Toolbar: constant Ttk_Frame := Create(ToolbarsFrame & ".actiontoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
      Label: constant Ttk_Label := Create(ToolbarsFrame & ".label");
      ButtonMenu: Tk_Menu;
      ToolCheckButton: Ttk_CheckButton;
   begin
      ToolButton := Create(Toolbar & ".quitbutton", "-command exit");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Quit from the program.}") & " \[" &
         To_String(Accelerators(1)) & "\]",
         "quit");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(Toolbar & ".bookmarksbutton");
      Set_Button
        (ToolMenuButton,
         Mc(Get_Context, "{Show bookmarks menu}") & " \[" &
         To_String(Accelerators(2)) & "\]",
         "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolCheckButton :=
        Create(Toolbar & ".searchbutton", "-command ToggleSearch");
      Set_Button
        (ToolCheckButton,
         Mc(Get_Context, "{Search for the file or directory}") & " \[" &
         To_String(Accelerators(3)) & "\]",
         "edit-find");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolButton :=
        Create(Toolbar & ".selectbutton", "-command ToggleSelection");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Select or unselect all files and directories.}") &
         " \[" & To_String(Accelerators(8)) & "\]",
         "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(Toolbar & ".userbutton");
      Set_Button
        (ToolMenuButton,
         Mc(Get_Context, "{Show user actions menu}") & " \[" &
         To_String(Accelerators(20)) & "\]",
         "run-build");
      ButtonMenu := Create(".actionsmenu", "-tearoff false");
      configure(ToolMenuButton, "-menu " & ButtonMenu & " -direction right");
      Set_User_Commands_Menu;
      ToolMenuButton := Create(Toolbar & ".newbutton");
      Set_Button
        (ToolMenuButton,
         Mc(Get_Context, "{Show add new item menu}") & " \[" &
         To_String(Accelerators(4)) & "\]",
         "document-new");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".newmenu", "-tearoff false");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{New directory}") &
         "} -command {ShowCreate directory}");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{New file}") &
         "} -command {ShowCreate file}");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{New link}") &
         "} -command {ShowCreate link}");
      configure(ToolMenuButton, "-menu " & ButtonMenu);
      ToolCheckButton :=
        Create(Toolbar & ".renamebutton", "-command ToggleRename");
      Set_Button
        (ToolCheckButton,
         Mc(Get_Context, "{Rename selected file or directory}") & " \[" &
         To_String(Accelerators(9)) & "\]",
         "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton := Create(Toolbar & ".copybutton", "-command CopyData");
      Set_Button
        (ToolCheckButton,
         Mc(Get_Context, "{Copy selected files}") & " \[" &
         To_String(Accelerators(10)) & "\]." & LF &
         Mc(Get_Context, "{Pressed button means start copying}") & LF &
         Mc(Get_Context, "{currently selected files or directories.}") & LF &
         Mc(Get_Context, "{Press again to copy them.}"),
         "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton := Create(Toolbar & ".movebutton", "-command MoveData");
      Set_Button
        (ToolCheckButton,
         Mc(Get_Context, "{Move selected files}") & " \[" &
         To_String(Accelerators(11)) & "\]." & LF &
         Mc(Get_Context, "{Pressed button means start moving}") & LF &
         Mc(Get_Context, "{currently selected files or directories.}") & LF &
         Mc(Get_Context, "{Press again to move them.}"),
         "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolMenuButton := Create(Toolbar & ".deletebutton");
      Set_Button
        (ToolMenuButton,
         Mc(Get_Context, "{Show delete menu}") & " \[" &
         To_String(Accelerators(5)) & "\]",
         "edit-delete");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".deletemenu", "-tearoff false");
      if Settings.Delete_Files then
         Menu.Add
           (ButtonMenu, "command",
            "-label {" & Mc(Get_Context, "{Delete selected}") &
            "} -command StartDeleting");
      else
         Menu.Add
           (ButtonMenu, "command",
            "-label {" & Mc(Get_Context, "{Move selected to Trash}") &
            "} -command StartDeleting");
      end if;
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Show Trash}") &
         "} -command ShowTrash");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Empty Trash}") &
         "} -command ClearTrash");
      configure(ToolMenuButton, "-menu " & ButtonMenu);
      ToolButton := Create(Toolbar & ".cancelbutton", "-command CancelAction");
      Set_Button
        (ToolButton,
         Mc
           (Get_Context,
            "{Discard all changes and back to files list \[Escape\]}"),
         "dialog-cancel");
      ToolButton :=
        Create(Toolbar & ".restorebutton", "-command RestoreItems");
      Set_Button
        (ToolButton,
         Mc
           (Get_Context,
            "{Restore selected file or directory from the trash}") &
         " \[" & To_String(Accelerators(19)) & "\]",
         "document-revert");
      Separator := Create(Toolbar & ".separator3");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton :=
        Create(Toolbar & ".optionsbutton", "-command ShowPreferences");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Show the program preferences}") & " \[" &
         To_String(Accelerators(12)) & "\]",
         "configure");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton := Create(Toolbar & ".aboutbutton");
      Set_Button
        (ToolMenuButton,
         Mc(Get_Context, "{Show menu with information about the program}") &
         " \[" & To_String(Accelerators(6)) & "\]",
         "help-about");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".aboutmenu", "-tearoff false");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{About the program}") &
         "} -command ShowAbout");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Show README}") &
         "} -command {ShowFile README.md}");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Show list of changes}") &
         "} -command {ShowFile CHANGELOG.md}");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Get involved}") &
         "} -command {ShowFile CONTRIBUTING.md}");
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Show modding guide}") &
         "} -command {ShowFile MODDING.md}");
      configure(ToolMenuButton, "-menu " & ButtonMenu);
      Tcl.Tk.Ada.Grid.Grid(Toolbar, "-sticky w");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Grid(ToolbarsFrame);
   end Create_Action_Toolbar;

   procedure Create_Item_Toolbar is
      Toolbar: constant Ttk_Frame := Create(".mainframe.toolbars.itemtoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
      ToolRadioButton: Ttk_RadioButton;
   begin
      ToolButton := Create(Toolbar & ".runbutton", "-command Execute");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Execute selected program}") & " \[" &
         To_String(Accelerators(18)) & "\]",
         "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(Toolbar & ".openbutton", "-command ActivateItem");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory}") & " \[" &
         To_String(Accelerators(7)) & "\]",
         "document-open");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create(Toolbar & ".openwithbutton", "-command ToggleExecuteWith");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory with command}") &
         " \[" & To_String(Accelerators(13)) & "\]",
         "system-run");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolRadioButton :=
        Create
          (Toolbar & ".previewbutton",
           "-variable previewtype -value preview -command ShowPreviewOrInfo");
      Set_Button
        (ToolRadioButton,
         Mc(Get_Context, "{Preview file or directory}") & " \[" &
         To_String(Accelerators(15)) & "\]",
         "document-preview");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      ToolRadioButton :=
        Create
          (Toolbar & ".infobutton",
           "-variable previewtype -value info -command ShowPreviewOrInfo");
      Set_Button
        (ToolRadioButton,
         Mc(Get_Context, "{File or directory information}") & " \[" &
         To_String(Accelerators(14)) & "\]",
         "document-properties");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      Separator := Create(Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(Toolbar & ".addbutton", "-command AddBookmark");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Add bookmark to this directory}") & " \[" &
         To_String(Accelerators(16)) & "\]",
         "list-add");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create(Toolbar & ".deletebutton", "-command RemoveBookmark");
      Set_Button
        (ToolButton,
         Mc(Get_Context, "{Remove bookmark from this directory}") & " \[" &
         To_String(Accelerators(17)) & "\]",
         "list-remove");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Tcl.Tk.Ada.Grid.Grid(Toolbar);
   end Create_Item_Toolbar;

   procedure Set_Actions_Buttons is
      Button: Ttk_Button :=
        Get_Widget(".mainframe.toolbars.itemtoolbar.runbutton");
      Side: constant String :=
        (if Settings.Toolbars_On_Top then "left" else "top");
   begin
      if Is_Executable_File(To_String(Current_Selected)) then
         if Winfo_Get(Button, "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Button,
               "-before .mainframe.toolbars.itemtoolbar.openwithbutton -side " &
               Side);
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.openbutton");
      if Can_Be_Opened(Get_Mime_Type(To_String(Current_Selected))) then
         if Winfo_Get(Button, "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Button,
               "-before .mainframe.toolbars.itemtoolbar.openwithbutton -side " &
               Side);
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
   end Set_Actions_Buttons;

   procedure Set_User_Commands_Menu is
      ActionsMenu: constant Tk_Menu := Get_Widget(".actionsmenu");
      ActionsButton: constant Ttk_MenuButton :=
        Get_Widget(".mainframe.toolbars.actiontoolbar.userbutton");
      Side: constant String :=
        (if Settings.Toolbars_On_Top then "left" else "top");
   begin
      Delete(ActionsMenu, "0", "end");
      if UserCommandsList.Is_Empty then
         Tcl.Tk.Ada.Pack.Pack_Forget(ActionsButton);
      else
         Tcl.Tk.Ada.Pack.Pack
           (ActionsButton,
            "-after .mainframe.toolbars.actiontoolbar.separator2 -side " &
            Side);
      end if;
      Add_User_Commands_Menu_Loop :
      for I in UserCommandsList.Iterate loop
         Menu.Add
           (ActionsMenu, "command",
            "-label {" & Commands_Container.Key(I) &
            "} -command {ExecuteCommand {" & Commands_Container.Key(I) & "}}");
      end loop Add_User_Commands_Menu_Loop;
   end Set_User_Commands_Menu;

end Toolbars;
