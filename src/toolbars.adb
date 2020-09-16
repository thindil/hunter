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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
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
with Bookmarks; use Bookmarks;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with Utils; use Utils;

package body Toolbars is

   procedure SetToolbars is
      Side, Direction, Orientation: Unbounded_String;
      Fill: String(1 .. 1);
      Toolbar, MainFrame: Ttk_Frame;
      Button: Ttk_Button;
      Label: Ttk_Label;
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String(".mainframe.toolbars.actiontoolbar.quitbutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.searchbutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.selectbutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.renamebutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.copybutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.movebutton"),
         To_Unbounded_String
           (".mainframe.toolbars.actiontoolbar.optionsbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.runbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.openbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.openwithbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.previewbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.infobutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.addbutton"),
         To_Unbounded_String(".mainframe.toolbars.itemtoolbar.deletebutton"));
      MenuButtonsNames: constant array
        (Positive range <>) of Unbounded_String :=
        (To_Unbounded_String
           (".mainframe.toolbars.actiontoolbar.bookmarksbutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.newbutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.deletebutton"),
         To_Unbounded_String(".mainframe.toolbars.actiontoolbar.aboutbutton"));
   begin
      if not Settings.ToolbarsOnTop then
         Side := To_Unbounded_String("top");
         Fill := "x";
         Direction := To_Unbounded_String("right");
         Orientation := To_Unbounded_String("horizontal");
      else
         Side := To_Unbounded_String("left");
         Fill := "y";
         Direction := To_Unbounded_String("below");
         Orientation := To_Unbounded_String("vertical");
      end if;
      Button.Interp := Get_Context;
      for Name of MenuButtonsNames loop
         Button.Name := New_String(To_String(Name));
         configure(Button, "-direction " & To_String(Direction));
         Tcl.Tk.Ada.Pack.Pack_Configure(Button, "-side " & To_String(Side));
      end loop;
      for Name of ButtonsNames loop
         Button.Name := New_String(To_String(Name));
         Tcl.Tk.Ada.Pack.Pack_Configure(Button, "-side " & To_String(Side));
      end loop;
      for I in 1 .. 3 loop
         Button.Name :=
           New_String
             (".mainframe.toolbars.actiontoolbar.separator" &
              Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & To_String(Orientation));
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button,
            "-side " & To_String(Side) & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      for I in 1 .. 2 loop
         Button.Name :=
           New_String
             (".mainframe.toolbars.itemtoolbar.separator" &
              Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & To_String(Orientation));
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button,
            "-side " & To_String(Side) & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(".mainframe.toolbars.itemtoolbar");
      if not Settings.ToolbarsOnTop then
         Grid_Configure(Toolbar, "-column 0 -row 2 -sticky s");
      else
         Grid_Configure(Toolbar, "-column 2 -row 0 -sticky e");
      end if;
      MainFrame.Interp := Get_Context;
      MainFrame.Name := New_String(".mainframe");
      Toolbar.Name := New_String(".mainframe.toolbars");
      if not Settings.ToolbarsOnTop then
         Grid_Configure(Toolbar, "-sticky ns -row 3 -column 0 -columnspan 1");
         Column_Configure(MainFrame, Toolbar, "-weight 0");
         Row_Configure(MainFrame, Toolbar, "-weight 0");
      else
         Grid_Configure(Toolbar, "-sticky we -row 0 -columnspan 2");
      end if;
      Label.Interp := Get_Context;
      Label.Name := New_String(".mainframe.toolbars.label");
      Toolbar.Name := New_String(".mainframe.toolbars");
      if not Settings.ToolbarsOnTop then
         Column_Configure(Toolbar, Label, "-weight 0");
         Row_Configure(Toolbar, Label, "-weight 1");
      else
         Column_Configure(Toolbar, Label, "-weight 1");
         Row_Configure(Toolbar, Label, "-weight 0");
      end if;
      if CurrentSelected /= Null_Unbounded_String then
         SetActionsButtons;
         SetBookmarkButton;
      end if;
   end SetToolbars;

   -- ****if* Toolbars/SetButton
   -- FUNCTION
   -- Configure selected button on toolbars
   -- PARAMETERS
   -- Button      - Button to configure
   -- TooltipText - Text which will be displayed as tooltip
   -- ImageName   - Name of image which will be used as icon for button
   -- SOURCE
   procedure SetButton
     (Button: Tk_Widget'Class; TooltipText, ImageName: String) is
      Image: constant Tk_Photo :=
        Create
          (ImageName & "icon",
           "-file {../share/hunter/images/" & ImageName &
           ".svg} -format {svg -scaletoheight" &
           Natural'Image(Settings.ToolbarsSize) & "}");
      pragma Unreferenced(Image);
      -- ****
   begin
      Add(Button, TooltipText);
      configure
        (Button,
         "-style Toolbutton -image " & ImageName & "icon -takefocus 0");
   end SetButton;

   procedure CreateActionToolbar is
      ToolMenuButton: Ttk_MenuButton;
      ToolbarsFrame: constant Ttk_Frame := Create(".mainframe.toolbars");
      Toolbar: constant Ttk_Frame :=
        Create(".mainframe.toolbars.actiontoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
      Label: constant Ttk_Label := Create(".mainframe.toolbars.label");
      ButtonMenu: Tk_Menu;
      ToolCheckButton: Ttk_CheckButton;
   begin
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.quitbutton", "-command exit");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Quit from the program.}") & " \[" &
         To_String(Accelerators(1)) & "\]",
         "quit");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.bookmarksbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show bookmarks menu }") & " \[" &
         To_String(Accelerators(2)) & "\]",
         "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolCheckButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.searchbutton",
           "-command ToggleSearch");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Search for the file or directory }") & " \[" &
         To_String(Accelerators(3)) & "\]",
         "edit-find");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.selectbutton",
           "-command ToggleSelection");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Select or unselect all files and directories.}") &
         " \[" & To_String(Accelerators(6)) & "\]",
         "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(".mainframe.toolbars.actiontoolbar.userbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show user actions menu }") & " \[" &
         To_String(Accelerators(4)) & "\]",
         "run-build");
      ButtonMenu := Create(".actionsmenu", "-tearoff false");
      configure
        (ToolMenuButton,
         "-menu " & Widget_Image(ButtonMenu) & " -direction right");
      SetUserCommandsMenu;
      ToolMenuButton := Create(".mainframe.toolbars.actiontoolbar.newbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show add new item menu }") & " \[" &
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
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      ToolCheckButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.renamebutton",
           "-command ToggleRename");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Rename selected file or directory }") & " \[" &
         To_String(Accelerators(9)) & "\]",
         "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.copybutton",
           "-command CopyData");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Copy selected files }") & " \[" &
         To_String(Accelerators(10)) &
         "\]. Pressed button means start copying\ncurrently selected files or directories.\nPress again to copy them.",
         "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.movebutton",
           "-command MoveData");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Move selected files }") & " \[" &
         To_String(Accelerators(11)) &
         "\]. Pressed button means start moving\ncurrently selected files or directories.\nPress again to move them.",
         "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.deletebutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show delete menu }") & " \[" &
         To_String(Accelerators(5)) & "\]",
         "edit-delete");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".deletemenu", "-tearoff false");
      if Settings.DeleteFiles then
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
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.cancelbutton",
           "-command CancelAction");
      SetButton
        (ToolButton,
         Mc
           (Get_Context,
            "{Discard all changes and back to files list \[Escape\]}"),
         "dialog-cancel");
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.restorebutton",
           "-command RestoreItems");
      SetButton
        (ToolButton,
         Mc
           (Get_Context,
            "{Restore selected file or directory from the trash }") &
         " \[" & To_String(Accelerators(19)) & "\]",
         "document-revert");
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator3");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.optionsbutton",
           "-command ShowPreferences");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Show the program preferences }") & " \[" &
         To_String(Accelerators(12)) & "\]",
         "configure");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.aboutbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show menu with information about the program }") &
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
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      Tcl.Tk.Ada.Grid.Grid(Toolbar, "-sticky w");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Grid(ToolbarsFrame);
   end CreateActionToolbar;

   procedure CreateItemToolbar is
      Toolbar: constant Ttk_Frame := Create(".mainframe.toolbars.itemtoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
      ToolRadioButton: Ttk_RadioButton;
   begin
      ToolButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.runbutton", "-command Execute");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Execute selected program }") & " \[" &
         To_String(Accelerators(18)) & "\]",
         "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.openbutton",
           "-command ActivateItem");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory }") & " \[" &
         To_String(Accelerators(7)) & "\]",
         "document-open");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.openwithbutton",
           "-command ToggleExecuteWith");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory with command }") &
         " \[" & To_String(Accelerators(13)) & "\]",
         "system-run");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.itemtoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolRadioButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.previewbutton",
           "-variable previewtype -value preview -command ShowPreviewOrInfo");
      SetButton
        (ToolRadioButton,
         Mc(Get_Context, "{Preview file or directory }") & " \[" &
         To_String(Accelerators(15)) & "\]",
         "document-preview");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      ToolRadioButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.infobutton",
           "-variable previewtype -value info -command ShowPreviewOrInfo");
      SetButton
        (ToolRadioButton,
         Mc(Get_Context, "{File or directory information }") & " \[" &
         To_String(Accelerators(14)) & "\]",
         "document-properties");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      Separator := Create(".mainframe.toolbars.itemtoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.addbutton",
           "-command AddBookmark");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Add bookmark to this directory }") & " \[" &
         To_String(Accelerators(16)) & "\]",
         "list-add");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create
          (".mainframe.toolbars.itemtoolbar.deletebutton",
           "-command RemoveBookmark");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Remove bookmark from this directory }") & " \[" &
         To_String(Accelerators(17)) & "\]",
         "list-remove");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Tcl.Tk.Ada.Grid.Grid(Toolbar);
   end CreateItemToolbar;

   procedure SetActionsButtons is
      Button: Ttk_Button;
   begin
      Button.Interp := Get_Context;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.runbutton");
      if Is_Executable_File(To_String(CurrentSelected)) then
         if Winfo_Get(Button, "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Button,
               "-before .mainframe.toolbars.itemtoolbar.openwithbutton");
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.openbutton");
      if CanBeOpened(GetMimeType(To_String(CurrentSelected))) then
         if Winfo_Get(Button, "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Button,
               "-before .mainframe.toolbars.itemtoolbar.openwithbutton");
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
   end SetActionsButtons;

   procedure SetUserCommandsMenu is
      ActionsMenu: Tk_Menu;
      ActionsButton: Ttk_MenuButton;
   begin
      ActionsButton.Interp := Get_Context;
      ActionsButton.Name :=
        New_String(".mainframe.toolbars.actiontoolbar.userbutton");
      ActionsMenu.Interp := Get_Context;
      ActionsMenu.Name := New_String(".actionsmenu");
      Delete(ActionsMenu, "0", "end");
      if UserCommands.Is_Empty then
         Tcl.Tk.Ada.Pack.Pack_Forget(ActionsButton);
      else
         Tcl.Tk.Ada.Pack.Pack
           (ActionsButton,
            "-after .mainframe.toolbars.actiontoolbar.separator2");
      end if;
      for I in UserCommands.Iterate loop
         Menu.Add
           (ActionsMenu, "command",
            "-label {" & Commands_Container.Key(I) & "}");
      end loop;
   end SetUserCommandsMenu;

end Toolbars;
