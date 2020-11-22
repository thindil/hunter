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
with Bookmarks; use Bookmarks;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with UserCommands; use UserCommands;
with Utils; use Utils;

package body Toolbars is

   procedure SetToolbars is
      Fill: constant Character :=
        (if Settings.ToolbarsOnTop then 'y' else 'x');
      MainFrame: constant Ttk_Frame := Get_Widget(".mainframe");
      Toolbar: Ttk_Frame;
      Button: Ttk_Button;
      Label: Ttk_Label;
      Tokens: Slice_Set;
      Side: constant String :=
        (if Settings.ToolbarsOnTop then "left" else "top");
      Direction: constant String :=
        (if Settings.ToolbarsOnTop then "below" else "right");
      Orientation: constant String :=
        (if Settings.ToolbarsOnTop then "vertical" else "horizontal");
   begin
      Button.Interp := Get_Context;
      Toolbar := Get_Widget(MainFrame & ".toolbars.actiontoolbar");
      Create(Tokens, Tcl.Tk.Ada.Pack.Pack_Slaves(Toolbar), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Button.Name := New_String(Slice(Tokens, I));
         if Winfo_Get(Button, "class") = "TMenubutton" then
            configure(Button, "-direction " & Direction);
         end if;
         Tcl.Tk.Ada.Pack.Pack_Configure(Button, "-side " & Side);
      end loop;
      Toolbar := Get_Widget(MainFrame & ".toolbars.itemtoolbar");
      Create(Tokens, Tcl.Tk.Ada.Pack.Pack_Slaves(Toolbar), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Button.Name := New_String(Slice(Tokens, I));
         if Winfo_Get(Button, "class") = "TMenubutton" then
            configure(Button, "-direction " & Direction);
         end if;
         Tcl.Tk.Ada.Pack.Pack_Configure(Button, "-side " & Side);
      end loop;
      for I in 1 .. 3 loop
         Button.Name :=
           New_String
             (MainFrame & ".toolbars.actiontoolbar.separator" &
              Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & Orientation);
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button, "-side " & Side & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      for I in 1 .. 2 loop
         Button.Name :=
           New_String
             (MainFrame & ".toolbars.itemtoolbar.separator" &
              Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & Orientation);
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button, "-side " & Side & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(MainFrame & ".toolbars.itemtoolbar");
      if not Settings.ToolbarsOnTop then
         Grid_Configure(Toolbar, "-column 0 -row 2 -sticky s");
      else
         Grid_Configure(Toolbar, "-column 2 -row 0 -sticky e");
      end if;
      Toolbar.Name := New_String(MainFrame & ".toolbars");
      if not Settings.ToolbarsOnTop then
         Grid_Configure(Toolbar, "-sticky ns -row 3 -column 0 -columnspan 1");
         Column_Configure(MainFrame, Toolbar, "-weight 0");
         Row_Configure(MainFrame, Toolbar, "-weight 0");
      else
         Grid_Configure(Toolbar, "-sticky we -row 0 -columnspan 2");
      end if;
      Label.Interp := Get_Context;
      Label.Name := New_String(MainFrame & ".toolbars.label");
      Toolbar.Name := New_String(MainFrame & ".toolbars");
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

   -- ****if* Toolbars/Toolbars.SetButton
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
      Toolbar: constant Ttk_Frame := Create(ToolbarsFrame & ".actiontoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
      Label: constant Ttk_Label := Create(ToolbarsFrame & ".label");
      ButtonMenu: Tk_Menu;
      ToolCheckButton: Ttk_CheckButton;
   begin
      ToolButton := Create(Toolbar & ".quitbutton", "-command exit");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Quit from the program.}") & " \[" &
         To_String(Accelerators(1)) & "\]",
         "quit");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(Toolbar & ".bookmarksbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show bookmarks menu }") & " \[" &
         To_String(Accelerators(2)) & "\]",
         "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolCheckButton :=
        Create(Toolbar & ".searchbutton", "-command ToggleSearch");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Search for the file or directory }") & " \[" &
         To_String(Accelerators(3)) & "\]",
         "edit-find");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolButton :=
        Create(Toolbar & ".selectbutton", "-command ToggleSelection");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Select or unselect all files and directories.}") &
         " \[" & To_String(Accelerators(8)) & "\]",
         "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(Toolbar & ".userbutton");
      SetButton
        (ToolMenuButton,
         Mc(Get_Context, "{Show user actions menu }") & " \[" &
         To_String(Accelerators(20)) & "\]",
         "run-build");
      ButtonMenu := Create(".actionsmenu", "-tearoff false");
      configure(ToolMenuButton, "-menu " & ButtonMenu & " -direction right");
      SetUserCommandsMenu;
      ToolMenuButton := Create(Toolbar & ".newbutton");
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
      configure(ToolMenuButton, "-menu " & ButtonMenu);
      ToolCheckButton :=
        Create(Toolbar & ".renamebutton", "-command ToggleRename");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Rename selected file or directory }") & " \[" &
         To_String(Accelerators(9)) & "\]",
         "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton := Create(Toolbar & ".copybutton", "-command CopyData");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Copy selected files }") & " \[" &
         To_String(Accelerators(10)) &
         "\]. Pressed button means start copying\ncurrently selected files or directories.\nPress again to copy them.",
         "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolCheckButton := Create(Toolbar & ".movebutton", "-command MoveData");
      SetButton
        (ToolCheckButton,
         Mc(Get_Context, "{Move selected files }") & " \[" &
         To_String(Accelerators(11)) &
         "\]. Pressed button means start moving\ncurrently selected files or directories.\nPress again to move them.",
         "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(ToolCheckButton);
      ToolMenuButton := Create(Toolbar & ".deletebutton");
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
      configure(ToolMenuButton, "-menu " & ButtonMenu);
      ToolButton := Create(Toolbar & ".cancelbutton", "-command CancelAction");
      SetButton
        (ToolButton,
         Mc
           (Get_Context,
            "{Discard all changes and back to files list \[Escape\]}"),
         "dialog-cancel");
      ToolButton :=
        Create(Toolbar & ".restorebutton", "-command RestoreItems");
      SetButton
        (ToolButton,
         Mc
           (Get_Context,
            "{Restore selected file or directory from the trash }") &
         " \[" & To_String(Accelerators(19)) & "\]",
         "document-revert");
      Separator := Create(Toolbar & ".separator3");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton :=
        Create(Toolbar & ".optionsbutton", "-command ShowPreferences");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Show the program preferences }") & " \[" &
         To_String(Accelerators(12)) & "\]",
         "configure");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton := Create(Toolbar & ".aboutbutton");
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
      Menu.Add
        (ButtonMenu, "command",
         "-label {" & Mc(Get_Context, "{Show modding guide}") &
         "} -command {ShowFile MODDING.md}");
      configure(ToolMenuButton, "-menu " & ButtonMenu);
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
      ToolButton := Create(Toolbar & ".runbutton", "-command Execute");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Execute selected program }") & " \[" &
         To_String(Accelerators(18)) & "\]",
         "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(Toolbar & ".openbutton", "-command ActivateItem");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory }") & " \[" &
         To_String(Accelerators(7)) & "\]",
         "document-open");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create(Toolbar & ".openwithbutton", "-command ToggleExecuteWith");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Open selected file or directory with command }") &
         " \[" & To_String(Accelerators(13)) & "\]",
         "system-run");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(Toolbar & ".separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolRadioButton :=
        Create
          (Toolbar & ".previewbutton",
           "-variable previewtype -value preview -command ShowPreviewOrInfo");
      SetButton
        (ToolRadioButton,
         Mc(Get_Context, "{Preview file or directory }") & " \[" &
         To_String(Accelerators(15)) & "\]",
         "document-preview");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      ToolRadioButton :=
        Create
          (Toolbar & ".infobutton",
           "-variable previewtype -value info -command ShowPreviewOrInfo");
      SetButton
        (ToolRadioButton,
         Mc(Get_Context, "{File or directory information }") & " \[" &
         To_String(Accelerators(14)) & "\]",
         "document-properties");
      Tcl.Tk.Ada.Pack.Pack(ToolRadioButton);
      Separator := Create(Toolbar & ".separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(Toolbar & ".addbutton", "-command AddBookmark");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Add bookmark to this directory }") & " \[" &
         To_String(Accelerators(16)) & "\]",
         "list-add");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton :=
        Create(Toolbar & ".deletebutton", "-command RemoveBookmark");
      SetButton
        (ToolButton,
         Mc(Get_Context, "{Remove bookmark from this directory }") & " \[" &
         To_String(Accelerators(17)) & "\]",
         "list-remove");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Tcl.Tk.Ada.Grid.Grid(Toolbar);
   end CreateItemToolbar;

   procedure SetActionsButtons is
      Button: Ttk_Button :=
        Get_Widget(".mainframe.toolbars.itemtoolbar.runbutton");
      Side: constant String :=
        (if Settings.ToolbarsOnTop then "left" else "top");
   begin
      if Is_Executable_File(To_String(CurrentSelected)) then
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
      if CanBeOpened(GetMimeType(To_String(CurrentSelected))) then
         if Winfo_Get(Button, "ismapped") = "0" then
            Tcl.Tk.Ada.Pack.Pack
              (Button,
               "-before .mainframe.toolbars.itemtoolbar.openwithbutton -side " &
               Side);
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
   end SetActionsButtons;

   procedure SetUserCommandsMenu is
      ActionsMenu: constant Tk_Menu := Get_Widget(".actionsmenu");
      ActionsButton: constant Ttk_MenuButton :=
        Get_Widget(".mainframe.toolbars.actiontoolbar.userbutton");
      Side: constant String :=
        (if Settings.ToolbarsOnTop then "left" else "top");
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
      for I in UserCommandsList.Iterate loop
         Menu.Add
           (ActionsMenu, "command",
            "-label {" & Commands_Container.Key(I) &
            "} -command {ExecuteCommand {" & Commands_Container.Key(I) & "}}");
      end loop;
   end SetUserCommandsMenu;

end Toolbars;
