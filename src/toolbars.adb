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
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Preferences; use Preferences;

package body Toolbars is

   procedure SetToolbars is
      Side, Direction, Orientation: Unbounded_String;
      Fill: String(1 .. 1);
      Toolbar: Ttk_Frame;
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
      Toolbar.Name := New_String(".mainframe.toolbars");
      if not Settings.ToolbarsOnTop then
         Grid_Configure(Toolbar, "-sticky ns -row 3");
      else
         Grid_Configure(Toolbar, "-sticky we -row 0");
      end if;
      Label.Interp := Get_Context;
      Label.Name := New_String(".mainframe.toolbars.label");
      if not Settings.ToolbarsOnTop then
         Column_Configure(Toolbar, Label, "-weight 0");
         Row_Configure(Toolbar, Label, "-weight 1");
      else
         Column_Configure(Toolbar, Label, "-weight 1");
         Row_Configure(Toolbar, Label, "-weight 0");
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
           "-file ""../share/hunter/images/" & ImageName & ".png""");
      pragma Unreferenced(Image);
   begin
      Add(Button, TooltipText);
      configure(Button, "-style Toolbutton -image " & ImageName & "icon");
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
   begin
      ToolButton :=
        Create
          (".mainframe.toolbars.actiontoolbar.quitbutton", "-command exit");
      SetButton(ToolButton, "Quit from the program. \[CTRL+Q\]", "quit");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.bookmarksbutton");
      SetButton(ToolMenuButton, "Show bookmarks menu \[ALT+H\]", "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.searchbutton");
      SetButton
        (ToolButton, "Search for the file or directory \[ALT+F\]",
         "edit-find");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.selectbutton");
      SetButton
        (ToolButton,
         "Select or unselect all files and directories in currently selected directory. \[CTRL+A\]",
         "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(".mainframe.toolbars.actiontoolbar.newbutton");
      SetButton
        (ToolMenuButton, "Show add new item menu \[ALT+N\].", "document-new");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".newmenu", "-tearoff false");
      Menu.Add(ButtonMenu, "command", "-label ""New directory""");
      Menu.Add(ButtonMenu, "command", "-label ""New file""");
      Menu.Add(ButtonMenu, "command", "-label ""New link""");
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.renamebutton");
      SetButton
        (ToolButton, "Rename selected file or directory \[CTRL+R\]",
         "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.copybutton");
      SetButton
        (ToolButton,
         "Copy selected files \[ALT+C\]. Pressed button means start copying currently selected files or directories. Press again to copy them.",
         "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.movebutton");
      SetButton
        (ToolButton,
         "Move selected files \[ALT+M\]. Pressed button means start moving currently selected files or directories. Press again to move them.",
         "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.deletebutton");
      SetButton(ToolMenuButton, "Show delete menu \[Delete\]", "edit-delete");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".deletemenu", "-tearoff false");
      if Settings.DeleteFiles then
         Menu.Add(ButtonMenu, "command", "-label ""Delete selected""");
      else
         Menu.Add(ButtonMenu, "command", "-label ""Move selected to Trash""");
      end if;
      Menu.Add(ButtonMenu, "command", "-label ""Show Trash""");
      Menu.Add(ButtonMenu, "command", "-label ""Empty Trash""");
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.cancelbutton");
      SetButton
        (ToolButton, "Discard all changes and back to files list \[Escape\]",
         "dialog-cancel");
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.restorebutton");
      SetButton
        (ToolButton,
         "Restore selected file or directory from the trash \[ALT+R\]",
         "document-revert");
      Separator := Create(".mainframe.toolbars.actiontoolbar.separator3");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".mainframe.toolbars.actiontoolbar.optionsbutton");
      SetButton
        (ToolButton, "Show the program preferences \[ALT+P\]", "configure");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton :=
        Create(".mainframe.toolbars.actiontoolbar.aboutbutton");
      SetButton
        (ToolMenuButton,
         "Show menu with information about the program \[ALT+A\]",
         "help-about");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ButtonMenu := Create(".aboutmenu", "-tearoff false");
      Menu.Add(ButtonMenu, "command", "-label ""About the program""");
      Menu.Add(ButtonMenu, "command", "-label ""Show README""");
      Menu.Add(ButtonMenu, "command", "-label ""Show list of changes""");
      Menu.Add(ButtonMenu, "command", "-label ""Get involved""");
      configure(ToolMenuButton, "-menu " & Widget_Image(ButtonMenu));
      Tcl.Tk.Ada.Grid.Grid(Toolbar, "-sticky w");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Grid(ToolbarsFrame);
   end CreateActionToolbar;

   procedure CreateItemToolbar is
      Toolbar: constant Ttk_Frame := Create(".mainframe.toolbars.itemtoolbar");
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
   begin
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.runbutton");
      SetButton
        (ToolButton, "Execute selected program \[ALT+E\].",
         "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.openbutton");
      SetButton
        (ToolButton, "Open selected file or directory \[ALT+O\]",
         "document-open");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.openwithbutton");
      SetButton
        (ToolButton, "Open selected file or directory with command \[ALT+W\]",
         "system-run");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.itemtoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.previewbutton");
      SetButton
        (ToolButton, "Preview file or directory \[ALT+V\]",
         "document-preview");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.infobutton");
      SetButton
        (ToolButton, "File or directory informations \[ALT+I\]",
         "document-properties");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".mainframe.toolbars.itemtoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.addbutton");
      SetButton
        (ToolButton, "Add bookmark to this directory \[ALT+B\].", "list-add");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".mainframe.toolbars.itemtoolbar.deletebutton");
      SetButton
        (ToolButton, "Remove bookmark to this directory \[ALT+B\]",
         "list-remove");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Tcl.Tk.Ada.Grid.Grid(Toolbar);
   end CreateItemToolbar;

end Toolbars;
