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

--with Bookmarks; use Bookmarks;
--with MainWindow; use MainWindow;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkSeparator; use Tcl.Tk.Ada.Widgets.TtkSeparator;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Preferences; use Preferences;

package body Toolbars is

   procedure SetToolbars is
      Side, Direction, Orientation, PanelSide: Unbounded_String;
      Fill, Anchor: String(1 .. 1);
      Toolbar: Ttk_Frame;
      Button: Ttk_Button;
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String(".actiontoolbar.quitbutton"),
         To_Unbounded_String(".actiontoolbar.searchbutton"),
         To_Unbounded_String(".actiontoolbar.selectbutton"),
         To_Unbounded_String(".actiontoolbar.renamebutton"),
         To_Unbounded_String(".actiontoolbar.copybutton"),
         To_Unbounded_String(".actiontoolbar.movebutton"),
         To_Unbounded_String(".actiontoolbar.optionsbutton"),
         To_Unbounded_String(".itemtoolbar.runbutton"),
         To_Unbounded_String(".itemtoolbar.openbutton"),
         To_Unbounded_String(".itemtoolbar.openwithbutton"),
         To_Unbounded_String(".itemtoolbar.previewbutton"),
         To_Unbounded_String(".itemtoolbar.infobutton"),
         To_Unbounded_String(".itemtoolbar.addbutton"),
         To_Unbounded_String(".itemtoolbar.deletebutton"));
      MenuButtonsNames: constant array
        (Positive range <>) of Unbounded_String :=
        (To_Unbounded_String(".actiontoolbar.bookmarksbutton"),
         To_Unbounded_String(".actiontoolbar.newbutton"),
         To_Unbounded_String(".actiontoolbar.deletebutton"),
         To_Unbounded_String(".actiontoolbar.aboutbutton"));
   begin
      if not Settings.ToolbarsOnTop then
         Side := To_Unbounded_String("top");
         Fill := "x";
         Direction := To_Unbounded_String("right");
         Orientation := To_Unbounded_String("horizontal");
         PanelSide := To_Unbounded_String("left");
         Anchor := "n";
      else
         Side := To_Unbounded_String("left");
         Fill := "y";
         Direction := To_Unbounded_String("below");
         Orientation := To_Unbounded_String("vertical");
         PanelSide := To_Unbounded_String("top");
         Anchor := "w";
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
             (".actiontoolbar.separator" & Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & To_String(Orientation));
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button,
            "-side " & To_String(Side) & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      for I in 1 .. 2 loop
         Button.Name :=
           New_String
             (".itemtoolbar.separator" & Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & To_String(Orientation));
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button,
            "-side " & To_String(Side) & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(".actiontoolbar");
      Tcl.Tk.Ada.Pack.Pack_Configure
        (Toolbar, "-side " & To_String(PanelSide) & " -anchor " & Anchor);
      Toolbar.Name := New_String(".itemtoolbar");
      if not Settings.ToolbarsOnTop then
         Anchor := "s";
      else
         Anchor := "e";
      end if;
      Tcl.Tk.Ada.Pack.Pack_Configure
        (Toolbar, "-side " & To_String(PanelSide) & " -anchor " & Anchor);
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
      Toolbar: Ttk_Frame;
      CurrentDir: constant String := Current_Directory;
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
   begin
      Create(Toolbar, ".actiontoolbar");
      Set_Directory(Containing_Directory(Command_Name));
      ToolButton := Create(".actiontoolbar.quitbutton");
      SetButton(ToolButton, "Quit from the program. \[CTRL+Q\]", "quit");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".actiontoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(".actiontoolbar.bookmarksbutton");
      SetButton(ToolMenuButton, "Show bookmarks menu \[ALT+H\]", "bookmarks");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolButton := Create(".actiontoolbar.searchbutton");
      SetButton
        (ToolButton, "Search for the file or directory \[ALT+F\]",
         "edit-find");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".actiontoolbar.selectbutton");
      SetButton
        (ToolButton,
         "Select or unselect all files and directories in currently selected directory. \[CTRL+A\]",
         "edit-select-all");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".actiontoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolMenuButton := Create(".actiontoolbar.newbutton");
      SetButton
        (ToolMenuButton, "Show add new item menu \[ALT+N\].", "document-new");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolButton := Create(".actiontoolbar.renamebutton");
      SetButton
        (ToolButton, "Rename selected file or directory \[CTRL+R\]",
         "document-save-as");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".actiontoolbar.copybutton");
      SetButton
        (ToolButton,
         "Copy selected files \[ALT+C\]. Pressed button means start copying currently selected files or directories. Press again to copy them.",
         "edit-copy");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".actiontoolbar.movebutton");
      SetButton
        (ToolButton,
         "Move selected files \[ALT+M\]. Pressed button means start moving currently selected files or directories. Press again to move them.",
         "edit-cut");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton := Create(".actiontoolbar.deletebutton");
      SetButton(ToolMenuButton, "Show delete menu \[Delete\]", "edit-delete");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      ToolButton := Create(".actiontoolbar.cancelbutton");
      SetButton
        (ToolButton, "Discard all changes and back to files list \[Escape\]",
         "dialog-cancel");
      ToolButton := Create(".actiontoolbar.restorebutton");
      SetButton
        (ToolButton,
         "Restore selected file or directory from the trash \[ALT+R\]",
         "document-revert");
      Separator := Create(".actiontoolbar.separator3");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".actiontoolbar.optionsbutton");
      SetButton
        (ToolButton, "Show the program preferences \[ALT+P\]", "configure");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolMenuButton := Create(".actiontoolbar.aboutbutton");
      SetButton
        (ToolMenuButton,
         "Show menu with information about the program \[ALT+A\]",
         "help-about");
      Tcl.Tk.Ada.Pack.Pack(ToolMenuButton);
      Tcl.Tk.Ada.Pack.Pack(Toolbar);
      Set_Directory(CurrentDir);
   end CreateActionToolbar;

   procedure CreateItemToolbar is
      Toolbar: Ttk_Frame;
      CurrentDir: constant String := Current_Directory;
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
   begin
      Create(Toolbar, ".itemtoolbar");
      Set_Directory(Containing_Directory(Command_Name));
      ToolButton := Create(".itemtoolbar.runbutton");
      SetButton
        (ToolButton, "Execute selected program \[ALT+E\].",
         "media-playback-start");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".itemtoolbar.openbutton");
      SetButton
        (ToolButton, "Open selected file or directory \[ALT+O\]",
         "document-open");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".itemtoolbar.openwithbutton");
      SetButton
        (ToolButton, "Open selected file or directory with command \[ALT+W\]",
         "system-run");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".itemtoolbar.separator1");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".itemtoolbar.previewbutton");
      SetButton
        (ToolButton, "Preview file or directory \[ALT+V\]",
         "document-preview");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".itemtoolbar.infobutton");
      SetButton
        (ToolButton, "File or directory informations \[ALT+I\]",
         "document-properties");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Separator := Create(".itemtoolbar.separator2");
      Tcl.Tk.Ada.Pack.Pack(Separator);
      ToolButton := Create(".itemtoolbar.addbutton");
      SetButton
        (ToolButton, "Add bookmark to this directory \[ALT+B\].", "list-add");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      ToolButton := Create(".itemtoolbar.deletebutton");
      SetButton
        (ToolButton, "Remove bookmark to this directory \[ALT+B\]",
         "list-remove");
      Tcl.Tk.Ada.Pack.Pack(ToolButton);
      Tcl.Tk.Ada.Pack.Pack(Toolbar);
      Set_Directory(CurrentDir);
   end CreateItemToolbar;

end Toolbars;
