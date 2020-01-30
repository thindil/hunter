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
      Side, Direction, Orientation: Unbounded_String;
      Fill: String(1 .. 1);
      Toolbar: Ttk_Frame;
      Button: Ttk_Button;
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String(".actiontoolbar.searchbutton"),
         To_Unbounded_String(".actiontoolbar.renamebutton"));
      MenuButtonsNames: constant array
        (Positive range <>) of Unbounded_String :=
        (To_Unbounded_String(".actiontoolbar.bookmarksbutton"),
         To_Unbounded_String(".actiontoolbar.newbutton"));
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
      for I in 1 .. 1 loop
         Button.Name :=
           New_String
             (".actiontoolbar.separator" & Trim(Positive'Image(I), Both));
         configure(Button, "-orient " & To_String(Orientation));
         Tcl.Tk.Ada.Pack.Pack_Configure
           (Button,
            "-side " & To_String(Side) & " -pad" & Fill & " 5 -fill " & Fill);
      end loop;
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(".actiontoolbar");
      Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill " & Fill);
   end SetToolbars;

   procedure CreateActionToolbar is
      ToolMenuButton: Ttk_MenuButton;
      Toolbar: Ttk_Frame;
      CurrentDir: constant String := Current_Directory;
      ToolButton: Ttk_Button;
      Separator: Ttk_Separator;
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
   begin
      Create(Toolbar, ".actiontoolbar");
      Set_Directory(Containing_Directory(Command_Name));
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
      Separator := Create(".actiontoolbar.separator1");
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
      Set_Directory(CurrentDir);
      SetToolbars;
   end CreateActionToolbar;

   -- ****if* Toolbars/AddButton
   -- FUNCTION
   -- Add button to the toolbar
   -- PARAMETERS
   -- Text     - Label of the button to add
   -- IconName - Name of icon which will be showed on the button
   -- Toolbar  - Toolbar to which the button will be added
   -- Tooltip  - Tooltip text for the button
   -- Key      - Keyboard shortcut for the button
   -- Mask     - Special key (Control, Alt, Shift, etc) which
   --            will be needed for trigger the button
   -- SOURCE
--   procedure AddButton
--     (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
--      Key: Gdk_Key_Type; Mask: Gdk_Modifier_Type := Mod1_Mask) is
--      -- ****
--      Button: constant Gtk_Tool_Button := Gtk_Tool_Button_New(Label => Text);
--   begin
--      Set_Tooltip_Text(Button, Tooltip);
--      Set_Icon_Name(Button, IconName);
--      Add_Accelerator
--        (Button, "clicked", Accelerators, Key, Mask, Accel_Visible);
--      Insert(Toolbar, Button);
--   end AddButton;
--
--   -- ****if* Toolbars/AddSeparator
--   -- FUNCTION
--   -- Add separator to toolbar
--   -- PARAMETERS
--   -- Toolbar - Toolbar to which the separator will be added
--   -- SOURCE
--   procedure AddSeparator(Toolbar: Gtk_Toolbar) is
--      -- ****
--      Separator: constant Gtk_Separator_Tool_Item :=
--        Gtk_Separator_Tool_Item_New;
--   begin
--      Insert(Toolbar, Separator);
--   end AddSeparator;
--
--   -- ****if* Toolbars/AddRadioButton
--   -- FUNCTION
--   -- Add radiobutton to the toolbar
--   -- PARAMETERS
--   -- Text       - Label of the radiobutton to add
--   -- IconName   - Name of icon which will be showed on the radiobutton
--   -- RadioGroup - The group to which the radiobutton will be belong
--   -- Toolbar    - The toolbar to which the button will be added
--   -- Tooltip    - Tooltip text for the radiobutton
--   -- Key        - Keyboard shortcut for the radiobutton
--   -- SOURCE
--   procedure AddRadioButton
--     (Text, IconName: String; RadioGroup: in out Widget_SList.GSlist;
--      Toolbar: Gtk_Toolbar; Tooltip: String; Key: Gdk_Key_Type) is
--      -- ****
--      Button: constant Gtk_Radio_Tool_Button :=
--        Gtk_Radio_Tool_Button_New(RadioGroup);
--   begin
--      RadioGroup := Get_Group(Button);
--      Set_Label(Button, Text);
--      Set_Tooltip_Text(Button, Tooltip);
--      Set_Icon_Name(Button, IconName);
--      Add_Accelerator
--        (Button, "clicked", Accelerators, Key, Mod1_Mask, Accel_Visible);
--      Insert(Toolbar, Button);
--   end AddRadioButton;
--
--   procedure CreateItemToolbarUI is
--      RadioGroup: Widget_SList.GSlist;
--   begin
--      ItemToolBar := Gtk_Toolbar_New;
--      Set_Style(ItemToolBar, Toolbar_Icons);
--      Set_Halign(ItemToolBar, Align_Center);
--      Set_Valign(ItemToolBar, Align_End);
--      AddButton
--        (Gettext("Run"), "media-playback-start", ItemToolBar,
--         Gettext("Execute selected program [ALT-E]."), GDK_E);
--      AddButton
--        (Gettext("Open"), "document-open", ItemToolBar,
--         Gettext("Open selected file or directory [ALT-O]"), GDK_O);
--      AddButton
--        (Gettext("Open with..."), "system-run", ItemToolBar,
--         Gettext("Open selected file or directory with command [ALT-W]"),
--         GDK_W);
--      AddSeparator(ItemToolBar);
--      AddRadioButton
--        (Gettext("Preview"), "document-print-preview", RadioGroup, ItemToolBar,
--         Gettext("Preview file or directory [ALT-V]"), GDK_V);
--      AddRadioButton
--        (Gettext("Info"), "document-properties", RadioGroup, ItemToolBar,
--         Gettext("File or directory informations [ALT-I]"), GDK_I);
--      AddSeparator(ItemToolBar);
--      AddButton
--        (Gettext("Add bookmark"), "list-add", ItemToolBar,
--         Gettext("Add bookmark to this directory [ALT-B]."), GDK_B);
--      AddButton
--        (Gettext("Remove bookmark"), "list-remove", ItemToolBar,
--         Gettext("Remove bookmark to this directory [ALT-B]"), GDK_B);
--      Pack_End
--        (Gtk_Header_Bar(Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
--         ItemToolBar);
--   end CreateItemToolbarUI;
--
--   procedure CreateActionToolbarUI is
--      procedure AddMenuButton
--        (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
--         Key: Gdk_Key_Type; Menu: Gtk_Widget) is
--         Button: constant Gtk_Menu_Tool_Button :=
--           Gtk_Menu_Tool_Button_New(Label => Text);
--      begin
--         Set_Tooltip_Text(Button, Tooltip);
--         Set_Icon_Name(Button, IconName);
--         Add_Accelerator
--           (Button, "clicked", Accelerators, Key, Mod1_Mask, Accel_Visible);
--         if Menu /= null then
--            Set_Menu(Button, Menu);
--         end if;
--         Insert(Toolbar, Button);
--      end AddMenuButton;
--      procedure AddToggleButton
--        (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
--         Key: Gdk_Key_Type; Mask: Gdk_Modifier_Type := Mod1_Mask) is
--         Button: constant Gtk_Toggle_Tool_Button := Gtk_Toggle_Tool_Button_New;
--      begin
--         Set_Label(Button, Text);
--         Set_Tooltip_Text(Button, Tooltip);
--         Set_Icon_Name(Button, IconName);
--         Add_Accelerator
--           (Button, "clicked", Accelerators, Key, Mask, Accel_Visible);
--         Insert(Toolbar, Button);
--      end AddToggleButton;
--   begin
--      ActionToolBar := Gtk_Toolbar_New;
--      Set_Style(ActionToolBar, Toolbar_Icons);
--      CreateBookmarkMenu(True);
--      AddToggleButton
--        (Gettext("Copy"), "edit-copy", ActionToolBar,
--         Gettext
--           ("Copy selected files [ALT-C]. Pressed button means start copying currently selected files or directories. Press again to copy them."),
--         GDK_C);
--      AddToggleButton
--        (Gettext("Move"), "edit-cut", ActionToolBar,
--         Gettext
--           ("Move selected files [ALT-M]. Pressed button means start moving currently selected files or directories. Press again to move them."),
--         GDK_M);
--      AddMenuButton
--        (Gettext("Delete"), "edit-delete", ActionToolBar, "", GDK_Delete,
--         null);
--      AddButton
--        (Gettext("Cancel"), "dialog-cancel", ActionToolBar,
--         Gettext("Discard all changes and back to files list [Escape]"),
--         GDK_Escape, 0);
--      AddButton
--        (Gettext("Restore"), "document-revert", ActionToolBar,
--         Gettext("Restore selected file or directory from the trash [ALT+R]"),
--         GDK_R);
--      AddSeparator(ActionToolBar);
--      AddButton
--        (Gettext("Preferences"), "preferences-desktop", ActionToolBar,
--         Gettext("Show the program preferences [ALT-P]"), GDK_P);
--      AddMenuButton
--        (Gettext("About"), "help-about", ActionToolBar,
--         Gettext("Show informations about the program [ALT-A]."), GDK_A, null);
--      AddSeparator(ActionToolBar);
--      Pack_Start
--        (Gtk_Header_Bar(Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
--         ActionToolBar);
--   end CreateActionToolbarUI;
--
--   procedure SetToolbars is
--      Header: constant GObject :=
--        GObject(Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0));
--      LeftBox: constant Gtk_Widget :=
--        Get_Child
--          (Gtk_Box
--             (Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 4)),
--           0);
--   begin
--      if Settings.ToolbarsOnTop then
--         if Get_Parent(Gtk_Widget(ActionToolBar)) = Gtk_Widget(Header) then
--            return;
--         end if;
--         Ref(ActionToolBar);
--         Remove(Gtk_Container(LeftBox), Gtk_Widget(ActionToolBar));
--         Ref(ItemToolBar);
--         Remove(Gtk_Container(LeftBox), ItemToolBar);
--         Pack_Start(Gtk_Header_Bar(Header), Gtk_Widget(ActionToolBar));
--         Pack_End(Gtk_Header_Bar(Header), ItemToolBar);
--         Set_Orientation(ActionToolBar, Orientation_Horizontal);
--         Set_Orientation(ItemToolBar, Orientation_Horizontal);
--      else
--         if Get_Parent(Gtk_Widget(ActionToolBar)) = LeftBox then
--            return;
--         end if;
--         Ref(ActionToolBar);
--         Remove(Gtk_Container(Header), Gtk_Widget(ActionToolBar));
--         Ref(ItemToolBar);
--         Remove(Gtk_Container(Header), ItemToolBar);
--         Pack_Start(Gtk_Box(LeftBox), Gtk_Widget(ActionToolBar));
--         Pack_End(Gtk_Box(LeftBox), ItemToolBar);
--         Set_Orientation(ActionToolBar, Orientation_Vertical);
--         Set_Orientation(ItemToolBar, Orientation_Vertical);
--      end if;
--   end SetToolbars;

end Toolbars;
