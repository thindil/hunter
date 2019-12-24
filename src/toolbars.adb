-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Box; use Gtk.Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Radio_Tool_Button; use Gtk.Radio_Tool_Button;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Widget; use Gtk.Widget;
with Glib.Object; use Glib.Object;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Bookmarks; use Bookmarks;
with MainWindow; use MainWindow;
with Preferences; use Preferences;

package body Toolbars is

   Accelerators: Gtk_Accel_Group;

   procedure AddButton
     (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
      Key: Gdk_Key_Type; Mask: Gdk_Modifier_Type := Mod1_Mask) is
      Button: constant Gtk_Tool_Button := Gtk_Tool_Button_New(Label => Text);
   begin
      Set_Tooltip_Text(Button, Tooltip);
      Set_Icon_Name(Button, IconName);
      Add_Accelerator
        (Button, "clicked", Accelerators, Key, Mask, Accel_Visible);
      Insert(Toolbar, Button);
   end AddButton;

   procedure AddSeparator(Toolbar: Gtk_Toolbar) is
      Separator: constant Gtk_Separator_Tool_Item :=
        Gtk_Separator_Tool_Item_New;
   begin
      Insert(Toolbar, Separator);
   end AddSeparator;

   procedure AddRadioButton
     (Text, IconName: String; RadioGroup: in out Widget_SList.GSlist;
      Toolbar: Gtk_Toolbar; Tooltip: String; Key: Gdk_Key_Type) is
      Button: constant Gtk_Radio_Tool_Button :=
        Gtk_Radio_Tool_Button_New(RadioGroup);
   begin
      RadioGroup := Get_Group(Button);
      Set_Label(Button, Text);
      Set_Tooltip_Text(Button, Tooltip);
      Set_Icon_Name(Button, IconName);
      Add_Accelerator
        (Button, "clicked", Accelerators, Key, Mod1_Mask, Accel_Visible);
      Insert(Toolbar, Button);
   end AddRadioButton;

   procedure CreateItemToolbarUI is
      RadioGroup: Widget_SList.GSlist;
   begin
      if Accelerators = null then
         Accelerators := Gtk_Accel_Group(Get_Object(Builder, "accelerators"));
      end if;
      ItemToolBar := Gtk_Toolbar_New;
      Set_Style(ItemToolBar, Toolbar_Icons);
      Set_Halign(ItemToolBar, Align_Center);
      Set_Valign(ItemToolBar, Align_End);
      AddButton
        (Gettext("Run"), "media-playback-start", ItemToolBar,
         Gettext("Execute selected program [ALT-E]."), GDK_E);
      AddButton
        (Gettext("Open"), "document-open", ItemToolBar,
         Gettext("Open selected file or directory [ALT-O]"), GDK_O);
      AddButton
        (Gettext("Open with..."), "system-run", ItemToolBar,
         Gettext("Open selected file or directory with command [ALT-W]"),
         GDK_W);
      AddSeparator(ItemToolBar);
      AddRadioButton
        (Gettext("Preview"), "document-print-preview", RadioGroup, ItemToolBar,
         Gettext("Preview file or directory [ALT-V]"), GDK_V);
      AddRadioButton
        (Gettext("Info"), "document-properties", RadioGroup, ItemToolBar,
         Gettext("File or directory informations [ALT-I]"), GDK_I);
      AddSeparator(ItemToolBar);
      AddButton
        (Gettext("Add bookmark"), "list-add", ItemToolBar,
         Gettext("Add bookmark to this directory [ALT-B]."), GDK_B);
      AddButton
        (Gettext("Remove bookmark"), "list-remove", ItemToolBar,
         Gettext("Remove bookmark to this directory [ALT-B]"), GDK_B);
      Pack_End(Gtk_Header_Bar(Get_Object(Builder, "header")), ItemToolBar);
   end CreateItemToolbarUI;

   procedure CreateActionToolbarUI is
      procedure AddMenuButton
        (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
         Key: Gdk_Key_Type; Menu: Gtk_Widget) is
         Button: constant Gtk_Menu_Tool_Button :=
           Gtk_Menu_Tool_Button_New(Label => Text);
      begin
         Set_Tooltip_Text(Button, Tooltip);
         Set_Icon_Name(Button, IconName);
         Add_Accelerator
           (Button, "clicked", Accelerators, Key, Mod1_Mask, Accel_Visible);
         Set_Menu(Button, Menu);
         Insert(Toolbar, Button);
      end AddMenuButton;
      procedure AddToggleButton
        (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String;
         Key: Gdk_Key_Type; Mask: Gdk_Modifier_Type := Mod1_Mask) is
         Button: constant Gtk_Toggle_Tool_Button := Gtk_Toggle_Tool_Button_New;
      begin
         Set_Label(Button, Text);
         Set_Tooltip_Text(Button, Tooltip);
         Set_Icon_Name(Button, IconName);
         Add_Accelerator
           (Button, "clicked", Accelerators, Key, Mask, Accel_Visible);
         Insert(Toolbar, Button);
      end AddToggleButton;
   begin
      if Accelerators = null then
         Accelerators := Gtk_Accel_Group(Get_Object(Builder, "accelerators"));
      end if;
      ActionToolBar := Gtk_Toolbar_New;
      Set_Style(ActionToolBar, Toolbar_Icons);
      CreateBookmarkMenu(True);
      AddMenuButton
        (Gettext("Home"), "user-home", ActionToolBar,
         Gettext
           ("Go to your home directory [ALT+H] or press arrow to see more bookmarks"),
         GDK_H, Gtk_Widget(Get_Object(Builder, "bookmarksmenu")));
      AddToggleButton
        (Gettext("Search"), "edit-find", ActionToolBar,
         Gettext("Search for the file or directory [ALT+F]"), GDK_F);
      AddButton
        (Gettext("Select All"), "edit-select-all", ActionToolBar,
         Gettext
           ("Select or unselect all files and directories in currently selected directory. [CTRL+A]"),
         GDK_A, Control_Mask);
      AddSeparator(ActionToolBar);
      AddMenuButton
        (Gettext("new"), "document-new", ActionToolBar,
         Gettext
           ("Add new directory [ALT+N] or press arrow to see more options."),
         GDK_N, Gtk_Widget(Get_Object(Builder, "newmenu")));
      AddButton
        (Gettext("Rename"), "document-save-as", ActionToolBar,
         Gettext("Rename selected file or directory [CTRL-R]"), GDK_R,
         Control_Mask);
      AddToggleButton
        (Gettext("Copy"), "edit-copy", ActionToolBar,
         Gettext
           ("Copy selected files [ALT-C]. Pressed button means start copying currently selected files or directories. Press again to copy them."),
         GDK_C);
      AddToggleButton
        (Gettext("Move"), "edit-cut", ActionToolBar,
         Gettext
           ("Move selected files [ALT-M]. Pressed button means start moving currently selected files or directories. Press again to move them."),
         GDK_M);
      AddMenuButton
        (Gettext("Delete"), "edit-delete", ActionToolBar, "", GDK_Delete, Gtk_Widget(Get_Object(Builder, "deletemenu")));
      AddButton
        (Gettext("Cancel"), "dialog-cancel", ActionToolBar,
         Gettext("Discard all changes and back to files list [Escape]"),
         GDK_Escape, 0);
      AddButton
        (Gettext("Restore"), "document-revert", ActionToolBar,
         Gettext("Restore selected file or directory from the trash [ALT+R]"),
         GDK_R);
      AddSeparator(ActionToolBar);
      AddButton
        (Gettext("Preferences"), "preferences-desktop", ActionToolBar,
         Gettext("Show the program preferences [ALT-P]"), GDK_P);
      AddMenuButton
        (Gettext("About"), "help-about", ActionToolBar,
         Gettext("Show informations about the program [ALT-A]."), GDK_A, Gtk_Widget(Get_Object(Builder, "aboutmenu")));
      AddSeparator(ActionToolBar);
      Pack_Start(Gtk_Header_Bar(Get_Object(Builder, "header")), ActionToolBar);
   end CreateActionToolbarUI;

   procedure SetToolbars is
      Header: constant GObject := Get_Object(Builder, "header");
      LeftBox: constant Gtk_Widget :=
        Get_Child
          (Gtk_Box
             (Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 4)),
           0);
   begin
      if Settings.ToolbarsOnTop then
         if Get_Parent(Gtk_Widget(ActionToolBar)) = Gtk_Widget(Header) then
            return;
         end if;
         Ref(ActionToolBar);
         Remove(Gtk_Container(LeftBox), Gtk_Widget(ActionToolBar));
         Ref(ItemToolBar);
         Remove(Gtk_Container(LeftBox), ItemToolBar);
         Pack_Start(Gtk_Header_Bar(Header), Gtk_Widget(ActionToolBar));
         Pack_End(Gtk_Header_Bar(Header), ItemToolBar);
         Set_Orientation(ActionToolBar, Orientation_Horizontal);
         Set_Orientation(ItemToolBar, Orientation_Horizontal);
      else
         if Get_Parent(Gtk_Widget(ActionToolBar)) = LeftBox then
            return;
         end if;
         Ref(ActionToolBar);
         Remove(Gtk_Container(Header), Gtk_Widget(ActionToolBar));
         Ref(ItemToolBar);
         Remove(Gtk_Container(Header), ItemToolBar);
         Pack_Start(Gtk_Box(LeftBox), Gtk_Widget(ActionToolBar));
         Pack_End(Gtk_Box(LeftBox), ItemToolBar);
         Set_Orientation(ActionToolBar, Orientation_Vertical);
         Set_Orientation(ItemToolBar, Orientation_Vertical);
      end if;
   end SetToolbars;

end Toolbars;
