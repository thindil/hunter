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

with Gtk.Box; use Gtk.Box;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Radio_Tool_Button; use Gtk.Radio_Tool_Button;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Widget; use Gtk.Widget;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with MainWindow; use MainWindow;
with Preferences; use Preferences;

package body Toolbars is

   procedure AddButton
     (Text, IconName: String; Toolbar: Gtk_Toolbar; Tooltip: String) is
      Button: constant Gtk_Tool_Button := Gtk_Tool_Button_New(Label => Text);
   begin
      Set_Tooltip_Text(Button, Tooltip);
      Set_Icon_Name(Button, IconName);
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
      Toolbar: Gtk_Toolbar; Tooltip: String) is
      Button: constant Gtk_Radio_Tool_Button :=
        Gtk_Radio_Tool_Button_New(RadioGroup);
   begin
      RadioGroup := Get_Group(Button);
      Set_Label(Button, Text);
      Set_Tooltip_Text(Button, Tooltip);
      Set_Icon_Name(Button, IconName);
      Insert(Toolbar, Button);
   end AddRadioButton;

   procedure CreateItemToolbarUI is
      RadioGroup: Widget_SList.GSlist;
   begin
      ItemToolBar := Gtk_Toolbar_New;
      Set_Style(ItemToolBar, Toolbar_Icons);
      Set_Halign(ItemToolBar, Align_Center);
      Set_Valign(ItemToolBar, Align_End);
      AddButton
        (Gettext("Run"), "media-playback-start", ItemToolBar,
         Gettext("Execute selected program [ALT-E]."));
      AddButton
        (Gettext("Open"), "document-open", ItemToolBar,
         Gettext("Open selected file or directory [ALT-O]"));
      AddButton
        (Gettext("Open with..."), "system-run", ItemToolBar,
         Gettext("Open selected file or directory with command [ALT-W]"));
      AddSeparator(ItemToolBar);
      AddRadioButton
        (Gettext("Preview"), "document-print-preview", RadioGroup, ItemToolBar,
         Gettext("Preview file or directory [ALT-V]"));
      AddRadioButton
        (Gettext("Info"), "document-properties", RadioGroup, ItemToolBar,
         Gettext("File or directory informations [ALT-I]"));
      AddSeparator(ItemToolBar);
      AddButton
        (Gettext("Add bookmark"), "list-add", ItemToolBar,
         Gettext("Add bookmark to this directory [ALT-B]."));
      AddButton
        (Gettext("Remove bookmark"), "list-remove", ItemToolBar,
         Gettext("Remove bookmark to this directory [ALT-B]"));
      Pack_End(Gtk_Header_Bar(Get_Object(Builder, "header")), ItemToolBar);
   end CreateItemToolbarUI;

   procedure SetToolbars is
      Header: constant GObject := Get_Object(Builder, "header");
      LeftBox: constant Gtk_Widget :=
        Get_Child
          (Gtk_Box
             (Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 4)),
           0);
      Toolbar: constant GObject := Get_Object(Builder, "toolbar");
   begin
      if Settings.ToolbarsOnTop then
         if Get_Parent(Gtk_Widget(Toolbar)) = Gtk_Widget(Header) then
            return;
         end if;
         Remove(Gtk_Container(LeftBox), Gtk_Widget(Toolbar));
         Ref(ItemToolBar);
         Remove(Gtk_Container(LeftBox), ItemToolBar);
         Pack_Start(Gtk_Header_Bar(Header), Gtk_Widget(Toolbar));
         Pack_End(Gtk_Header_Bar(Header), ItemToolBar);
         Set_Orientation(Gtk_Toolbar(Toolbar), Orientation_Horizontal);
         Set_Orientation(ItemToolBar, Orientation_Horizontal);
      else
         if Get_Parent(Gtk_Widget(Toolbar)) = LeftBox then
            return;
         end if;
         Remove(Gtk_Container(Header), Gtk_Widget(Toolbar));
         Ref(ItemToolBar);
         Remove(Gtk_Container(Header), ItemToolBar);
         Pack_Start(Gtk_Box(LeftBox), Gtk_Widget(Toolbar));
         Pack_End(Gtk_Box(LeftBox), ItemToolBar);
         Set_Orientation(Gtk_Toolbar(Toolbar), Orientation_Vertical);
         Set_Orientation(ItemToolBar, Orientation_Vertical);
      end if;
   end SetToolbars;

end Toolbars;
