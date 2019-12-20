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

with Gtk.Enums; use Gtk.Enums;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with MainWindow; use MainWindow;

package body Toolbars is

   procedure CreateItemToolbarUI is
      procedure AddButton(Text, IconName: String) is
         Button: constant Gtk_Tool_Button := Gtk_Tool_Button_New(Label => Text);
      begin
         Set_Icon_Name(Button, IconName);
         Insert(ItemToolBar, Button);
      end AddButton;
      procedure AddSeparator is
         Separator: constant Gtk_Separator_Tool_Item := Gtk_Separator_Tool_Item_New;
      begin
         Insert(ItemToolBar, Separator);
      end AddSeparator;
   begin
      ItemToolBar := Gtk_Toolbar_New;
      Set_Style(ItemToolBar, Toolbar_Icons);
      Set_Halign(ItemToolBar, Align_Center);
      Set_Valign(ItemToolBar, Align_End);
      AddButton(Gettext("Run"), "media-playback-start");
      AddButton(Gettext("Open"), "document-open");
      AddButton(Gettext("Open with..."), "system-run");
      AddSeparator;
      AddButton(Gettext("Preview"), "document-print-preview");
      AddButton(Gettext("Info"), "document-properties");
      AddSeparator;
      AddButton(Gettext("Add bookmark"), "list-add");
      AddButton(Gettext("Remove bookmark"), "list-remove");
      Pack_End(Gtk_Header_Bar(Get_Object(Builder, "header")), ItemToolBar);
   end CreateItemToolbarUI;

end Toolbars;
