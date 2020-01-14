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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.About_Dialog; use Gtk.About_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib.Error; use Glib.Error;

package body AboutDialog is

   procedure ShowAboutDialog(Parent: Gtk_Window) is
      AboutDialog: constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      Error: GError;
      LogoBuf: Gdk_Pixbuf;
      IconName: Unbounded_String;
   begin
      Set_Transient_For(Gtk_Window(AboutDialog), Parent);
      Set_Program_Name(AboutDialog, "Hunter");
      Set_License_Type(AboutDialog, License_Gpl_3_0);
      Set_Comments(AboutDialog, "Graphical File Manager for Linux");
      Set_Copyright(AboutDialog, "(c) 2019 Bartek thindil Jasicki");
      Set_Authors
        (AboutDialog,
         (new String'("Bartek thindil Jasicki <thindil@laeran.pl>"),
          new String'("")));
      Set_Translator_Credits(AboutDialog, Gettext("translator-credits"));
      if Ada.Directories.Exists
          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
         IconName :=
           To_Unbounded_String(Value("APPDIR", "") & "/hunter-icon.png");
      else
         IconName :=
           To_Unbounded_String
             (Containing_Directory(Current_Directory) &
              "/others/hunter-icon.png");
      end if;
      Gdk_New_From_File(LogoBuf, To_String(IconName), Error);
      Set_Logo(AboutDialog, LogoBuf);
      Set_Version(AboutDialog, "1.2");
      Set_Website(AboutDialog, "https://thindil.github.io/hunter/");
      Set_Website_Label(AboutDialog, "Website");
      if Run(Gtk_Dialog(AboutDialog)) = Gtk_Response_Delete_Event then
         Destroy(Gtk_Widget(AboutDialog));
      end if;
   end ShowAboutDialog;

end AboutDialog;
