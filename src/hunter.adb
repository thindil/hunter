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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Main; use Gtk.Main;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Bindings; use Gtkada.Bindings;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with ErrorDialog; use ErrorDialog;
with MainWindow; use MainWindow;

procedure Hunter is
   Builder: Gtkada_Builder;
   Error: aliased GError;
begin
   if not Ada.Environment_Variables.Exists("RUNFROMSCRIPT") then
      Put_Line
        ("The program can be run only via 'hunter.sh' script. Please don't run binary directly.");
      return;
   end if;
   Init;
   Set_On_Exception(On_Exception'Access);
   Gtk_New(Builder);
   if Add_From_File(Builder, "ui/hunter.glade", Error'Access) = Guint(0) then
      Put_Line("Error : " & Get_Message(Error));
      return;
   end if;
   CreateErrorDialog(Builder);
   if Argument_Count < 1 then
      CreateMainWindow(Builder, Value("HOME"));
   else
      CreateMainWindow(Builder, Full_Name(Argument(1)));
   end if;
   Clear("LD_LIBRARY_PATH");
   Clear("GDK_PIXBUF_MODULE_FILE");
   Clear("GDK_PIXBUF_MODULEDIR");
   Clear("FONTCONFIG_FILE");
   Main;
exception
   when An_Exception : others =>
      SaveException(An_Exception, True);
end Hunter;
