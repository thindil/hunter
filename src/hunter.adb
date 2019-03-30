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
   Init;
   Set_On_Exception(On_Exception'Access);
   Gtk_New(Builder);
   if Add_From_File(Builder, "ui/hunter.glade", Error'Access) = Guint(0) then
      Put_Line("Error : " & Get_Message(Error));
      return;
   end if;
   CreateErrorDialog(Builder);
   CreateMainWindow(Builder);
   Main;
exception
   when An_Exception : others =>
      SaveException(An_Exception, True);
end Hunter;
