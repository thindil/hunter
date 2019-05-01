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

with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;

package Messages is

   -- Show message with selected type to the user
   procedure ShowMessage(Message: String;
      MessageType: Gtk_Message_Type := Message_Error);
   -- Hide message
   procedure HideMessage(Object: access Gtkada_Builder_Record'Class);
   -- Emit Gtk Response Yes for message if user pressed Yes button
   procedure MessageYes(Object: access Gtkada_Builder_Record'Class);
   -- Emit Gtk Response No for message if user pressed No button
   procedure MessageNo(Object: access Gtkada_Builder_Record'Class);
   -- Hide message or do action, depends on the user response
   procedure MessageResponse(Self: access Gtk_Info_Bar_Record'Class;
      Response_Id: Gint);

end Messages;
