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

   -- ****f* Messages/ShowMessage
   -- FUNCTION
   -- Show message with selected type to the user
   -- PARAMETERS
   -- Message    - Text of message to show to the user
   -- MesageType - Gtk_Message_Type of message to show to the user. Default is
   --              Message_Error
   -- SOURCE
   procedure ShowMessage(Message: String;
      MessageType: Gtk_Message_Type := Message_Error);
   -- ****
   -- ****f* Messages/HideMessage
   -- FUNCTION
   -- Hide message
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure HideMessage(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Messages/MessageYes
   -- FUNCTION
   -- Emit Gtk Response Yes for message if user pressed Yes button
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure MessageYes(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Messages/MessageNo
   -- FUNCTION
   -- Emit Gtk Response No for message if user pressed No button
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure MessageNo(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Messages/MessageResponse
   -- FUNCTION
   -- Hide message or do action, depends on the user response
   -- PARAMETERS
   -- Self        - Gtk_Info_Bar which contains the message
   -- Response_Id - Gtk_Response depends on which button user clicked
   -- SOURCE
   procedure MessageResponse(Self: access Gtk_Info_Bar_Record'Class;
      Response_Id: Gint);
   -- ****

end Messages;
