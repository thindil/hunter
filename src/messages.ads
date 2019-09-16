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

with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with Glib.Object; use Glib.Object;

-- ****h* Hunter/Messages
-- FUNCTION
-- Provide code to show or hide messages to the user.
-- SOURCE
package Messages is
-- ****

   -- ****v* Messages/YesToAll
   -- FUNCTION
   -- Set to True if user clicked Yes for All button in response to question,
   -- otherwise False
   -- SOURCE
   YesForAll: Boolean;
   -- ****

   -- ****f* Messages/ShowMessage
   -- FUNCTION
   -- Show message with selected type to the user
   -- PARAMETERS
   -- Message    - Text of message to show to the user
   -- MesageType - Gtk_Message_Type of message to show to the user. Default is
   --              Message_Error
   -- SOURCE
   procedure ShowMessage
     (Message: String; MessageType: Gtk_Message_Type := Message_Error);
   -- ****

   -- ****f* Messages/HideMessage
   -- FUNCTION
   -- Hide message
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure HideMessage(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Messages/CreateMessagesUI
   -- FUNCTION
   -- Create messages UI - mostly register proper procedures and functions
   -- for use in GTKAda Builder
   -- SOURCE
   procedure CreateMessagesUI;
   -- ****

end Messages;
