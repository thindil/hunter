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

--with Gtk.Info_Bar; use Gtk.Info_Bar;
--with Gtk.Message_Dialog; use Gtk.Message_Dialog;

-- ****h* Hunter/Messages
-- FUNCTION
-- Provide code to show or hide messages to the user.
-- SOURCE
package Messages is
-- ****

   -- ****f* Messages/CreateMessagesUI
   -- FUNCTION
   -- Create UI related to the program messages
   -- SOURCE
   procedure CreateMessagesUI;
   -- ****

   -- ****v* Messages/YesToAll
   -- FUNCTION
   -- Set to True if user clicked Yes for All button in response to question,
   -- otherwise False
   -- SOURCE
--   YesForAll: Boolean;
--   -- ****
--
--   -- ****v* Messages/InfoBar
--   -- FUNCTION
--   -- Gtk_Info_Bar which will be showing all messages
--   -- SOURCE
--   InfoBar: Gtk_Info_Bar;
--   -- ****
--
--   -- ****f* Messages/CloseMessage
--   -- FUNCTION
--   -- Close message and stop timer
--   -- PARAMETERS
--   -- Self - Gtk_Info_Bar with message to close. Unused. Can be null
--   -- SOURCE
--   procedure CloseMessage(Self: access Gtk_Info_Bar_Record'Class);
--   -- ****
--
--   -- ****f* Messages/ShowMessage
--   -- FUNCTION
--   -- Show message with selected type to the user
--   -- PARAMETERS
--   -- Message    - Text of message to show to the user
--   -- MesageType - Gtk_Message_Type of message to show to the user. Default is
--   --              Message_Error
--   -- SOURCE
--   procedure ShowMessage
--     (Message: String; MessageType: Gtk_Message_Type := Message_Error);
   -- ****

end Messages;
