-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with MainWindow; use MainWindow;

-- ****h* Messages/MessagesTUI
-- FUNCTION
-- Provide code to show or hide messages to the user.
-- SOURCE
package Messages.UI is
-- ****

   -- ****f* MessagesTUI/MessagesTUI.Show_Message
   -- FUNCTION
   -- Show selected message with selected type to the user
   -- PARAMETERS
   -- Message      - Text of message to show to the user
   -- Message_Type - Type of message. Posible values are error, message,
   --                question. Default value is error.
   -- SOURCE
   procedure Show_Message(Message: String; Message_Type: String := "error");
   -- ****

   -- ****f* MessagesTUI/MessagesTUI.Message_Keys
   -- FUNCTION
   -- Handles keys events when the message dialog is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Message_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

end Messages.UI;
