-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* Messages/Messages
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

   -- ****o* Messages/Close_Command
   -- FUNCTION
   -- Hide message frame
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseMessage
   -- SOURCE
   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* Messages/CreateMessagesUI
      -- FUNCTION
      -- Create UI related to the program messages
      -- SOURCE
   procedure CreateMessagesUI;
   -- ****

   -- ****f* Messages/ShowMessage
   -- FUNCTION
   -- Show selected message with selected type to the user
   -- PARAMETERS
   -- Message     - Text of message to show to the user
   -- MessageType - Type of message. Posible values are error, message,
   --               question. Default value is error.
   -- SOURCE
   procedure ShowMessage(Message: String; MessageType: String := "error");
   -- ****

end Messages;
