-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****v* Messages/Messages.Yes_For_All
   -- FUNCTION
   -- Set to True if user clicked Yes for All button in response to question,
   -- otherwise False
   -- SOURCE
   Yes_For_All: Boolean;
   -- ****

   -- ****o* Messages/Messages.Close_Command
   -- FUNCTION
   -- Hide message frame
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseMessage
   -- SOURCE
   function Close_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* Messages/Messages.Create_Messages_Ui
      -- FUNCTION
      -- Create UI related to the program messages
      -- SOURCE
   procedure Create_Messages_Ui;
   -- ****

   -- ****f* Messages/Messages.Show_Message
   -- FUNCTION
   -- Show selected message with selected type to the user
   -- PARAMETERS
   -- Message      - Text of message to show to the user
   -- Message_Type - Type of message. Posible values are error, message,
   --                question. Default value is error.
   -- SOURCE
   procedure Show_Message(Message: String; Message_Type: String := "error");
   -- ****

end Messages;
