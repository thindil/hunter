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

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* UserCommands/UserCommands
-- FUNCTION
-- Provide code for manipulate the user defined commands/actions
-- SOURCE
package UserCommands is
-- ****

   -- ****s* UserCommands/UserCommands.User_Command
   -- FUNCTION
   -- Data structure for the user defined commands
   -- OPTIONS
   -- Need_Output - If true, show the command output to the user
   -- Command     - The command to execute
   -- SOURCE
   type User_Command is record
      Need_Output: Boolean;
      Command: Unbounded_String;
   end record;
   -- ****

   -- ****d* UserCommands/UserCommands.Empty_User_Command
   -- FUNCTION
   -- Default value for empty user command
   -- SOURCE
   Empty_User_Command: constant User_Command :=
     (Need_Output => False, Command => Null_Unbounded_String);
   -- ****

   -- ****t* UserCommands/UserCommands.Commands_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Commands_Container is new Indefinite_Hashed_Maps
     (Key_Type => String, Element_Type => User_Command,
      Hash => Ada.Strings.Hash, Equivalent_Keys => "=");
   -- ****

   -- ****v* UserCommands/UserCommands.User_Commands_List
   -- FUNCTION
   -- User defined commands list
   -- SOURCE
   User_Commands_List: Commands_Container.Map;
   -- ****

   -- ****f* UserCommands/UserCommands.Add_Commands
   -- FUNCTION
   -- Adds the Ada code to the Tcl interpreter
   -- SOURCE
   procedure Add_Commands;
   -- ****

end UserCommands;
