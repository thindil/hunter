-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****s* UserCommands/UserCommand
   -- FUNCTION
   -- Data structure for the user defined commands
   -- OPTIONS
   -- NeedOutput - If true, show the command output to the user
   -- Command    - The command to execute
   -- SOURCE
   type UserCommand is record
      NeedOutput: Boolean;
      Command: Unbounded_String;
   end record;
   -- ****

   -- ****t* UserCommands/Commands_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Commands_Container is new Indefinite_Hashed_Maps(String,
      UserCommand, Ada.Strings.Hash, "=");
   -- ****

   -- ****v* UserCommands/UserCommandsList
   -- FUNCTION
   -- User defined commands list
   -- SOURCE
   UserCommandsList: Commands_Container.Map;
   -- ****

   -- ****f* UserCommands/UpdateUserCommandsList
   -- FUNCTION
   -- Update list of user defined commands
   -- SOURCE
   procedure UpdateUserCommandsList;
   -- ****

   -- ****f* UserCommands/AddCommands
   -- FUNCTION
   -- Adds the Ada code to the Tcl interpreter
   -- SOURCE
   procedure AddCommands;
   -- ****

end UserCommands;
