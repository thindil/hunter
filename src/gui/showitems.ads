-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* ShowItems/ShowItems
-- FUNCTION
-- Provide code to show informations and set some settings for selected files
-- or directories.
-- SOURCE
package ShowItems is
-- ****

   --## rule off GLOBAL_REFERENCES
   -- ****f* ShowItems/ShowItems.Destination_Directory
   -- FUNCTION
   -- Current directory previewed. Used during showing destination target for
   -- various commands
   -- SOURCE
   Destination_Directory: Unbounded_String;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****f* ShowItems/ShowItems.Scale_Image
   -- FUNCTION
   -- Scale currently previewed image
   -- SOURCE
   procedure Scale_Image;
   -- ****

   -- ****f* ShowItems/ShowItems.Show_Preview
   -- FUNCTION
   -- Show preview of the currently selected file or directory. If preview is
   -- not available, show information about the selected item.
   -- SOURCE
   procedure Show_Preview;
   -- ****

   -- ****o* ShowItems/ShowItems.Show_Selected_Command
   -- FUNCTION
   -- Show preview or information about the currently selected file or
   -- directory after user select it in the directory view
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowSelected
   -- SOURCE
   function Show_Selected_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

      -- ****f* ShowItems/ShowItems.Create_Show_Items_Ui
      -- FUNCTION
      -- Create UI related to show items and destination for moving/copying
      -- items.
      -- SOURCE
   procedure Create_Show_Items_Ui;
   -- ****

   -- ****f* ShowItems/ShowItems.Show_Destination
   -- FUNCTION
   -- Show destination directory for various commands in the preview widget
   -- SOURCE
   procedure Show_Destination;
   -- ****

   -- ****f* ShowItems/ShowItems.Show_Output
   -- FUNCTION
   -- Show the UI for the output of the user command
   -- SOURCE
   procedure Show_Output;
   -- ****

   -- ****f* ShowItems/ShowItems.Update_Output
   -- FUNCTION
   -- Update UI with the output of the user command
   -- PARAMETERS
   -- Text_To_Append - The new text to add in the output frame
   -- SOURCE
   procedure Update_Output(Text_To_Append: String);
   -- ****

end ShowItems;
