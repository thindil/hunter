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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with MainWindow; use MainWindow;

-- ****h* ShowItemsTUI/ShowItemsTUI
-- FUNCTION
-- Provide code to show informations and set some settings for selected files
-- or directories.
-- SOURCE
package ShowItems is
-- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Destination_Directory
   -- FUNCTION
   -- Current directory previewed. Used during showing destination target for
   -- various commands
   -- SOURCE
   Destination_Directory: Unbounded_String;
   -- ****

   -- ****v* ShowItemsTUI/ShowItemsTUI.DestinationList
   -- FUNCTION
   -- Current content of the destination directory
   -- SOURCE
   DestinationList: Menu;
   -- ****

   -- ****v* ShowItems/Default_Program_Form
   -- FUNCTION
   -- Form to set default program and item permissions
   -- SOURCE
   Info_Form: Forms.Form;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.ShowInfo
   -- FUNCTION
   -- Show information about the currently selected file or directory.
   -- SOURCE
   procedure ShowInfo;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Show_Preview
   -- FUNCTION
   -- Show preview of the currently selected file or directory. If preview is
   -- not available, show information about the selected item.
   -- PARAMETERS
   -- Reset_Preview - If True, reset the first line in preview text file. Can
   --                 be empty. Default value is True
   -- SOURCE
   procedure Show_Preview(Reset_Preview: Boolean := True);
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Show_Selected
   -- FUNCTION
   -- Show preview or information about the currently selected file or
   -- directory after user select it in the directory view
   -- SOURCE
   procedure Show_Selected;
   -- ****

      -- ****f* ShowItemsTUI/ShowItemsTUI.CreateShowItemsUI
      -- FUNCTION
      -- Create UI related to show items and destination for moving/copying
      -- items.
      -- SOURCE
   procedure CreateShowItemsUI;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.ShowDestination
   -- FUNCTION
   -- Show destination directory for various commands in the preview widget
   -- SOURCE
   procedure ShowDestination;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Show_Output
   -- FUNCTION
   -- Show the UI for the output of the user command
   -- SOURCE
   procedure Show_Output;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Update_Output
   -- FUNCTION
   -- Update UI with the output of the user command
   -- PARAMETERS
   -- Text_To_Append - The text which will be appended to the output
   -- SOURCE
   procedure Update_Output(Text_To_Append: String);
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Destination_Keys
   -- FUNCTION
   -- Handles keys events when the destination view is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- SOURCE
   procedure Destination_Keys(Key: Key_Code);
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Destination_Path_Keys
   -- FUNCTION
   -- Handles keys events when destination directory path buttons are active
   -- elements of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Destination_Path_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Preview_Keys
   -- FUNCTION
   -- Handles keys events when the preview window is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- SOURCE
   procedure Preview_Keys(Key: Key_Code);
   -- ****

   -- ****f* ShowItemsTUI/ShowItemsTUI.Clear_Preview_Window
   -- FUNCTION
   -- Clear the preview window
   -- SOURCE
   procedure Clear_Preview_Window;
   -- ****

end ShowItems;
