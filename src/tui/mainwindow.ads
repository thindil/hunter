-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Tcl; use Tcl;

-- ****h* MainWindowTUI/MainWindowTUI
-- FUNCTION
-- Provide code to show and manipulate the main program window.
-- SOURCE
package MainWindow is
-- ****

   -- ****t* MainWindow/UI_Locations
   -- FUNCTION
   -- Parts of the program UI, used mostly in handling keys events
   -- OPTIONS
   -- DIRECTORY_VIEW   - Current directory listing
   -- PATH_BUTTONS     - Current directory path buttons
   -- MAIN_MENU        - The main program's menu
   -- PREVIEW          - The preview window
   -- ACTIONS_MENU     - The actions menu
   -- CREATE_FORM      - The create a new directory or file form
   -- DELETE_FORM      - The delete files confirmation form
   -- MESSAGE_FORM     - The message dialog
   -- RENAME_FORM      - The rename directory or file form
   -- DESTINATION_VIEW - The destination directory for copying/moving items
   -- DESTINATION_PATH - The destination directory path buttons
   -- BOOKMARKS_MENU   - The user's bookmarks menu
   -- BOOKMARKS_FORM   - The enter destination directory form
   -- CREATELINK_FORM  - The creating of new link
   -- SELECTED_MENU    - The selected file or directory menu
   -- PROGRAMS_MENU    - The list of applications to use with item
   -- VIEW_MENU        - Select/deselect/search items in the directory view
   -- SEARCH_FORM      - The search entry form
   -- EXECUTE_FORM     - The execute item with command form
   -- ABOUT_MENU       - The menu about the program
   -- ABOUT_FORM       - The about widget
   -- DEVELOPERS_VIEW  - The lists of the program's programmers and translators
   -- OPTIONS_VIEW     - The view of the program's options settings
   -- SECONDS_MENU     - The menu with list of values to set as time interval
   -- COLORS_MENU      - The menu with list of available color themes for text
   --                    highlightning
   -- SHORTCUT_FORM    - The form for set the program keyboard shortcut
   -- COMMAND_FORM     - The form for add or edit the user defined commands
   -- COMMANDS_MENU    - The menu with user defined commands
   -- T_ACTIONS_MENU   - The menu with actions in trash
   -- QUIT_PROGRAM     - Quitting from the program
   -- SOURCE
   type Ui_Locations is
     (DIRECTORY_VIEW, PATH_BUTTONS, MAIN_MENU, PREVIEW, ACTIONS_MENU,
      CREATE_FORM, DELETE_FORM, MESSAGE_FORM, RENAME_FORM, DESTINATION_VIEW,
      DESTINATION_PATH, BOOKMARKS_MENU, BOOKMARKS_FORM, CREATELINK_FORM,
      SELECTED_MENU, PROGRAMS_MENU, VIEW_MENU, SEARCH_FORM, EXECUTE_FORM,
      ABOUT_MENU, ABOUT_FORM, DEVELOPERS_VIEW, OPTIONS_VIEW, SECONDS_MENU,
      COLORS_MENU, SHORTCUT_FORM, COMMAND_FORM, COMMANDS_MENU, T_ACTIONS_MENU,
      QUIT_PROGRAM);
   -- ****

   -- ****d* MainWindowTUI/MainWindowTUI.Default_Ui_Location
   -- FUNCTION
   -- Default UI location
   -- SOURCE
   Default_Ui_Location: constant Ui_Locations := DIRECTORY_VIEW;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.Ui_Location
   -- FUNCTION
   -- Currently active element of UI
   -- SOURCE
   Ui_Location: Ui_Locations := Default_Ui_Location;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.Directory_List
   -- FUNCTION
   -- Menu with list of all items in the current viewed directory
   -- SOURCE
   Directory_List: Menu;
   -- ****

   -- ****f* MainWindow/Create_Program_Menu
   -- FUNCTION
   -- Create the main program menu, the menu content depends on the program
   -- state
   -- PARAMETERS
   -- Update - If True also update the menu window. Default value is False.
   -- SOURCE
   procedure Create_Program_Menu(Update: Boolean := False);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Create_Main_Window
   -- FUNCTION
   -- Create main window and show content of selected directory
   -- PARAMETERS
   -- Directory  - Full path to the directory which will be show at the
   --              program start
   -- SOURCE
   procedure Create_Main_Window(Directory: String);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Update_Directory_List
   -- FUNCTION
   -- Update directory list
   -- PARAMETERS
   -- Clear      - Clear current list of items
   -- Search_For - If not empty, show only items which contains the selected
   --              string. Otherwise show everything. Default value is empty.
   -- SOURCE
   procedure Update_Directory_List
     (Clear: Boolean := False; Search_For: String := "");
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Directory_Keys
   -- FUNCTION
   -- Handles keys events when directory view is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Directory_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Path_Keys
   -- FUNCTION
   -- Handles keys events when path buttons are active elements of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Path_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Draw_Menu
   -- FUNCTION
   -- Show the selected menu to the user
   -- PARAMETERS
   -- Menu_Type - The type of the menu to show
   -- SOURCE
   procedure Draw_Menu(Menu_Type: Ui_Locations);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Menu_Keys
   -- FUNCTION
   -- Handles keys events when the main menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Menu_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Actions_Keys
   -- FUNCTION
   -- Handles keys events when the actions menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Actions_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Bookmarks_Keys
   -- FUNCTION
   -- Handles keys events when the bookmarks menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Bookmarks_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Selected_Keys
   -- FUNCTION
   -- Handles keys events when the selected item menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Selected_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.View_Keys
   -- FUNCTION
   -- Handles keys events when the view menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function View_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.About_Keys
   -- FUNCTION
   -- Handles keys events when the about menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function About_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Show_Main_Window
   -- FUNCTION
   -- Show the main window of the program to the user
   -- SOURCE
   procedure Show_Main_Window;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.User_Commands_Keys
   -- FUNCTION
   -- Handles keys events when the user commands menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function User_Commands_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Trash_Actions_Keys
   -- FUNCTION
   -- Handles keys events when the trash actions menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Trash_Actions_Keys(Key: Key_Code) return Ui_Locations;
   -- ****

end MainWindow;
