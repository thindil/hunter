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

with Ada.Containers.Vectors; use Ada.Containers;
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

   -- ****v* MainWindowTUI/MainWindowTUI.Current_Directory
   -- FUNCTION
   -- Currently selected directory to show
   -- SOURCE
   Current_Directory: Unbounded_String;
   -- ****

   -- ****t* MainWindowTUI/MainWindowTUI.ItemActions
   -- FUNCTION
   -- Types of action on files and directories
   -- OPTIONS
   -- CREATEFILE      - create file
   -- CREATEDIRECTORY - create directory
   -- RENAME          - rename file or directory
   -- DELETE          - delete file or directory
   -- COPY            - copy file or directory
   -- MOVE            - move file or directory
   -- OPENWITH        - open selected file or directory with command
   -- GOTOPATH        - go to selected path
   -- CREATELINK      - create symbolic link to selected file or directory
   -- CLEARTRASH      - remove all files and directories from trash
   -- SHOWTRASH       - show content of the trash
   -- DELETETRASH     - delete file or directory from trash
   -- SOURCE
   type ItemActions is
     (CREATEFILE, CREATEDIRECTORY, RENAME, DELETE, COPY, MOVE, OPENWITH,
      GOTOPATH, CREATELINK, CLEARTRASH, SHOWTRASH, DELETETRASH);
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.New_Action
   -- FUNCTION
   -- Current performed action on files or directories
   -- SOURCE
   New_Action: ItemActions;
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
   -- SOURCE
   type UI_Locations is
     (DIRECTORY_VIEW, PATH_BUTTONS, MAIN_MENU, PREVIEW, ACTIONS_MENU,
      CREATE_FORM, DELETE_FORM, MESSAGE_FORM, RENAME_FORM, DESTINATION_VIEW,
      DESTINATION_PATH, BOOKMARKS_MENU, BOOKMARKS_FORM, CREATELINK_FORM,
      SELECTED_MENU, PROGRAMS_MENU, VIEW_MENU, SEARCH_FORM, EXECUTE_FORM,
      ABOUT_MENU, ABOUT_FORM, DEVELOPERS_VIEW, OPTIONS_VIEW, SECONDS_MENU,
      COLORS_MENU, SHORTCUT_FORM, COMMAND_FORM);
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.UILocation
   -- FUNCTION
   -- Currently active element of UI
   -- SOURCE
   UILocation: UI_Locations := DIRECTORY_VIEW;
   -- ****

   -- ****t* MainWindowTUI/MainWindowTUI.UnboundedString_Container
   -- FUNCTION
   -- Used to store various Unbounded_String data in list.
   -- SOURCE
   package UnboundedString_Container is new Vectors
     (Positive, Unbounded_String);
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.SelectedItems
   -- FUNCTION
   -- List of currently selected files and directories by user
   -- SOURCE
   SelectedItems: UnboundedString_Container.Vector;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.Current_Selected
   -- FUNCTION
   -- Full path to currently selected file or directory
   -- SOURCE
   Current_Selected: Unbounded_String;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.Interpreter
   -- FUNCTION
   -- Tcl interpreter to execute various program commands
   -- SOURCE
   Interpreter: Tcl_Interp;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.DirectoryList
   -- FUNCTION
   -- Menu with list of all items in the current viewed directory
   -- SOURCE
   DirectoryList: Menu;
   -- ****

   -- ****f* MainWindow/CreateProgramMenu
   -- FUNCTION
   -- Create the main program menu, the menu content depends on the program
   -- state
   -- PARAMETERS
   -- Update - If True also update the menu window. Default value is False.
   -- SOURCE
   procedure CreateProgramMenu(Update: Boolean := False);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.CreateMainWindow
   -- FUNCTION
   -- Create main window and show content of selected directory
   -- PARAMETERS
   -- Directory  - Full path to the directory which will be show at the
   --              program start
   -- SOURCE
   procedure CreateMainWindow(Directory: String);
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
   function Directory_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Path_Keys
   -- FUNCTION
   -- Handles keys events when path buttons are active elements of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Path_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Menu_Keys
   -- FUNCTION
   -- Handles keys events when the main menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Menu_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Actions_Keys
   -- FUNCTION
   -- Handles keys events when the actions menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Actions_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Bookmarks_Keys
   -- FUNCTION
   -- Handles keys events when the bookmarks menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Bookmarks_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Selected_Keys
   -- FUNCTION
   -- Handles keys events when the selected item menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function Selected_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.View_Keys
   -- FUNCTION
   -- Handles keys events when the view menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function View_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.About_Keys
   -- FUNCTION
   -- Handles keys events when the about menu is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- RESULT
   -- The currently selected UI element of the program
   -- SOURCE
   function About_Keys(Key: Key_Code) return UI_Locations;
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Show_Main_Window
   -- FUNCTION
   -- Show the main window of the program to the user
   -- SOURCE
   procedure Show_Main_Window;
   -- ****

end MainWindow;
