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

   -- ****v* MainWindowTUI/MainWindowTUI.CurrentDirectory
   -- FUNCTION
   -- Currently selected directory to show
   -- SOURCE
   CurrentDirectory: Unbounded_String;
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

   -- ****v* MainWindowTUI/MainWindowTUI.NewAction
   -- FUNCTION
   -- Current performed action on files or directories
   -- SOURCE
   NewAction: ItemActions;
   -- ****

   -- ****t* MainWindow/UI_Locations
   -- FUNCTION
   -- Parts of the program UI, used mostly in handling keys events
   -- OPTION
   -- DIRECTORY_VIEW - Current directory listing
   -- PATH_BUTTONS   - Current directory path buttons
   -- MAIN_MENU      - The main program's menu
   -- PREVIEW        - The preview window
   -- SOURCE
   type UI_Locations is (DIRECTORY_VIEW, PATH_BUTTONS, MAIN_MENU, PREVIEW);
   -- ****

   -- ****t* MainWindowTUI/MainWindowTUI.UnboundedString_Container
   -- FUNCTION
   -- Used to store various Unbounded_String data in list.
   -- SOURCE
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.SelectedItems
   -- FUNCTION
   -- List of currently selected files and directories by user
   -- SOURCE
   SelectedItems: UnboundedString_Container.Vector;
   -- ****

   -- ****v* MainWindowTUI/MainWindowTUI.CurrentSelected
   -- FUNCTION
   -- Full path to currently selected file or directory
   -- SOURCE
   CurrentSelected: Unbounded_String;
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

   -- ****f* MainWindowTUI/MainWindowTUI.CreateMainWindow
   -- FUNCTION
   -- Create main window and show content of selected directory
   -- PARAMETERS
   -- Directory  - Full path to the directory which will be show at the
   --              program start
   -- Interp     - Tcl interpreter used by the program
   -- SOURCE
   procedure CreateMainWindow(Directory: String; Interp: Tcl_Interp);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.UpdateDirectoryList
   -- FUNCTION
   -- Update directory list
   -- PARAMETERS
   -- Clear - Clear current list of items
   -- SOURCE
   procedure UpdateDirectoryList(Clear: Boolean := False);
   -- ****

   -- ****f* MainWindowTUI/MainWindowTUI.Directory_Keys
   -- FUNCTION
   -- Handles keys events when directory view is active element of UI
   -- PARAMETERS
   -- Key - Key pressed by the user
   -- SOURCE
   procedure Directory_Keys(Key: Key_Code);
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

end MainWindow;
