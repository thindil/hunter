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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Hunter/MainWindow
-- FUNCTION
-- Provide code to show and manipulate the main program window.
-- SOURCE
package MainWindow is
-- ****

   -- ****v* MainWindow/CurrentDirectory
   -- FUNCTION
   -- Currently selected directory to show
   -- SOURCE
   CurrentDirectory: Unbounded_String;
   -- ****

   -- ****t* MainWindow/ItemActions
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

   -- ****v* MainWindow/NewAction
   -- FUNCTION
   -- Current performed action on files or directories
   -- SOURCE
   NewAction: ItemActions;
   -- ****

   -- ****f* MainWindow/CreateMainWindow
   -- FUNCTION
   -- Create main window and show content of selected directory
   -- PARAMETERS
   -- Directory  - Full path to the directory which will be show at the
   --              program start
   -- SOURCE
   procedure CreateMainWindow(Directory: String);
   -- ****

   -- ****f* MainWindow/UpdateDirectoryList
   -- FUNCTION
   -- Update directory list
   -- PARAMETERS
   -- Clear - Clear current list of items
   -- SOURCE
   procedure UpdateDirectoryList(Clear: Boolean := False);
   -- ****

   -- ****t* MainWindow/UnboundedString_Container
   -- FUNCTION
   -- Used to store various Unbounded_String data in list.
   -- SOURCE
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   -- ****

   -- ****v* MainWindow/SelectedItems
   -- FUNCTION
   -- List of currently selected files and directories by user
   -- SOURCE
   SelectedItems: UnboundedString_Container.Vector;
   -- ****

   -- ****v* MainWindow/CurrentSelected
   -- FUNCTION
   -- Full path to currently selected file or directory
   -- SOURCE
   CurrentSelected: Unbounded_String;
   -- ****

   -- ****v* MainWindow/Setting
   -- FUNCTION
   -- If true, the program is in the setting mode
   -- SOURCE
--   Setting: Boolean;
   -- ****

   -- ****v* MainWindow/DestinationPath
   -- FUNCTION
   -- Full path to directory where selected files and directories will be
   -- copied or moved.
   -- SOURCE
--   DestinationPath: Unbounded_String;
   -- ****

   -- ****v* MainWindow/DirectoryView
   -- FUNCTION
   -- Gtk_Tree_View with current directory listing
   -- SOURCE
--   DirectoryView: Gtk_Tree_View;
   -- ****

   -- ****v* MainWindow/FilesPaned
   -- FUNCTION
   -- Gtk_Paned with current directory listing and preview UI
   -- SOURCE
--   FilesPaned: Gtk_Paned;
   -- ****

   -- ****v* MainWindow/TextEntry
   -- FUNCTION
   -- Gtk_GEntry used to create new files or directories or rename them
   -- SOURCE
--   TextEntry: Gtk_GEntry;
   -- ****

   -- ****v* MainWindow/FileStack
   -- FUNCTION
   -- Gtk_Stack with almost all the program UI
   -- SOURCE
--   FileStack: Gtk_Stack;
   -- ****

   -- ****v* MainWindow/MainWindow
   -- FUNCTION
   -- Main window of the program
   -- SOURCE
--   Window: Gtk_Window;
   -- ****

   -- ****v* MainWindow/Accelerators
   -- FUNCTION
   -- Keyboard accelerators associated with the main program window
   -- SOURCE
--   Accelerators: Gtk_Accel_Group;
   -- ****

end MainWindow;
