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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* MainWindow/MainWindow
-- FUNCTION
-- Provide code to show and manipulate the main program window.
-- SOURCE
package MainWindow is
-- ****

   -- ****v* MainWindow/MainWindow.Current_Directory
   -- FUNCTION
   -- Currently selected directory to show
   -- SOURCE
   Current_Directory: Unbounded_String;
   -- ****

   -- ****t* MainWindow/MainWindow.Item_Actions
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
   type Item_Actions is
     (CREATEFILE, CREATEDIRECTORY, RENAME, DELETE, COPY, MOVE, OPENWITH,
      GOTOPATH, CREATELINK, CLEARTRASH, SHOWTRASH, DELETETRASH);
   -- ****

   -- ****d* MainWindow/MainWindow.Default_Item_Action
   -- FUNCTION
   -- Default action type for files and directories
   -- SOURCE
   Default_Item_Action: constant Item_Actions := CREATEFILE;
   -- ****

   -- ****v* MainWindow/MainWindow.New_Action
   -- FUNCTION
   -- Current performed action on files or directories
   -- SOURCE
   New_Action: Item_Actions;
   -- ****

   -- ****t* MainWindow/MainWindow.UnboundedString_Container
   -- FUNCTION
   -- Used to store various Unbounded_String data in list.
   -- SOURCE
   package UnboundedString_Container is new Vectors
     (Index_Type => Positive, Element_Type => Unbounded_String);
   -- ****

   -- ****v* MainWindow/MainWindow.Selected_Items
   -- FUNCTION
   -- List of currently selected files and directories by user
   -- SOURCE
   Selected_Items: UnboundedString_Container.Vector;
   -- ****

   -- ****v* MainWindow/MainWindow.Current_Selected
   -- FUNCTION
   -- Full path to currently selected file or directory
   -- SOURCE
   Current_Selected: Unbounded_String;
   -- ****

   -- ****f* MainWindow/MainWindow.Create_Main_Window
   -- FUNCTION
   -- Create main window and show content of selected directory
   -- PARAMETERS
   -- Directory  - Full path to the directory which will be show at the
   --              program start
   -- SOURCE
   procedure Create_Main_Window(Directory: String);
   -- ****

   -- ****f* MainWindow/MainWindow.Update_Directory_List
   -- FUNCTION
   -- Update directory list
   -- PARAMETERS
   -- Clear      - Clear current list of items
   -- Frame_Name - The name of the frame to update. Default value is
   --              "directory" which mean the frame with preview of the
   --              current directory
   -- SOURCE
   procedure Update_Directory_List
     (Clear: Boolean := False; Frame_Name: String := "directory");
   -- ****

end MainWindow;
