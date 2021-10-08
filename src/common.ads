-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

-- ****h* Common/Common
-- FUNCTION
-- Provide various common settings for graphical and console version of the
-- program
-- SOURCE
package Common is
-- ****

   -- ****v* Common/Common.Current_Directory
   -- FUNCTION
   -- Currently selected directory to show
   -- SOURCE
   Current_Directory: Unbounded_String;
   -- ****

   -- ****t* Common/Common.Item_Actions
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

   -- ****d* Common/Common.Default_Item_Action
   -- FUNCTION
   -- Default action type for files and directories
   -- SOURCE
   Default_Item_Action: constant Item_Actions := CREATEFILE;
   -- ****

   -- ****v* Common/Common.New_Action
   -- FUNCTION
   -- Current performed action on files or directories
   -- SOURCE
   New_Action: Item_Actions;
   -- ****

end Common;
