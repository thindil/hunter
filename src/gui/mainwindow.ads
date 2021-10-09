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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* MainWindow/MainWindow
-- FUNCTION
-- Provide code to show and manipulate the main program window.
-- SOURCE
package MainWindow is
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
