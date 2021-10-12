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
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;

-- ****h* ProgramsMenu/ProgramsMenu
-- FUNCTION
-- Provides common code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu is
-- ****

   -- ****f* ProgramsMenu/ProgramsMenu.GetProgramName
   -- FUNCTION
   -- Search for name of application associated with selected desktop file
   -- PARAMETERS
   -- DesktopFile - File name of .desktop file to search
   -- RESULT
   -- Name of the application associated with the selected desktop file or
   -- DesktopFile if application was not found
   -- SOURCE
   function GetProgramName(DesktopFile: String) return String;
   -- ****

   -- ****f* ProgramsMenu/ProgramsMenu.CreateProgramsMenu
   -- FUNCTION
   -- Create associated programs menu list
   -- SOURCE
   procedure CreateProgramsMenu;
   -- ****

private

   -- ****iv* ProgramsMenu/ProgramsMenu.ApplicationsList
   -- FUNCTION
   -- List of all applications which can be used to execute files or
   -- directories
   -- SOURCE
   ApplicationsList: Bookmarks_Container.Map;
   -- ****

   -- ****iv* ProgramsMenu/ProgramsMenu.NamesList
   -- FUNCTION
   -- List of all applications showed in the menu
   -- SOURCE
   NamesList: UnboundedString_Container.Vector;
   -- ****

   -- ****it* ProgramsMenu/ProgramsMenu.Programs_Sorting
   -- FUNCTION
   -- Used in sorting available programs
   -- SOURCE
   package Programs_Sorting is new UnboundedString_Container.Generic_Sorting;
   -- ****

end ProgramsMenu;
