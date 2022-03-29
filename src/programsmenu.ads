-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

--## rule off REDUCEABLE_SCOPE
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--## rule on REDUCEABLE_SCOPE
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;

-- ****h* ProgramsMenu/ProgramsMenu
-- FUNCTION
-- Provides common code for manipulate associated programs with files.
-- SOURCE
package ProgramsMenu is
-- ****

   -- ****f* ProgramsMenu/ProgramsMenu.Get_Program_Name
   -- FUNCTION
   -- Search for name of application associated with selected desktop file
   -- PARAMETERS
   -- Desktop_File - File name of .desktop file to search
   -- RESULT
   -- Name of the application associated with the selected desktop file or
   -- DesktopFile if application was not found
   -- SOURCE
   function Get_Program_Name(Desktop_File: String) return String;
   -- ****

   -- ****f* ProgramsMenu/ProgramsMenu.Create_Programs_Menu
   -- FUNCTION
   -- Create associated programs menu list
   -- SOURCE
   procedure Create_Programs_Menu;
   -- ****

private

   --## rule off GLOBAL_REFERENCES
   -- ****iv* ProgramsMenu/ProgramsMenu.Applications_List
   -- FUNCTION
   -- List of all applications which can be used to execute files or
   -- directories
   -- SOURCE
   Applications_List: Bookmarks_Container.Map;
   -- ****

   -- ****iv* ProgramsMenu/ProgramsMenu.Names_List
   -- FUNCTION
   -- List of all applications showed in the menu
   -- SOURCE
   Names_List: UnboundedString_Container.Vector;
   -- ****
   --## rule on GLOBAL_REFERENCES

end ProgramsMenu;
