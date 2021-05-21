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

package body AboutDialog is

   procedure Show_About_Dialog is
   begin
      null;
   end Show_About_Dialog;

   function About_View_Keys(Key: Key_Code) return UI_Locations is
      pragma Unreferenced(Key);
   begin
      return ABOUT_FORM;
   end About_View_Keys;

end AboutDialog;
