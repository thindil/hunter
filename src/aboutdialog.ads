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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl;

-- ****h* AboutDialog/AboutDialog
-- FUNCTION
-- Provide code for information about the program
-- SOURCE
package AboutDialog is
-- ****

   Version_Number: constant String := "1.7";
   procedure Set_About_Dialog_Information(Interp: Tcl.Tcl_Interp);

private

   Copyright: constant String := "© Bartek Jasicki 2019-2022";
   Website: constant String := "https://www.laeran.pl/repositories/hunter/";
   Programmer: constant String := "Bartek Jasicki <thindil@laeran.pl>";
   Translator: constant String :=
     "Polski - Bartek Jasicki <thindil@laeran.pl>";
   --## rule off GLOBAL_REFERENCES
   Translators_Text: Unbounded_String;
   Programmers_Text: Unbounded_String;
   License: Unbounded_String;
   Close_Text: Unbounded_String;
   Version: Unbounded_String;
   Website_Text: Unbounded_String;
   --## rule on GLOBAL_REFERENCES

end AboutDialog;
