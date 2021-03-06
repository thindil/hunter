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

with Ada.Exceptions; use Ada.Exceptions;

-- ****h* ErrorDialog/ErrorDialog
-- FUNCTION
-- Provide code to show informations about program crash (unhandled exception).
-- SOURCE
package ErrorDialog is
-- ****

   -- ****
   -- ****f* ErrorDialog/ErrorDialog.SaveException
   -- FUNCTION
   -- Save exception data to file and show error dialog
   -- PARAMETERS
   -- An_Exception    - Exception's data which occured
   -- SOURCE
   procedure Save_Exception(An_Exception: Exception_Occurrence);
   -- ****

end ErrorDialog;
