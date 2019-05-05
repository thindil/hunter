-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Gtkada.Builder; use Gtkada.Builder;

package ErrorDialog is

   -- ****f* ErrorDialog/SaveException
   -- FUNCTION
   -- Save exception data to file and show error dialog
   -- PARAMETERS
   -- An_Exception    - Exception's data which occured
   -- PrintToTerminal - If true, information about exception will be printed
   --                   in terminal
   -- SOURCE
   procedure SaveException(An_Exception: Exception_Occurrence;
      PrintToTerminal: Boolean);
   -- ****
   -- ****f* ErrorDialog/On_Exception
   -- FUNCTION
   -- Handle GUI exceptions
   -- PARAMETERS
   -- An_Exception - Exception's data which occured
   -- SOURCE
   procedure On_Exception(An_Exception: Exception_Occurrence);
   -- ****
   -- ****f* ErrorDialog/CreateErrorDialog
   -- FUNCTION
   -- Create error dialog UI
   -- PARAMETERS
   -- NewBuilder: Gtk Builder with UI data read from .glade file
   -- SOURCE
   procedure CreateErrorDialog(NewBuilder: Gtkada_Builder);
   -- ****

end ErrorDialog;
