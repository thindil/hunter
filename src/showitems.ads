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

with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

-- ****h* Hunter/ShowItems
-- FUNCTION
-- Provide code to show informations and set some settings for selected files
-- or directories.
-- SOURCE
package ShowItems is
-- ****

   -- ****f* ShowItems/ScaleImage
   -- FUNCTION
   -- Scale currently previewed image
   -- SOURCE
   procedure ScaleImage;
   -- ****

      -- ****f* ShowItems/Show_Selected_Command
      -- FUNCTION
      -- Show preview or information about the currently selected file or
      -- directory after user select it in the directory view
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Show_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   -- ****f* ShowItems/CreateShowItemsUI
   -- FUNCTION
   -- Create UI related to show items and destination for moving/copying
   -- items.
   -- SOURCE
   procedure CreateShowItemsUI;
   -- ****

end ShowItems;
