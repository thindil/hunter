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

-- ****h* Libmagic/LibMagic
-- FUNCTION
-- Small Ada binding to the libmagic library
-- SOURCE
package LibMagic is
-- ****

   -- ****f* LibMagic/MagicOpen
   -- FUNCTION
   -- Initialize magic data
   -- SOURCE
   procedure MagicOpen;
   -- ****

   -- ****f* LibMagic/MagicFile
   -- FUNCTION
   -- Get mime type of selected file if libmagic is not initialized, it
   -- fallback to the xdg-mime program.
   -- PARAMETERS
   -- Name - Full path to the file which mime type will be check
   -- RESULT
   -- MIME type of selected file or "unknown" if libmagic is not initialized
   -- and xdg-mime program don't exists.
   -- SOURCE
   function MagicFile(Name: String) return String;
   -- ****

   -- ****f* LibMagic/MagicClose
   -- FUNCTION
   -- Close and release all magic data
   -- SOURCE
   procedure MagicClose;
   -- ****

end LibMagic;
