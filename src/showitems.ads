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

--with Gtk.Stack; use Gtk.Stack;
--with Gtk.Tool_Button; use Gtk.Tool_Button;
--with Gtk.Tree_Selection; use Gtk.Tree_Selection;

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

   -- ****f* ShowItems/CreateShowItemsUI
   -- FUNCTION
   -- Create UI related to show items and destination for moving/copying
   -- items.
   -- SOURCE
   procedure CreateShowItemsUI;
   -- ****

end ShowItems;
