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

with Gtkada.Builder; use Gtkada.Builder;

package ShowItems is

   -- ****f* ShowItems/ShowItemInfo
   -- FUNCTION
   -- Show detailed information (name, size, modification date, etc) about
   -- selected file or directory.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* ShowItems/PreviewItem
   -- FUNCTION
   -- Preview selected file or directory. If preview is not available, show
   -- info about selected file or directory.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure PreviewItem(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* ShowItems/ShowItem
   -- FUNCTION
   -- Show info about selected item or preview it.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ShowItem(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* MainWindow/SetAssociated
   -- FUNCTION
   -- Set associated program with selected file MIME type
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure SetAssociated(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* ShowItems/SetPermission
   -- FUNCTION
   -- Set selected permissions to selected file or directory
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure SetPermission(Object: access Gtkada_Builder_Record'Class);
   -- ****
end ShowItems;
