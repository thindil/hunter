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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.GEntry; use Gtk.GEntry;
with Gdk.Event; use Gdk.Event;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Hunter/CreateItems
-- FUNCTION
-- Provide code for create files and directories.
-- SOURCE
package CreateItems is
-- ****

   -- ****v* CreateItems/LinkTarget
   -- FUNCTION
   -- Destination for new symbolic link
   -- SOURCE
   LinkTarget: Unbounded_String;
   -- ****

   -- ****f* CreateItems/AddNew
   -- FUNCTION
   -- Show text entry for enter new file/directory name
   -- PARAMETERS
   -- User_Data - Which menu option was selected (create file or directory)
   -- SOURCE
   procedure AddNew(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* CreateItems/IconPressed
   -- FUNCTION
   -- Create new file or directory when user press icon or hide text entry
   -- PARAMETERS
   -- Self     - Text entry with name for new file/directory
   -- Icon_Pos - Position of text entry icon which was pressed
   -- SOURCE
   procedure IconPressed
     (Self: access Gtk_Entry_Record'Class; Icon_Pos: Gtk_Entry_Icon_Position;
      Event: Gdk_Event_Button);
   -- ****
   -- ****f* CreateItems/CreateNew
   -- FUNCTION
   -- Create new file or directory when user press enter in text entry
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure CreateNew(Object: access Gtkada_Builder_Record'Class);
   -- ****

end CreateItems;
