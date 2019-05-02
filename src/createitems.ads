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

with Gtk.GEntry; use Gtk.GEntry;
with Gdk.Event; use Gdk.Event;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;

package CreateItems is

   -- Create new file or directory with selected name
   procedure CreateItem(Self: access Gtk_Entry_Record'Class;
      Icon_Pos: Gtk_Entry_Icon_Position);
   -- Show text entry for enter new file/directory name
   procedure AddNew(User_Data: access GObject_Record'Class);
   -- Create new file or directory when user press icon
   procedure IconPressed(Self: access Gtk_Entry_Record'Class;
      Icon_Pos: Gtk_Entry_Icon_Position; Event: Gdk_Event_Button);
   -- Create new file or directory when user press enter in text entry
   procedure CreateNew(Object: access Gtkada_Builder_Record'Class);

end CreateItems;
