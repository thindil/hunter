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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Inotify is

   type Inotify_Events is
     (Accessed, Modified, Metadata, Closed_Write, Closed_No_Write, Opened,
      Moved_From, Moved_To, Created, Deleted, Deleted_Self, Moved_Self,
      Unmounted_Filesystem);

   for Inotify_Events use (Accessed => 16#0001#, Modified => 16#0002#,
      Metadata => 16#0004#, Closed_Write => 16#0008#,
      Closed_No_Write => 16#0010#, Opened => 16#0020#, Moved_From => 16#0040#,
      Moved_To => 16#0080#, Created => 16#0100#, Deleted => 16#0200#,
      Deleted_Self => 16#0400#, Moved_Self => 16#0800#,
      Unmounted_Filesystem => 16#2000#);

   type Event_Data is record
      Event: Inotify_Events;
      Target: Unbounded_String;
      Path: Unbounded_String;
   end record;

   package Events_Container is new Vectors(Positive, Event_Data);

   Events: Events_Container.Vector;

   procedure InotifyInit;
   procedure InotifyClose;
   procedure AddWatch(Path: String);
   procedure RemoveWatches;
   procedure InotifyRead;

end Inotify;
