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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Inotify/Inotify
-- FUNCTION
-- Small Ada binding to kernel inotify
-- SOURCE
package Inotify is
-- ****

   -- ****t* Inotify/Inotify.Inotify_Events
   -- FUNCTION
   -- Types of inotify events and values for them
   -- SOURCE
   type Inotify_Events is
     (ACCESSED, MODIFIED, METADATA, CLOSED_WRITE, CLOSED_NO_WRITE, OPENED,
      MOVED_FROM, MOVED_TO, CREATED, DELETED, DELETED_SELF, MOVED_SELF,
      UNMOUNTED_FILESYSTEM);

   for Inotify_Events use (ACCESSED => 16#0001#, MODIFIED => 16#0002#,
      METADATA => 16#0004#, CLOSED_WRITE => 16#0008#,
      CLOSED_NO_WRITE => 16#0010#, OPENED => 16#0020#, MOVED_FROM => 16#0040#,
      MOVED_TO => 16#0080#, CREATED => 16#0100#, DELETED => 16#0200#,
      DELETED_SELF => 16#0400#, MOVED_SELF => 16#0800#,
      UNMOUNTED_FILESYSTEM => 16#2000#);
   -- ****

   -- ****d* Inotify/Inotify.Accesed_Event
   -- FUNCTION
   -- Default Inotify event
   -- SOURCE
   Accessed_Event: constant Inotify_Events := ACCESSED;
   -- ****

   -- ****s* Inotify/Inotify.Event_Data
   -- FUNCTION
   -- Data structure used to store information about inotify event
   -- OPTIONS
   -- Event  - inotify event
   -- Target - target file or directory
   -- Path   - path in which event occurs
   -- SOURCE
   type Event_Data is record
      Event: Inotify_Events;
      Target: Unbounded_String;
      Path: Unbounded_String;
   end record;
   -- ****

   -- ****d* Inotify/Inotify.Empty_Event_Data
   -- FUNCTION
   -- Empty event data record
   -- SOURCE
   Empty_Event_Data: constant Event_Data := Event_Data'(others => <>);
   -- ****

   -- ****t* Inotify/Inotify.Events_Container
   -- FUNCTION
   -- Used to store data about inotify events
   -- SOURCE
   package Events_Container is new Vectors(Index_Type => Positive,
      Element_Type => Event_Data);
   -- ****

   -- ****v* Inotify/Inotify.Events_List
   -- FUNCTION
   -- List of all caught inotify events
   -- SOURCE
   Events_List: Events_Container.Vector;
   -- ****

   -- ****v* Inotify/Inotify.Temporary_Stop
   -- FUNCTION
   -- If true, temporary stop refreshing directory listing (mainly in trash).
   -- Default is false
   -- SOURCE
   Temporary_Stop: Boolean := False;
   -- ****

   -- ****f* Inotify/Inotify.Inotify_Init
   -- FUNCTION
   -- Start inotify instance
   -- SOURCE
   procedure Inotify_Init;
   -- ****

   -- ****f* Inotify/Inotify.Inotify_Close
   -- FUNCTION
   -- Close inotify instance
   -- SOURCE
   procedure Inotify_Close;
   -- ****

   -- ****f* Inotify/Inotify.Add_Watches
   -- FUNCTION
   -- Add inotify watches for selected path and all subdirectories inside it,
   -- but not deeper.
   -- PARAMETERS
   -- Path - Full path to which inotify watches will be added
   -- SOURCE
   procedure Add_Watches(Path: String);
   -- ****

   -- ****f* Inotify/Inotify.Remove_Watches
   -- FUNCTION
   -- Remove all inotify watches
   -- SOURCE
   procedure Remove_Watches;
   -- ****

   -- ****f* Inotify/Inotify.Inotify_Read
   -- FUNCTION
   -- Read all caught and waiting inotify events and store them in EventsList.
   -- If need, remove and add new watches also.
   -- SOURCE
   procedure Inotify_Read;
   -- ****

end Inotify;
