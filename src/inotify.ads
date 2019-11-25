--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

private with Interfaces.C;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;

private with GNAT.OS_Lib;

package Inotify is
   pragma Preelaborate;

   type Watch_Bits is record
      Accessed        : Boolean := False;
      Modified        : Boolean := False;
      Metadata        : Boolean := False;
      Closed_Write    : Boolean := False;
      Closed_No_Write : Boolean := False;
      Opened          : Boolean := False;
      Moved_From      : Boolean := False;
      Moved_To        : Boolean := False;
      Created         : Boolean := False;
      Deleted         : Boolean := False;
      Deleted_Self    : Boolean := False;
      Moved_Self      : Boolean := False;

      Only_Directories : Boolean := False;
      Do_Not_Follow    : Boolean := False;
      Exclude_Unlinked : Boolean := False;
      Add_Mask         : Boolean := False;
      One_Shot         : Boolean := False;
   end record;

   All_Events : constant Watch_Bits;

   type Instance is tagged limited private;

   type Watch is private;

   procedure Add_Watch
     (Object : in out Instance;
      Path   :        String;
      Mask   :        Watch_Bits := All_Events);

   function Add_Watch
     (Object : in out Instance;
      Path   :        String;
      Mask   :        Watch_Bits := All_Events) return Watch;

   procedure Remove_Watch (Object : in out Instance; Subject : Watch);

   function Name (Object : Instance; Subject : Watch) return String;

   type Event_Kind is
     (Accessed,
      Modified,
      Metadata,
      Closed_Write,
      Closed_No_Write,
      Opened,
      Moved_From,
      Moved_To,
      Created,
      Deleted,
      Deleted_Self,
      Moved_Self,
      Unmounted_Filesystem);

   procedure Process_Events
     (Object : in out Instance;
      Handle :        not null access procedure
        (Subject      : Watch;
         Event        : Event_Kind;
         Is_Directory : Boolean;
         Name         : String));
   --  Wait and process events
   --
   --  If reading an event fails, a Read_Error is raised. If the event queue
   --  overflowed then Queue_Overflow_Error is raised.

   procedure Process_Events
     (Object : in out Instance;
      Handle :        not null access procedure
        (Subject      : Watch;
         Event        : Event_Kind;
         Is_Directory : Boolean;
         Name         : String);
      Move_Handle : not null access procedure
        (Subject      : Watch;
         Is_Directory : Boolean;
         From, To     : String));
   --  Wait and process events
   --
   --  Move_Handle is called after matching Moved_From and Moved_To events
   --  have been processed. To be effective, Add_Watch must have been called
   --  with a mask containing these two events.
   --
   --  If reading an event fails, a Read_Error is raised. If the event queue
   --  overflowed then Queue_Overflow_Error is raised.

   Read_Error           : exception;
   Queue_Overflow_Error : exception;

private

   for Event_Kind use
     (Accessed             => 16#0001#,
      Modified             => 16#0002#,
      Metadata             => 16#0004#,
      Closed_Write         => 16#0008#,
      Closed_No_Write      => 16#0010#,
      Opened               => 16#0020#,
      Moved_From           => 16#0040#,
      Moved_To             => 16#0080#,
      Created              => 16#0100#,
      Deleted              => 16#0200#,
      Deleted_Self         => 16#0400#,
      Moved_Self           => 16#0800#,
      Unmounted_Filesystem => 16#2000#);
   for Event_Kind'Size use 14;

   for Watch_Bits use record
      Accessed        at 0 range  0 ..  0;
      Modified        at 0 range  1 ..  1;
      Metadata        at 0 range  2 ..  2;
      Closed_Write    at 0 range  3 ..  3;
      Closed_No_Write at 0 range  4 ..  4;
      Opened          at 0 range  5 ..  5;
      Moved_From      at 0 range  6 ..  6;
      Moved_To        at 0 range  7 ..  7;
      Created         at 0 range  8 ..  8;
      Deleted         at 0 range  9 ..  9;
      Deleted_Self    at 0 range 10 .. 10;
      Moved_Self      at 0 range 11 .. 11;

      Only_Directories at 0 range 24 .. 24;
      Do_Not_Follow    at 0 range 25 .. 25;
      Exclude_Unlinked at 0 range 26 .. 26;
      Add_Mask         at 0 range 29 .. 29;
      One_Shot         at 0 range 31 .. 31;
   end record;
   for Watch_Bits'Size use Interfaces.C.unsigned'Size;
   for Watch_Bits'Alignment use Interfaces.C.unsigned'Alignment;

   All_Events : constant Watch_Bits :=
     (Accessed        |
      Modified        |
      Metadata        |
      Closed_Write    |
      Closed_No_Write |
      Opened          |
      Moved_From      |
      Moved_To        |
      Created         |
      Deleted         |
      Deleted_Self    |
      Moved_Self      =>
        True,
      others => False);

   -----------------------------------------------------------------------------

   package SU renames Ada.Strings.Unbounded;

   type Move is record
      From, To : SU.Unbounded_String;
   end record;

   -----------------------------------------------------------------------------

   function Hash (Key : Interfaces.C.int) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Key));

   package Watch_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Interfaces.C.int,
      Element_Type    => String,
      Hash            => Hash,
      Equivalent_Keys => Interfaces.C."=");

   type Instance is limited new Ada.Finalization.Limited_Controlled with record
      Instance : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Watches  : Watch_Maps.Map;
   end record;

   overriding procedure Initialize (Object : in out Instance);

   overriding procedure Finalize (Object : in out Instance);

   type Watch is record
      Watch : Interfaces.C.int;
   end record;

end Inotify;
