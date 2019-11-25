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

with System;

with Ada.Streams;
with Ada.Unchecked_Conversion;

with Ada.Containers.Bounded_Vectors;

package body Inotify is

   use type GNAT.OS_Lib.File_Descriptor;

   overriding procedure Initialize (Object : in out Instance) is
      function Inotify_Init return GNAT.OS_Lib.File_Descriptor
        with Import, Convention => C, External_Name => "inotify_init";
   begin
      Object.Instance := Inotify_Init;

      if Object.Instance = GNAT.OS_Lib.Invalid_FD then
         raise Program_Error;
      end if;
   end Initialize;

   overriding procedure Finalize (Object : in out Instance) is
      Status : Boolean;
   begin
      if Object.Instance /= GNAT.OS_Lib.Invalid_FD then
         GNAT.OS_Lib.Close (Object.Instance, Status);
         Object.Instance := GNAT.OS_Lib.Invalid_FD;

         if not Status then
            raise Program_Error;
         end if;
      end if;
   end Finalize;

   use type Interfaces.C.int;

   function Add_Watch
     (Object : in out Instance;
      Path   :        String;
      Mask   :        Watch_Bits := All_Events) return Watch
   is
      function Inotify_Add_Watch
        (Instance : GNAT.OS_Lib.File_Descriptor;
         Path : Interfaces.C.char_array;
         Mask : Interfaces.C.unsigned) return Interfaces.C.int
      with Import, Convention => C, External_Name => "inotify_add_watch";

      function Convert is new Ada.Unchecked_Conversion
        (Source => Watch_Bits, Target => Interfaces.C.unsigned);

      Result : constant Interfaces.C.int := Inotify_Add_Watch
        (Object.Instance, Interfaces.C.To_C (Path), Convert (Mask));
   begin
      if Result = -1 then
         raise Program_Error;
      end if;

      Object.Watches.Include (Result, Path);
      return (Watch => Result);
   end Add_Watch;

   procedure Add_Watch
     (Object : in out Instance;
      Path   :        String;
      Mask   :        Watch_Bits := All_Events)
   is
      Result : constant Watch := Instance'Class (Object).Add_Watch (Path, Mask);
   begin
      pragma Assert (Result.Watch /= -1);
   end Add_Watch;

   procedure Remove_Watch (Object : in out Instance; Subject : Watch) is
      function Inotify_Remove_Watch
        (Instance : GNAT.OS_Lib.File_Descriptor;
         Watch    : Interfaces.C.int) return Interfaces.C.int
      with Import, Convention => C, External_Name => "inotify_rm_watch";
   begin
      if Inotify_Remove_Watch (Object.Instance, Subject.Watch) = -1 then
         raise Program_Error;
      end if;

      Object.Watches.Delete (Subject.Watch);
   end Remove_Watch;

   function Name (Object : Instance; Subject : Watch) return String is
     (Object.Watches.Element (Subject.Watch));

   -----------------------------------------------------------------------------

   type Inotify_Event is record
      Watch  : Interfaces.C.int;  -- -1 if event queue has overflowed
      Mask   : Interfaces.C.unsigned;
      Cookie : Interfaces.C.unsigned;
      Length : Interfaces.C.unsigned;
   end record
     with Convention => C,
          Alignment  => 4;

   type Event_Bits is record
      Event : Event_Kind;

      Queue_Overflowed : Boolean := False;
      Ignored          : Boolean := False;
      Is_Directory     : Boolean := False;
   end record;

   for Event_Bits use record
      Event at 0 range 0 .. 13;

      Queue_Overflowed at 0 range 14 .. 14;
      Ignored          at 0 range 15 .. 15;
      Is_Directory     at 0 range 30 .. 30;
   end record;
   for Event_Bits'Size use Interfaces.C.unsigned'Size;
   for Event_Bits'Alignment use Interfaces.C.unsigned'Alignment;

   type Cookie_Move_Pair is record
      Key   : Interfaces.C.unsigned;
      Value : Move;
   end record;

   package Move_Vectors is new Ada.Containers.Bounded_Vectors
     (Index_Type   => Positive,
      Element_Type => Cookie_Move_Pair);

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
         From, To     : String))
   is
      use Ada.Streams;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Stream_Element_Array, Target => Inotify_Event);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned, Target => Event_Bits);

      Event_In_Bytes : constant Stream_Element_Offset
        := Inotify_Event'Size / System.Storage_Unit;

      Length : Stream_Element_Offset;
      Buffer : Stream_Element_Array (1 .. 4096)
        with Alignment => 4;

      Moves : Move_Vectors.Vector (Capacity => 8);
      --  A small container to hold pairs of cookies and files that are moved
      --
      --  The container needs to be bounded because files that are moved
      --  to outside the monitored folder do not generate Moved_To events.
      --  A vector is used instead of a map so that the oldest pair can
      --  be deleted if the container is full.

      function Find_Move (Cookie : Interfaces.C.unsigned) return Move_Vectors.Cursor is
         Cursor : Move_Vectors.Cursor := Move_Vectors.No_Element;

         procedure Reverse_Iterate (Position : Move_Vectors.Cursor) is
            use type Interfaces.C.unsigned;
         begin
            if Cookie = Moves (Position).Key then
               Cursor := Position;
            end if;
         end Reverse_Iterate;
      begin
         Moves.Reverse_Iterate (Reverse_Iterate'Access);
         return Cursor;
      end Find_Move;

      use type Ada.Containers.Count_Type;
   begin
      while not Object.Watches.Is_Empty loop
         Length := Stream_Element_Offset (GNAT.OS_Lib.Read
           (Object.Instance, Buffer'Address, Buffer'Length));

         if Length = -1 then
            raise Read_Error;
         end if;

         if Length > 0 then
            declare
               Index : Stream_Element_Offset := Buffer'First;
            begin
               while Index < Buffer'First + Length loop
                  declare
                     Event : constant Inotify_Event
                       := Convert (Buffer (Index .. Index + Event_In_Bytes - 1));
                     Mask  : constant Event_Bits := Convert (Event.Mask);

                     Name_Length : constant Stream_Element_Offset
                       := Stream_Element_Offset (Event.Length);
                  begin
                     if Mask.Queue_Overflowed then
                        raise Queue_Overflow_Error;
                     end if;
                     pragma Assert (Event.Watch /= -1);

                     if Mask.Ignored then
                        Object.Watches.Exclude (Event.Watch);
                     else
                        declare
                           Directory : constant String := Object.Watches.Element (Event.Watch);
                        begin
                           if Name_Length > 0 then
                              declare
                                 subtype Name_Array is Interfaces.C.char_array
                                  (1 .. Interfaces.C.size_t (Event.Length));
                                 subtype Name_Buffer is Stream_Element_Array
                                  (1 .. Name_Length);

                                 function Convert is new Ada.Unchecked_Conversion
                                   (Source => Name_Buffer, Target => Name_Array);

                                 Name_Index : constant Stream_Element_Offset
                                   := Index + Event_In_Bytes;

                                 Name : constant String := Interfaces.C.To_Ada (Convert
                                   (Buffer (Name_Index .. Name_Index + Name_Length - 1)));
                              begin
                                 Handle
                                   ((Watch => Event.Watch), Mask.Event,
                                    Mask.Is_Directory, Directory & "/" & Name);

                                 case Mask.Event is
                                    when Moved_From =>
                                       if Moves.Length = Moves.Capacity then
                                          Moves.Delete_First;
                                       end if;
                                       Moves.Append ((Event.Cookie,
                                         (From => SU.To_Unbounded_String (Directory & "/" & Name),
                                          To   => <>)));
                                       --  If inode is moved to outside watched directory,
                                       --  then there will never be a Moved_To or Moved_Self
                                       --  if instance is not recursive
                                    when Moved_To =>
                                       declare
                                          Cursor : Move_Vectors.Cursor := Find_Move (Event.Cookie);
                                          use type Move_Vectors.Cursor;
                                       begin
                                          if Cursor /= Move_Vectors.No_Element then
                                             --  It's a rename
                                             Move_Handle
                                               (Subject      => (Watch => Event.Watch),
                                                Is_Directory => Mask.Is_Directory,
                                                From => SU.To_String (Moves (Cursor).Value.From),
                                                To   => Directory & "/" & Name);
                                             Moves.Delete (Cursor);
                                          else
                                             Move_Handle
                                               (Subject      => (Watch => Event.Watch),
                                                Is_Directory => Mask.Is_Directory,
                                                From => "",
                                                To   => Directory & "/" & Name);
                                          end if;
                                       end;
                                    when others =>
                                       null;
                                 end case;
                              end;
                           else
                              Handle
                                ((Watch => Event.Watch), Mask.Event,
                                 Mask.Is_Directory, Directory);
                           end if;
                        end;
                     end if;

                     Index := Index + Event_In_Bytes + Name_Length;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Process_Events;

   procedure Process_Events
     (Object : in out Instance;
      Handle :        not null access procedure
        (Subject      : Watch;
         Event        : Event_Kind;
         Is_Directory : Boolean;
         Name         : String))
   is
      procedure Move_Handle
        (Subject      : Watch;
         Is_Directory : Boolean;
         From, To     : String) is null;
   begin
      Object.Process_Events (Handle, Move_Handle'Access);
   end Process_Events;

end Inotify;
