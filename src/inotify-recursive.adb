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

with Ada.Containers.Bounded_Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Inotify.Recursive is

   function "+"(Value: String) return SU.Unbounded_String renames
     SU.To_Unbounded_String;

   package Move_Vectors is new Ada.Containers.Bounded_Vectors(Positive, Move);

   overriding function Add_Watch
     (Object: in out Recursive_Instance; Path: String;
      Mask: Watch_Bits := All_Events) return Watch is
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
   begin
      if Depth = Count(Path, "/") - 2 then
         return (Watch => Interfaces.C.int(-1));
      end if;
      Open(Directory, Path);
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) in "." | ".." then
            goto End_Of_Loop;
         end if;
         if Is_Directory(Path & Directory_Separator & FileName(1 .. Last))
           and then Is_Read_Accessible_File
             (Path & Directory_Separator & FileName(1 .. Last)) then
            Object.Add_Watch
              (Ada.Directories.Compose(Path, FileName(1 .. Last)), Mask);
         end if;
         <<End_Of_Loop>>
      end loop;
      Close(Directory);
      return Instance(Object).Add_Watch(Path, Mask);
   end Add_Watch;

   procedure Remove_Watches(Object: in out Recursive_Instance; Path: String) is
      Cursor: Watch_Maps.Cursor;
   begin
      if Natural(Instance(Object).Watches.Length) < 2 then
         return;
      end if;
      Cursor := Instance(Object).Watches.First;
      loop
         if Instance(Object).Watches(Cursor) /= Path then
            Instance(Object).Remove_Watch((Watch => Watch_Maps.Key(Cursor)));
            Cursor := Instance(Object).Watches.First;
         else
            Watch_Maps.Next(Cursor);
         end if;
         exit when Natural(Instance(Object).Watches.Length) = 1;
      end loop;
   end Remove_Watches;

   overriding procedure Process_Events
     (Object: in out Recursive_Instance;
      Handle: not null access procedure
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String);
      Move_Handle: not null access procedure
        (Subject: Watch; Is_Directory: Boolean; From, To: String)) is
      Moves: Move_Vectors.Vector (Capacity => Object.Watches.Length);

      procedure Handle_Event
        (Subject: Inotify.Watch; Event: Inotify.Event_Kind;
         Is_Directory: Boolean; Name: String) is
      begin
         Handle(Subject, Event, Is_Directory, Name);
      end Handle_Event;

      procedure Handle_Move_Event
        (Subject: Watch; Is_Directory: Boolean; From, To: String) is
      begin
         Move_Handle(Subject, Is_Directory, From, To);

         if Is_Directory then
            if From /= "" then
               Moves.Append((+From, +To));
            end if;
         end if;
      end Handle_Move_Event;
   begin
      Instance(Object).Process_Events
        (Handle_Event'Access, Handle_Move_Event'Access);
   end Process_Events;

   overriding procedure Process_Events
     (Object: in out Recursive_Instance;
      Handle: not null access procedure
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String)) is
      procedure Move_Handle
        (Subject: Watch; Is_Directory: Boolean; From, To: String) is null;
   begin
      Object.Process_Events(Handle, Move_Handle'Access);
   end Process_Events;

end Inotify.Recursive;
