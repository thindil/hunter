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
              (Ada.Directories.Compose(Path, FileName(1 .. Last)),
               Mask);
         end if;
         <<End_Of_Loop>>
      end loop;
      Close(Directory);
      return
        Result: constant Watch :=
          Instance(Object).Add_Watch(Path, Mask) do
         if not Object.Masks.Contains(Result.Watch) then
            Object.Masks.Insert(Result.Watch, Mask);
         end if;
      end return;
   end Add_Watch;

   overriding procedure Remove_Watch
     (Object: in out Recursive_Instance; Subject: Watch) is
     pragma Unreferenced(Subject);
   begin
      for I in Object.Watches.Iterate loop
         Instance(Object).Remove_Watch((Watch => Watch_Maps.Key(I)));
      end loop;
      Object.Masks.Clear;
      Instance(Object).Watches.Clear;
   end Remove_Watch;

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
            else
               Object.Add_Watch(To, Object.Masks.Element(Subject.Watch));
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
