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

package Inotify.Recursive is

   type Recursive_Instance is limited new Instance with private;

   Depth: Positive;

   overriding function Add_Watch
     (Object: in out Recursive_Instance; Path: String;
      Mask: Watch_Bits := All_Events) return Watch;
   --  Watch the given path and any subdirectories
   --
   --  If the user has no permission to read the folder at the given path,
   --  then a Use_Error is raised. Any subdirectory, however, is ignored
   --  if it is not readable.

   procedure Remove_Watches
     (Object: in out Recursive_Instance);

   overriding procedure Process_Events
     (Object: in out Recursive_Instance;
      Handle: not null access procedure
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String));

   overriding procedure Process_Events
     (Object: in out Recursive_Instance;
      Handle: not null access procedure
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String);
      Move_Handle: not null access procedure
        (Subject: Watch; Is_Directory: Boolean; From, To: String));

private

   package Mask_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Interfaces.C.int, Element_Type => Watch_Bits, Hash => Hash,
      Equivalent_Keys => Interfaces.C."=");

   type Recursive_Instance is limited new Instance with record
      Masks: Mask_Maps.Map;
   end record;

end Inotify.Recursive;
