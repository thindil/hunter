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

with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with RefreshData; use RefreshData;

package body Inotify is

   -- ****iv* Inotify/Instance
   -- FUNCTION
   -- inotify instance to read
   -- SOURCE
   Instance: File_Descriptor;
   -- ****

   -- ****it* Inotify/Watch_Data
   -- FUNCTION
   -- Data structure for inotify watches
   -- OPTIONS
   -- Id   - Id of the inotify watch
   -- Path - Full path which the inotify watch is watching
   -- SOURCE
   type Watch_Data is record
      Id: int;
      Path: Unbounded_String;
   end record;
   -- ****

   -- ****it* Inotify/Watches_Container
   -- FUNCTION
   -- Used to store information about inotify watches
   -- SOURCE
   package Watches_Container is new Vectors(Positive, Watch_Data);
   -- ****

   -- ****iv* Inotify/Watches
   -- FUNCTION
   -- Stores information about all active inotify watches
   -- SOURCE
   Watches: Watches_Container.Vector;
   -- ****

   -- ****it* Inotify/Mask_Array
   -- FUNCTION
   -- Array of inotify events to catch
   -- SOURCE
   type Mask_Array is array(Positive range <>) of Inotify_Events;
   -- ****

   -- ****if* Inotify/inotify_init
   -- FUNCTION
   -- Binding to the C function
   -- RESULT
   -- File descriptor used for a newly created inotify instance
   -- SOURCE
   function inotify_init return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_init";
      -- ****

      -- ****if* Inotify/inotify_add_watch
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- fd       - inotify instance to which add a new watch
      -- pathname - full path to which a new inotify watch will be added
      -- mask     - list of inotify events to listen
      -- RESULT
      -- -1 if inotify watch cannot be added, 0 if exists, otherwise index of
      -- newly created watch
      -- SOURCE
   function inotify_add_watch
     (fd: int; pathname: chars_ptr; mask: int) return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_add_watch";
      -- ****

      -- ****if* Inotify/inotify_rm_watch
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- fd - inotify instance from which selected watch will be removed
      -- wd - inotify watch index to remove
      -- RESULT
      -- 0 if inotify watch was succesfully removed, -1 when error occurs
      -- SOURCE
   function inotify_rm_watch(fd, wd: int) return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_rm_watch";
      -- ****

   procedure InotifyInit is
   begin
      Instance := File_Descriptor(inotify_init);
   end InotifyInit;

   procedure InotifyClose is
   begin
      Close(Instance);
   end InotifyClose;

   -- ****if* Inotify/CreateMask
   -- FUNCTION
   -- Convert list of inotify events to mask value for C
   -- PARAMETERS
   -- Events - list of inotify events to convert
   -- RESULT
   -- Mask value for C
   -- SOURCE
   function CreateMask(Events: Mask_Array) return int is
      -- ****
      type Unsigned_Integer is mod 2**Integer'Size;
      Mask: Unsigned_Integer :=
        Unsigned_Integer(Inotify_Events'Enum_Rep(Events(1)));
   begin
      for I in 2 .. Events'Last loop
         Mask := Mask or Unsigned_Integer(Inotify_Events'Enum_Rep(Events(I)));
      end loop;
      return int(Mask);
   end CreateMask;

   -- ****if* Inotify/AddWatch
   -- FUNCTION
   -- Add inotify watch to selected path
   -- PARAMETERS
   -- Path - Full path to which new inotify watch will be added
   -- SOURCE
   procedure AddWatch(Path: String) is
      -- ****
      Watch: int;
      Mask: constant int :=
        CreateMask((Metadata, Moved_From, Moved_To, Deleted, Created));
   begin
      Watch := inotify_add_watch(int(Instance), New_String(Path), Mask);
      if Watch > 0 then
         Watches.Append((Watch, To_Unbounded_String(Path)));
      end if;
   end AddWatch;

   procedure AddWatches(Path: String) is
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
   begin
      AddWatch(Path);
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
            AddWatch(Path & "/" & FileName(1 .. Last));
         end if;
         <<End_Of_Loop>>
      end loop;
      Close(Directory);
   end AddWatches;

   procedure RemoveWatches is
   begin
      for Watch of Watches loop
         if inotify_rm_watch(int(Instance), Watch.Id) = -1 then
            null;
         end if;
      end loop;
      Watches.Clear;
   end RemoveWatches;

   -- ****if* Inotify/RemoveWatch
   -- FUNCTION
   -- Remove selected watch
   -- PARAMETERS
   -- Path - Full path from which inotify watch will be removed
   -- SOURCE
   procedure RemoveWatch(Path: String) is
      -- ****
   begin
      for Watch of Watches loop
         if To_String(Watch.Path) = Path then
            if inotify_rm_watch(int(Instance), Watch.Id) = -1 then
               null;
            end if;
            exit;
         end if;
      end loop;
   end RemoveWatch;

   procedure InotifyRead is
      Buffer: array(1 .. 4096) of Character;
      Length, NameLength, Start: Integer;
      Path, Target: Unbounded_String;
      Event: Inotify_Events;
      Added: Boolean;
   begin
      loop
         Length := Read(Instance, Buffer'Address, 4096);
         if Length = -1 then
            exit;
         end if;
         if TemporaryStop then
            goto End_Of_Loop;
         end if;
         Start := 1;
         loop
            for Watch of Watches loop
               if int(Character'Pos(Buffer(Start))) = Watch.Id then
                  Path := Watch.Path;
                  NameLength := Character'Pos(Buffer(Start + 12)) + 15 + Start;
                  Target := Null_Unbounded_String;
                  for I in Start + 16 .. NameLength loop
                     exit when Character'Pos(Buffer(I)) = 0;
                     Append(Target, Buffer(I));
                  end loop;
                  exit;
               end if;
            end loop;
            if Character'Pos(Buffer(Start + 4)) > 0 then
               Event :=
                 Inotify_Events'Enum_val(Character'Pos(Buffer(Start + 4)));
            else
               Event :=
                 Inotify_Events'Enum_val(Character'Pos(Buffer(Start + 5)));
            end if;
            Added := False;
            for Event2 of EventsList loop
               if Event2.Path = Path and Event2.Target = Target then
                  Event2.Event := Event;
                  Added := True;
                  exit;
               end if;
            end loop;
            if not Added then
               EventsList.Append((Event, Target, Path));
            end if;
            if Event in Accessed | Moved_To
              and then Is_Directory
                (To_String(Path & Directory_Separator & Target)) then
               AddWatch(To_String(Path & Directory_Separator & Target));
            end if;
            if Event in Modified | Moved_From
              and then Is_Directory
                (To_String(Path & Directory_Separator & Target))
              and then not Exists
                (To_String(Path & Directory_Separator & Target)) then
               RemoveWatch(To_String(Path & Directory_Separator & Target));
            end if;
            exit when NameLength >= Length;
            Start := NameLength + 1;
         end loop;
         <<End_Of_Loop>>
      end loop;
   end InotifyRead;

end Inotify;
