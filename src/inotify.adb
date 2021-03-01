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

with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Inotify is

   -- ****iv* Inotify/Inotify.Instance
   -- FUNCTION
   -- inotify instance to read
   -- SOURCE
   Instance: File_Descriptor;
   -- ****

   -- ****is* Inotify/Inotify.Watch_Data
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

   -- ****it* Inotify/Inotify.Watches_Container
   -- FUNCTION
   -- Used to store information about inotify watches
   -- SOURCE
   package Watches_Container is new Vectors(Index_Type => Positive,
      Element_Type => Watch_Data);
   -- ****

   -- ****iv* Inotify/Inotify.Watches
   -- FUNCTION
   -- Stores information about all active inotify watches
   -- SOURCE
   Watches: Watches_Container.Vector;
   -- ****

   -- ****it* Inotify/Inotify.Mask_Array
   -- FUNCTION
   -- Array of inotify events to catch
   -- SOURCE
   type Mask_Array is array(Positive range <>) of Inotify_Events;
   -- ****

   -- ****if* Inotify/Inotify.Get_Instance
   -- FUNCTION
   -- Get the current instance of inotify converted to C int type
   -- RESULT
   -- The current inotify instance converted to C int type
   -- SOURCE
   function Get_Instance return int is
      -- ****
   begin
      return int(Instance);
   end Get_Instance;

   -- ****if* Inotify/Inotify.Inotify_Init_C
   -- FUNCTION
   -- Binding to the C function
   -- RESULT
   -- File descriptor used for a newly created inotify instance
   -- SOURCE
   function Inotify_Init_C return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_init";
      -- ****

      -- ****if* Inotify/Inotify.Inotify_Add_Watch_C
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- Fd       - inotify instance to which add a new watch
      -- Pathname - full path to which a new inotify watch will be added
      -- Mask     - list of inotify events to listen
      -- RESULT
      -- -1 if inotify watch cannot be added, 0 if exists, otherwise index of
      -- newly created watch
      -- SOURCE
   function Inotify_Add_Watch_C
     (Fd: int; Pathname: chars_ptr; Mask: int) return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_add_watch";
      -- ****

      -- ****if* Inotify/Inotify.Inotify_Rm_Watch_C
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- Fd - inotify instance from which selected watch will be removed
      -- Wd - inotify watch index to remove
      -- RESULT
      -- 0 if inotify watch was succesfully removed, -1 when error occurs
      -- SOURCE
   function Inotify_Rm_Watch_C(Fd, Wd: int) return int with
      Import => True,
      Convention => C,
      External_Name => "inotify_rm_watch";
      -- ****

   procedure Inotify_Init is
   begin
      Instance := File_Descriptor(Inotify_Init_C);
   end Inotify_Init;

   procedure Inotify_Close is
   begin
      Close(FD => Instance);
   end Inotify_Close;

   -- ****if* Inotify/Inotify.Create_Mask
   -- FUNCTION
   -- Convert list of inotify events to mask value for C
   -- PARAMETERS
   -- Events - list of inotify events to convert
   -- RESULT
   -- Mask value for C
   -- SOURCE
   function Create_Mask(Events: Mask_Array) return int is
      -- ****
      type Unsigned_Integer is mod 2**Integer'Size;
      Initial_Value: constant Unsigned_Integer := 0;
      Mask: Unsigned_Integer := Initial_Value;
   begin
      Create_Mask_Loop :
      for Event_Mask of Events loop
         Mask := Mask or Inotify_Events'Enum_Rep(Event_Mask);
      end loop Create_Mask_Loop;
      return int(Mask);
   end Create_Mask;

   -- ****if* Inotify/Inotify.Add_Watch
   -- FUNCTION
   -- Add inotify watch to selected path
   -- PARAMETERS
   -- Path - Full path to which new inotify watch will be added
   -- SOURCE
   procedure Add_Watch(Path: String) is
      -- ****
      Watch: int;
      Mask: constant int :=
        Create_Mask
          (Events =>
             (1 => METADATA, 2 => MOVED_FROM, 3 => MOVED_TO, 4 => DELETED,
              5 => CREATED));
   begin
      Watch :=
        Inotify_Add_Watch_C
          (Fd => Get_Instance, Pathname => New_String(Str => Path),
           Mask => Mask);
      if Watch > 0 then
         Watches.Append
           (New_Item =>
              (Id => Watch, Path => To_Unbounded_String(Source => Path)));
      end if;
   end Add_Watch;

   procedure Add_Watches(Path: String) is
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
   begin
      Add_Watch(Path => Path);
      Open(Dir => Directory, Dir_Name => Path);
      Add_Watches_Loop :
      loop
         Read(Dir => Directory, Str => FileName, Last => Last);
         exit Add_Watches_Loop when Last = 0;
         if FileName(1 .. Last) in "." | ".." then
            goto End_Of_Loop;
         end if;
         if Is_Directory
             (Name => Path & Directory_Separator & FileName(1 .. Last))
           and then Is_Read_Accessible_File
             (Name => Path & Directory_Separator & FileName(1 .. Last)) then
            Add_Watch(Path => Path & "/" & FileName(1 .. Last));
         end if;
         <<End_Of_Loop>>
      end loop Add_Watches_Loop;
      Close(Dir => Directory);
   end Add_Watches;

   procedure Remove_Watches is
   begin
      Remove_Watches_Loop :
      for Watch of Watches loop
         if Inotify_Rm_Watch_C(Fd => Get_Instance, Wd => Watch.Id) = -1 then
            null;
         end if;
      end loop Remove_Watches_Loop;
      Watches.Clear;
   end Remove_Watches;

   -- ****if* Inotify/Inotify.RemoveWatch
   -- FUNCTION
   -- Remove selected watch
   -- PARAMETERS
   -- Path - Full path from which inotify watch will be removed
   -- SOURCE
   procedure RemoveWatch(Path: String) is
   -- ****
   begin
      Remove_Watches_Loop :
      for Watch of Watches loop
         if To_String(Source => Watch.Path) = Path then
            if Inotify_Rm_Watch_C(Fd => Get_Instance, Wd => Watch.Id) = -1 then
               null;
            end if;
            exit Remove_Watches_Loop;
         end if;
      end loop Remove_Watches_Loop;
   end RemoveWatch;

   procedure Inotify_Read is
      Buffer: array(1 .. 4096) of Character;
      Length, NameLength, Start: Integer;
      Path, Target: Unbounded_String;
      Event: Inotify_Events;
      Added: Boolean;
   begin
      Read_Events_Loop :
      loop
         Length := Read(File_Descriptor(Get_Instance), Buffer'Address, 4096);
         exit Read_Events_Loop when Length = -1;
         if Temporary_Stop then
            goto End_Of_Loop;
         end if;
         Start := 1;
         Read_Event_Loop :
         loop
            Read_Watches_Loop :
            for Watch of Watches loop
               if int(Character'Pos(Buffer(Start))) = Watch.Id then
                  Path := Watch.Path;
                  NameLength := Character'Pos(Buffer(Start + 12)) + 15 + Start;
                  Target := Null_Unbounded_String;
                  for I in Start + 16 .. NameLength loop
                     exit Read_Watches_Loop when Character'Pos(Buffer(I)) = 0;
                     Append(Target, Buffer(I));
                  end loop;
                  exit Read_Watches_Loop;
               end if;
            end loop Read_Watches_Loop;
            Event :=
              (if Character'Pos(Buffer(Start + 4)) > 0 then
                 Inotify_Events'Enum_Val(Character'Pos(Buffer(Start + 4)))
               else Inotify_Events'Enum_Val(Character'Pos(Buffer(Start + 5))));
            Added := False;
            Check_Added_Loop :
            for Event2 of Events_List loop
               if Event2.Path = Path and Event2.Target = Target then
                  Event2.Event := Event;
                  Added := True;
                  exit Check_Added_Loop;
               end if;
            end loop Check_Added_Loop;
            if not Added then
               Events_List.Append((Event, Target, Path));
            end if;
            if Event in ACCESSED | MOVED_TO
              and then Is_Directory
                (To_String(Path & Directory_Separator & Target)) then
               Add_Watch(To_String(Path & Directory_Separator & Target));
            end if;
            if Event in MODIFIED | MOVED_FROM
              and then Is_Directory
                (To_String(Path & Directory_Separator & Target))
              and then not Exists
                (To_String(Path & Directory_Separator & Target)) then
               RemoveWatch(To_String(Path & Directory_Separator & Target));
            end if;
            exit Read_Event_Loop when NameLength >= Length;
            Start := NameLength + 1;
         end loop Read_Event_Loop;
         <<End_Of_Loop>>
      end loop Read_Events_Loop;
   end Inotify_Read;

end Inotify;
