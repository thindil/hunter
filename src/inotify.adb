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

with Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
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

   -- ****id* Inotify/Empty_Watch
   -- FUNCTION
   -- Empty inotify watch data
   -- SOURCE
   Empty_Watch: constant Watch_Data :=
     (Id => 0, Path => Null_Unbounded_String);
   -- ****

   -- ****it* Inotify/Inotify.Watches_Container
   -- FUNCTION
   -- Used to store information about inotify watches
   -- SOURCE
   package Watches_Container is new Vectors
     (Index_Type => Positive, Element_Type => Watch_Data);
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****iv* Inotify/Inotify.Watches
   -- FUNCTION
   -- Stores information about all active inotify watches
   -- SOURCE
   Watches: Watches_Container.Vector;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****if* Inotify/Inotify.Watches_List
   -- FUNCTION
   -- Manipulate the list of currently active inotify watches
   -- PARAMETERS
   -- New_Watch - A new inotify watch to add to the list. Can be empty.
   --             Default value is Empty_Watch
   -- Clear     - If true, clear the list
   -- RESULT
   -- The vector with list of current inotify active watches
   -- SOURCE
   function Watches_List
     (New_Watch: Watch_Data := Empty_Watch; Clear: Boolean := False)
      return Watches_Container.Vector is
      -- ****
   begin
      if New_Watch /= Empty_Watch then
         Watches.Append(New_Item => New_Watch);
      end if;
      if Clear then
         Watches.Clear;
      end if;
      return Watches;
   end Watches_List;

   -- ****ie* Inotify/Inotify_Error
   -- FUNCTION
   -- Exception raised by problems during manipulating watches list
   -- SOURCE
   Inotify_Error: exception;
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
      function Inotify_Init_C return int with
         Import => True,
         Convention => C,
         External_Name => "inotify_init";
   begin
      Instance := File_Descriptor(Inotify_Init_C);
   end Inotify_Init;

   procedure Inotify_Close is
   begin
      Close(FD => File_Descriptor(Get_Instance));
   end Inotify_Close;

   -- ****if* Inotify/Inotify.Add_Watch
   -- FUNCTION
   -- Add inotify watch to selected path
   -- PARAMETERS
   -- Path - Full path to which new inotify watch will be added
   -- SOURCE
   procedure Add_Watch(Path: String) is
      -- ****
      use Interfaces.C.Strings;

      type Unsigned_Integer is mod 2**Integer'Size;
      Initial_Value: constant Unsigned_Integer := 0;
      Mask: Unsigned_Integer := Initial_Value;
      Default_Masks: constant array(1 .. 5) of Inotify_Events :=
        (1 => METADATA, 2 => MOVED_FROM, 3 => MOVED_TO, 4 => DELETED,
         5 => CREATED);
      Watch: int := 0;
      Amount: constant Natural := Natural(Watches_List.Length);
      function Inotify_Add_Watch_C
        (Fd: int; Pathname: chars_ptr; Mask_C: int) return int with
         Import => True,
         Convention => C,
         External_Name => "inotify_add_watch";
   begin
      Create_Mask_Loop :
      for Event_Mask of Default_Masks loop
         Mask := Mask or Inotify_Events'Enum_Rep(Event_Mask);
      end loop Create_Mask_Loop;
      Watch :=
        Inotify_Add_Watch_C
          (Fd => Get_Instance, Pathname => New_String(Str => Path),
           Mask_C => int(Mask));
      if Watch > 0
        and then Amount =
          Natural
            (Watches_List
               (New_Watch =>
                  (Id => Watch, Path => To_Unbounded_String(Source => Path)))
               .Length) then
         raise Inotify_Error with "Can't add a new watch";
      end if;
   end Add_Watch;

   procedure Add_Watches(Path: String) is
      use GNAT.Directory_Operations;

      Directory: Dir_Type;
      Last: Natural := 0;
      File_Name: String(1 .. 1_024) := (others => ' ');
   begin
      Add_Watch(Path => Path);
      Open(Dir => Directory, Dir_Name => Path);
      Add_Watches_Loop :
      loop
         Read(Dir => Directory, Str => File_Name, Last => Last);
         exit Add_Watches_Loop when Last = 0;
         if File_Name(1 .. Last) in "." | ".." then
            goto End_Of_Loop;
         end if;
         if Is_Directory
             (Name => Path & Directory_Separator & File_Name(1 .. Last))
           and then Is_Read_Accessible_File
             (Name => Path & Directory_Separator & File_Name(1 .. Last)) then
            Add_Watch(Path => Path & "/" & File_Name(1 .. Last));
         end if;
         <<End_Of_Loop>>
      end loop Add_Watches_Loop;
      Close(Dir => Directory);
   end Add_Watches;

   procedure Remove_Watches is
   begin
      Remove_Watches_Loop :
      for Watch of Watches_List loop
         if Inotify_Rm_Watch_C(Fd => Get_Instance, Wd => Watch.Id) = -1 then
            null;
         end if;
      end loop Remove_Watches_Loop;
      if Watches_List(Clear => True).Length > 0 then
         raise Inotify_Error with "Can't clear watches list";
      end if;
   end Remove_Watches;

   procedure Inotify_Read is
      use Ada.Directories;

      Buffer: array(1 .. 4_096) of Character := (others => ' ');
      Length, Name_Length, Start: Integer := 0;
      Path, Target: Unbounded_String := Null_Unbounded_String;
      Event: Inotify_Events := Accessed_Event;
      Added: Boolean := False;
      procedure Remove_Watch(Watch_Path: String) is
      begin
         Remove_Watches_Loop :
         for Watch of Watches_List loop
            if To_String(Source => Watch.Path) = Watch_Path then
               if Inotify_Rm_Watch_C(Fd => Get_Instance, Wd => Watch.Id) =
                 -1 then
                  null;
               end if;
               exit Remove_Watches_Loop;
            end if;
         end loop Remove_Watches_Loop;
      end Remove_Watch;
   begin
      Read_Events_Loop :
      loop
         Length :=
           Read
             (FD => File_Descriptor(Get_Instance), A => Buffer'Address,
              N => 4_096);
         exit Read_Events_Loop when Length = -1;
         if Temporary_Stop then
            goto End_Of_Loop;
         end if;
         Start := 1;
         Read_Event_Loop :
         loop
            Read_Watches_Loop :
            for Watch of Watches_List loop
               if int(Character'Pos(Buffer(Start))) = Watch.Id then
                  Path := Watch.Path;
                  Name_Length :=
                    Character'Pos(Buffer(Start + 12)) + 15 + Start;
                  Target := Null_Unbounded_String;
                  --## rule off SIMPLIFIABLE_STATEMENTS
                  Read_Buffer_Loop :
                  for I in Start + 16 .. Name_Length loop
                     exit Read_Watches_Loop when Character'Pos(Buffer(I)) = 0;
                     Append(Source => Target, New_Item => Buffer(I));
                  end loop Read_Buffer_Loop;
                  --## rule on SIMPLIFIABLE_STATEMENTS
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
               Events_List.Append
                 (New_Item =>
                    (Event => Event, Target => Target, Path => Path));
            end if;
            if Event in ACCESSED | MOVED_TO
              and then Is_Directory
                (Name =>
                   To_String
                     (Source => Path & Directory_Separator & Target)) then
               Add_Watch
                 (Path =>
                    To_String(Source => Path & Directory_Separator & Target));
            end if;
            if Event in MODIFIED | MOVED_FROM
              and then Is_Directory
                (Name =>
                   To_String(Source => Path & Directory_Separator & Target))
              and then not Exists
                (Name =>
                   To_String
                     (Source => Path & Directory_Separator & Target)) then
               Remove_Watch
                 (Watch_Path =>
                    To_String(Source => Path & Directory_Separator & Target));
            end if;
            exit Read_Event_Loop when Name_Length >= Length;
            Start := Name_Length + 1;
         end loop Read_Event_Loop;
         <<End_Of_Loop>>
      end loop Read_Events_Loop;
   end Inotify_Read;

end Inotify;
