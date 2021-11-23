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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Tcl; use Tcl;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Common; use Common;
with Inotify; use Inotify;
pragma Elaborate_All(Inotify);
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body RefreshData is

   task body Inotify_Task is
   begin
      accept Start;
      Inotify_Read;
   end Inotify_Task;

   -- ****iv* RefreshData/RefreshData.Timer_Token
   -- FUNCTION
   -- Identifier for the timer for periodically update directory listing
   -- SOURCE
   Timer_Token: Tcl_TimerToken;
   -- ****

   -- ****iv* RefreshData/Is_Checking
   -- FUNCTION
   -- If True, checking events is on
   -- SOURCE
   Is_Checking: Boolean := False;
   -- ****

   -- ****if* RefreshData/RefreshData.Check_Items
   -- FUNCTION
   -- Check all inotify events and update directory listing if needed
   -- PARAMETERS
   -- Data - Custom data send to the command. Unused
   -- SOURCE
   procedure Check_Items(Data: ClientData) with
      Convention => C;
      -- ****

   procedure Check_Items(Data: ClientData) is
      pragma Unreferenced(Data);
      Refresh_List, Item_Exists: Boolean := False;
      Item_Index: Items_Container.Extended_Index := 0;
      File_Name: Unbounded_String := Null_Unbounded_String;
      Directory: Dir_Type;
      Sub_File_Name: String(1 .. 1_024) := (others => ' ');
      Last: Natural range 0 .. Sub_File_Name'Last := 0;
      procedure Remove_Item is
      begin
         Items_List.Delete(Index => Item_Index);
         if Items_List.Length = 0 then
            Current_Selected := Common.Current_Directory;
         end if;
         Refresh_List := True;
      end Remove_Item;
   begin
      if Is_Checking then
         return;
      end if;
      Is_Checking := True;
      Check_Events_Loop :
      for Event of Events_List loop
         if Temporary_Stop then
            goto Clear_List;
         end if;
         if Event.Path = Common.Current_Directory
           and then
           (Event.Event in MOVED_TO | METADATA | ACCESSED and
            Exists(Name => To_String(Source => Event.Path & "/" & Event.Target))) then
            Item_Exists := False;
            Check_If_Item_Exists_Loop :
            for Item of Items_List loop
               if Item.Name = Event.Target then
                  Item_Exists := True;
                  exit Check_If_Item_Exists_Loop;
               end if;
            end loop Check_If_Item_Exists_Loop;
            if not Item_Exists then
               Add_Item
                 (Path => To_String(Source => Event.Path & "/" & Event.Target), List => Items_List);
               Refresh_List := True;
               goto End_Of_Loop;
            end if;
         end if;
         Item_Index := Items_List.First_Index;
         --## rule off SIMPLIFIABLE_STATEMENTS
         Update_Items_Loop :
         while Item_Index <= Items_List.Last_Index loop
            File_Name := Items_List(Item_Index).Path;
            if File_Name = Event.Path or
              Items_List(Item_Index).Name = Event.Target then
               case Event.Event is
                  when MOVED_FROM | DELETED =>
                     Remove_Item;
                     exit Update_Items_Loop;
                  when METADATA | MODIFIED | MOVED_TO | ACCESSED =>
                     if not Exists(To_String(File_Name)) then
                        Remove_Item;
                        exit Update_Items_Loop;
                     end if;
                     Refresh_List := True;
                     Items_List(Item_Index).Modified :=
                       Modification_Time(To_String(File_Name));
                     if not Is_Read_Accessible_File(To_String(File_Name)) then
                        Items_List(Item_Index).Size := -1;
                        exit Update_Items_Loop;
                     end if;
                     if Is_Directory(To_String(File_Name)) then
                        Open(Directory, To_String(File_Name));
                        Items_List(Item_Index).Size := 0;
                        Count_New_Size_Loop :
                        loop
                           Read(Directory, Sub_File_Name, Last);
                           exit Count_New_Size_Loop when Last = 0;
                           if Sub_File_Name(1 .. Last) /= "." and
                             Sub_File_Name(1 .. Last) /= ".." then
                              Items_List(Item_Index).Size :=
                                Items_List(Item_Index).Size + 1;
                           end if;
                        end loop Count_New_Size_Loop;
                        Close(Directory);
                     elsif Is_Regular_File(To_String(File_Name)) then
                        Items_List(Item_Index).Size :=
                          Item_Size(Ada.Directories.Size(To_String(File_Name)));
                     end if;
                     if File_Name = To_String(Current_Selected) then
                        ShowPreview;
                     end if;
                     exit Update_Items_Loop;
                  when others =>
                     null;
               end case;
            end if;
            Item_Index := Item_Index + 1;
         end loop Update_Items_Loop;
         --## rule on SIMPLIFIABLE_STATEMENTS
         <<End_Of_Loop>>
      end loop Check_Events_Loop;
      if Refresh_List then
         Items_Sorting.Sort(Items_List);
         Update_Directory_List(True);
         Refresh_List := False;
      end if;
      <<Clear_List>>
      begin
         Events_List.Clear;
      exception
         when others =>
            null;
      end;
      Timer_Token :=
        Tcl_CreateTimerHandler
          (int(Settings.Auto_Refresh_Interval) * 1_000, Check_Items'Access,
           Null_ClientData);
      Is_Checking := False;
   end Check_Items;

   procedure Start_Timer(Path: String := "") is
   begin
      if Timer_Token /= null then
         Tcl_DeleteTimerHandler(Timer_Token);
      end if;
      if Path /= "" then
         Add_Watches(Path);
         Inotify_Task.Start;
      end if;
      if Settings.Auto_Refresh_Interval > 0 then
         Timer_Token :=
           Tcl_CreateTimerHandler
             (int(Settings.Auto_Refresh_Interval) * 1_000, Check_Items'Access,
              Null_ClientData);
      end if;
   end Start_Timer;

   procedure Update_Watch(Path: String) is
   begin
      Temporary_Stop := True;
      Check_Items(Null_ClientData);
      Remove_Watches;
      Add_Watches(Path);
      Temporary_Stop := False;
   end Update_Watch;

end RefreshData;
