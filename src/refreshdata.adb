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
with Inotify; use Inotify;
pragma Elaborate_All(Inotify);
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body RefreshData is

   task body InotifyTask is
   begin
      accept Start;
      Inotify_Read;
   end InotifyTask;

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

   -- ****if* RefreshData/RefreshData.CheckItems
   -- FUNCTION
   -- Check all inotify events and update directory listing if needed
   -- PARAMETERS
   -- data - Custom data send to the command. Unused
   -- SOURCE
   procedure CheckItems(data: ClientData) with
      Convention => C;
      -- ****

   procedure CheckItems(data: ClientData) is
      pragma Unreferenced(data);
      RefreshList, ItemExists: Boolean := False;
      ItemIndex: Items_Container.Extended_Index;
      FileName: Unbounded_String;
      Directory: Dir_Type;
      SubFileName: String(1 .. 1_024);
      Last: Natural range 0 .. SubFileName'Last;
      procedure RemoveItem is
      begin
         ItemsList.Delete(ItemIndex);
         if ItemsList.Length = 0 then
            Current_Selected := MainWindow.Current_Directory;
         end if;
         RefreshList := True;
      end RemoveItem;
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
         if Event.Path = MainWindow.Current_Directory
           and then
           ((Event.Event in MOVED_TO | METADATA | ACCESSED) and
            Exists(To_String(Event.Path & "/" & Event.Target))) then
            ItemExists := False;
            Check_If_Item_Exists_Loop :
            for Item of ItemsList loop
               if Item.Name = Event.Target then
                  ItemExists := True;
                  exit Check_If_Item_Exists_Loop;
               end if;
            end loop Check_If_Item_Exists_Loop;
            if not ItemExists then
               AddItem(To_String(Event.Path & "/" & Event.Target), ItemsList);
               RefreshList := True;
               goto End_Of_Loop;
            end if;
         end if;
         ItemIndex := ItemsList.First_Index;
         Update_Items_Loop :
         while ItemIndex <= ItemsList.Last_Index loop
            FileName := ItemsList(ItemIndex).Path;
            if FileName = Event.Path or
              ItemsList(ItemIndex).Name = Event.Target then
               case Event.Event is
                  when MOVED_FROM | DELETED =>
                     RemoveItem;
                     exit Update_Items_Loop;
                  when METADATA | MODIFIED | MOVED_TO | ACCESSED =>
                     if not Exists(To_String(FileName)) then
                        RemoveItem;
                        exit Update_Items_Loop;
                     end if;
                     RefreshList := True;
                     ItemsList(ItemIndex).Modified :=
                       Modification_Time(To_String(FileName));
                     if not Is_Read_Accessible_File(To_String(FileName)) then
                        ItemsList(ItemIndex).Size := -1;
                        exit Update_Items_Loop;
                     end if;
                     if Is_Directory(To_String(FileName)) then
                        Open(Directory, To_String(FileName));
                        ItemsList(ItemIndex).Size := 0;
                        Count_New_Size_Loop :
                        loop
                           Read(Directory, SubFileName, Last);
                           exit Count_New_Size_Loop when Last = 0;
                           if SubFileName(1 .. Last) /= "." and
                             SubFileName(1 .. Last) /= ".." then
                              ItemsList(ItemIndex).Size :=
                                ItemsList(ItemIndex).Size + 1;
                           end if;
                        end loop Count_New_Size_Loop;
                        Close(Directory);
                     elsif Is_Regular_File(To_String(FileName)) then
                        ItemsList(ItemIndex).Size :=
                          Item_Size(Ada.Directories.Size(To_String(FileName)));
                     end if;
                     if FileName = To_String(Current_Selected) then
                        ShowPreview;
                     end if;
                     exit Update_Items_Loop;
                  when others =>
                     null;
               end case;
            end if;
            ItemIndex := ItemIndex + 1;
         end loop Update_Items_Loop;
         <<End_Of_Loop>>
      end loop Check_Events_Loop;
      if RefreshList then
         Items_Sorting.Sort(ItemsList);
         Update_Directory_List(True);
         RefreshList := False;
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
          (int(Settings.Auto_Refresh_Interval) * 1_000, CheckItems'Access,
           Null_ClientData);
      Is_Checking := False;
   end CheckItems;

   procedure StartTimer(Path: String := "") is
   begin
      if Timer_Token /= null then
         Tcl_DeleteTimerHandler(Timer_Token);
      end if;
      if Path /= "" then
         Add_Watches(Path);
         InotifyTask.Start;
      end if;
      if Settings.Auto_Refresh_Interval > 0 then
         Timer_Token :=
           Tcl_CreateTimerHandler
             (int(Settings.Auto_Refresh_Interval) * 1_000, CheckItems'Access,
              Null_ClientData);
      end if;
   end StartTimer;

   procedure UpdateWatch(Path: String) is
   begin
      Temporary_Stop := True;
      CheckItems(Null_ClientData);
      Remove_Watches;
      Add_Watches(Path);
      Temporary_Stop := False;
   end UpdateWatch;

end RefreshData;
