-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
      InotifyRead;
   end InotifyTask;

   -- ****iv* RefreshData/RefreshData.Timer_Token
   -- FUNCTION
   -- Identifier for the timer for periodically update directory listing
   -- SOURCE
   Timer_Token: Tcl_TimerToken;
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
            CurrentSelected := CurrentDirectory;
         end if;
         RefreshList := True;
      end RemoveItem;
   begin
      if TemporaryStop then
         goto Clear_List;
      end if;
      for Event of EventsList loop
         if Event.Path = CurrentDirectory
           and then
           ((Event.Event in Moved_To | Metadata | Accessed) and
            Exists(To_String(Event.Path & "/" & Event.Target))) then
            ItemExists := False;
            for Item of ItemsList loop
               if Item.Name = Event.Target then
                  ItemExists := True;
                  exit;
               end if;
            end loop;
            if not ItemExists then
               AddItem(To_String(Event.Path & "/" & Event.Target), ItemsList);
               RefreshList := True;
               goto End_Of_Loop;
            end if;
         end if;
         ItemIndex := ItemsList.First_Index;
         while ItemIndex <= ItemsList.Last_Index loop
            FileName := ItemsList(ItemIndex).Path;
            if FileName = Event.Path or
              ItemsList(ItemIndex).Name = Event.Target then
               case Event.Event is
                  when Moved_From | Deleted =>
                     RemoveItem;
                     exit;
                  when Metadata | Modified | Moved_To | Accessed =>
                     if not Exists(To_String(FileName)) then
                        RemoveItem;
                        exit;
                     end if;
                     RefreshList := True;
                     ItemsList(ItemIndex).Modified :=
                       Modification_Time(To_String(FileName));
                     if not Is_Read_Accessible_File(To_String(FileName)) then
                        ItemsList(ItemIndex).Size := -1;
                        exit;
                     end if;
                     if Is_Directory(To_String(FileName)) then
                        Open(Directory, To_String(FileName));
                        ItemsList(ItemIndex).Size := 0;
                        loop
                           Read(Directory, SubFileName, Last);
                           exit when Last = 0;
                           if SubFileName(1 .. Last) /= "." and
                             SubFileName(1 .. Last) /= ".." then
                              ItemsList(ItemIndex).Size :=
                                ItemsList(ItemIndex).Size + 1;
                           end if;
                        end loop;
                        Close(Directory);
                     elsif Is_Regular_File(To_String(FileName)) then
                        ItemsList(ItemIndex).Size :=
                          Item_Size(Ada.Directories.Size(To_String(FileName)));
                     end if;
                     if FileName = To_String(CurrentSelected) then
                        ShowPreview;
                     end if;
                     exit;
                  when others =>
                     null;
               end case;
            end if;
            ItemIndex := ItemIndex + 1;
         end loop;
         <<End_Of_Loop>>
      end loop;
      if RefreshList then
         Items_Sorting.Sort(ItemsList);
         UpdateDirectoryList(True);
         RefreshList := False;
      end if;
      <<Clear_List>>
      EventsList.Clear;
      Timer_Token :=
        Tcl_CreateTimerHandler
          (int(Settings.AutoRefreshInterval) * 1_000, CheckItems'Access,
           Null_ClientData);
   end CheckItems;

   procedure StartTimer(Path: String := "") is
   begin
      if Timer_Token /= null then
         Tcl_DeleteTimerHandler(Timer_Token);
      end if;
      if Path /= "" then
         AddWatches(Path);
         InotifyTask.Start;
      end if;
      if Settings.AutoRefreshInterval > 0 then
         Timer_Token :=
           Tcl_CreateTimerHandler
             (int(Settings.AutoRefreshInterval) * 1_000, CheckItems'Access,
              Null_ClientData);
      end if;
   end StartTimer;

   procedure UpdateWatch(Path: String) is
   begin
      TemporaryStop := True;
      EventsList.Clear;
      RemoveWatches;
      AddWatches(Path);
      TemporaryStop := False;
   end UpdateWatch;

end RefreshData;
