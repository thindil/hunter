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

--with Ada.Calendar; use Ada.Calendar;
--with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
--with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
--with Ada.Containers; use Ada.Containers;
--with Ada.Directories; use Ada.Directories;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with GNAT.Directory_Operations; use GNAT.Directory_Operations;
--with GNAT.OS_Lib; use GNAT.OS_Lib;
--with Gtk.List_Store; use Gtk.List_Store;
--with Gtk.Tree_Model; use Gtk.Tree_Model;
--with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
--with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
--with Gtk.Tree_View;
--with Glib; use Glib;
--with Glib.Main; use Glib.Main;
--with LoadData; use LoadData;
--with MainWindow; use MainWindow;
with Inotify; use Inotify;
--with Preferences; use Preferences;
--with ShowItems; use ShowItems;
--with Utils; use Utils;

package body RefreshData is

   -- ****iv* RefreshData/Source_Id
   -- FUNCTION
   -- ID of timer to hide messages
   -- SOURCE
--   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   -- ****if* RefreshData/UpdateItem
   -- FUNCTION
   -- Check if selected file or directory was modified and upgrade information
   -- if was
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content of the current directory
   -- Path  - Gtk_Tree_Path to selected file or directory (Unused)
   -- Iter  - Gtk_Tree_Iter to selected file or directory
   -- RESULT
   -- Return false to check each file or directory in current directory. If
   -- file or directory was deleted, restart checking and return true.
   -- SOURCE
--   function UpdateItem
--     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
--      return Boolean is
--      -- ****
--      pragma Unreferenced(Path);
--      FileName: constant String :=
--        To_String(CurrentDirectory) & "/" & Get_String(Model, Iter, 0);
--      NewIter: Gtk_Tree_Iter := Iter;
--      Size: File_Size;
--      Directory: Dir_Type;
--      Last: Natural;
--      SubFileName: String(1 .. 1024);
--      Index: Natural := 0;
--      procedure RemoveItem is
--      begin
--         Remove(-(Model), NewIter);
--         EventsList.Delete(Index);
--         if NewIter = Null_Iter then
--            if N_Children(Model) = 0 then
--               CurrentSelected := CurrentDirectory;
--            end if;
--            PreviewItem(null);
--         end if;
--         Foreach(Model, UpdateItem'Access);
--      end RemoveItem;
--   begin
--      for I in EventsList.Iterate loop
--         if EventsList(I).Path = To_Unbounded_String(FileName) or
--           (EventsList(I).Path & "/" & EventsList(I).Target) =
--             To_Unbounded_String(FileName) then
--            Index := Events_Container.To_Index(I);
--            exit;
--         end if;
--      end loop;
--      if Index = 0 then
--         return False;
--      end if;
--      case EventsList(Index).Event is
--         when Moved_From | Deleted =>
--            RemoveItem;
--            return True;
--         when Metadata | Modified | Moved_To | Accessed =>
--            if not Exists(FileName) then
--               RemoveItem;
--               return True;
--            end if;
--            Set
--              (-(Model), Iter, 5,
--               Image
--                 (Date => Modification_Time(FileName),
--                  Time_Zone => UTC_Time_Offset));
--            if not Is_Read_Accessible_File(FileName) then
--               Set(-(Model), Iter, 3, "?");
--               Set(-(Model), Iter, 4, 0);
--               EventsList.Delete(Index);
--               return False;
--            end if;
--            if Is_Directory(FileName) then
--               Open(Directory, FileName);
--               Size := 0;
--               loop
--                  Read(Directory, SubFileName, Last);
--                  exit when Last = 0;
--                  if SubFileName(1 .. Last) /= "." and
--                    SubFileName(1 .. Last) /= ".." then
--                     Size := Size + 1;
--                  end if;
--               end loop;
--               Close(Directory);
--               Set(-(Model), Iter, 3, File_Size'Image(Size));
--            elsif Is_Regular_File(FileName) then
--               Size := Ada.Directories.Size(FileName);
--               Set(-(Model), Iter, 3, CountFileSize(Size));
--               if Size > File_Size(Gint'Last) then
--                  Size := File_Size(Gint'Last);
--               end if;
--               Set(-(Model), Iter, 4, Gint(Size));
--            end if;
--            if FileName = To_String(CurrentSelected) then
--               PreviewItem(null);
--            end if;
--         when others =>
--            null;
--      end case;
--      EventsList.Delete(Index);
--      return False;
--   end UpdateItem;
--
--   -- ****if* RefreshData/CheckItems
--   -- FUNCTION
--   -- Check if any events are pending and update directory listing with new
--   -- information.
--   -- RESULT
--   -- This function always return True to not stop timer from triggering.
--   -- SOURCE
--   function CheckItems return Boolean is
--      FilesList: constant Gtk_List_Store :=
--        -(Gtk.Tree_Model_Filter.Get_Model
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model(DirectoryView))))));
--      -- ****
--      FileIter: Gtk_Tree_Iter := Get_Iter_First(FilesList);
--      procedure RefilterList is
--      begin
--         Gtk.Tree_Model_Sort.Set_Sort_Func
--           (-(Gtk.Tree_View.Get_Model(DirectoryView)), 0, SortFiles'Access);
--         Refilter
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model(DirectoryView)))));
--      end RefilterList;
--   begin
--      if TemporaryStop or Settings.AutoRefreshInterval = 0 or
--        EventsList.Length = 0 then
--         EventsList.Clear;
--         return True;
--      end if;
--      Gtk.Tree_Model_Sort.Set_Sort_Func
--        (-(Gtk.Tree_View.Get_Model(DirectoryView)), 0, EmptySortFiles'Access);
--      Foreach(FilesList, UpdateItem'Access);
--      if EventsList.Length = 0 then
--         RefilterList;
--         return True;
--      end if;
--      for Event of EventsList loop
--         if Event.Path = CurrentDirectory
--           and then
--           ((Event.Event in Moved_To | Metadata | Accessed) and
--            Exists(To_String(Event.Path & "/" & Event.Target))) then
--            AddItem
--              (FilesList, FileIter,
--               To_String(Event.Path & "/" & Event.Target));
--         end if;
--      end loop;
--      RefilterList;
--      EventsList.Clear;
--      return True;
--   end CheckItems;

   task body InotifyTask is
   begin
      accept Start;
      InotifyRead;
   end InotifyTask;

   procedure StartTimer(Path: String := "") is
   begin
--      if Source_Id /= No_Source_Id then
--         Remove(Source_Id);
--      end if;
      if Path /= "" then
         AddWatches(Path);
         InotifyTask.Start;
      end if;
 --     if Settings.AutoRefreshInterval > 0 then
 --        Source_Id :=
 --          Timeout_Add
 --            (Guint(Settings.AutoRefreshInterval) * 1000, CheckItems'Access);
 --     end if;
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
