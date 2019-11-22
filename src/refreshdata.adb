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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Inotify; use Inotify;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with Glib.Main; use Glib.Main;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with Utils; use Utils;

package body RefreshData is

   -- ****iv* RefreshData/Source_Id
   -- FUNCTION
   -- ID of timer to hide messages
   -- SOURCE
   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   -- ****iv* RefreshData/InotifyInstance
   -- FUNCTION
   -- inotify instance
   -- SOURCE
   InotifyInstance: Instance;
   -- ****

   -- ****it* RefreshData/Items_Container
   -- FUNCTION
   -- Used to store inotify events data.
   -- SOURCE
   package Items_Container is new Indefinite_Hashed_Maps(String, Event_Kind,
      Ada.Strings.Hash, "=");
   -- ****

   -- ****iv* RefreshData/ItemsList
   -- FUNCTION
   -- Stores all information about pending inotify events.
   -- SOURCE
   ItemsList: Items_Container.Map;
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
   -- Always return false to check each file or directory in current directory
   -- SOURCE
   function UpdateItem
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      -- ****
      pragma Unreferenced(Path);
      FileName: constant String :=
        To_String(CurrentDirectory) & "/" & Get_String(Model, Iter, 0);
      NewIter: Gtk_Tree_Iter := Iter;
      Size: File_Size;
      Directory: Dir_Type;
      Last: Natural;
      SubFileName: String(1 .. 1024);
   begin
      if ItemsList.Contains(FileName) then
         case ItemsList(FileName) is
            when Moved_From | Deleted =>
               Remove(-(Model), NewIter);
            when Metadata | Closed_Write =>
               Set
                 (-(Model), Iter, 5,
                  Image
                    (Date => Modification_Time(FileName),
                     Time_Zone => UTC_Time_Offset));
               if not Is_Read_Accessible_File(FileName) then
                  Set(-(Model), Iter, 3, "?");
                  Set(-(Model), Iter, 4, 0);
                  ItemsList.Delete(FileName);
                  return False;
               end if;
               if Is_Directory(FileName) then
                  Open(Directory, FileName);
                  Size := 0;
                  loop
                     Read(Directory, SubFileName, Last);
                     exit when Last = 0;
                     if SubFileName(1 .. Last) /= "." and
                       SubFileName(1 .. Last) /= ".." then
                        Size := Size + 1;
                     end if;
                  end loop;
                  Close(Directory);
                  Set(-(Model), Iter, 3, File_Size'Image(Size));
               elsif Is_Regular_File(FileName) then
                  Size := Ada.Directories.Size(FileName);
                  Set(-(Model), Iter, 3, CountFileSize(Size));
                  if Size > File_Size(Gint'Last) then
                     Size := File_Size(Gint'Last);
                  end if;
                  Set(-(Model), Iter, 4, Gint(Size));
               end if;
            when others =>
               null;
         end case;
         ItemsList.Delete(FileName);
      end if;
      return False;
   end UpdateItem;

   -- ****if* RefreshData/CheckItems
   -- FUNCTION
   -- Check if any events are pending and update directory listing with new
   -- information.
   -- RESULT
   -- This function always return True to not stop timer from triggering.
   -- SOURCE
   function CheckItems return Boolean is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "fileslist"));
      -- ****
      FileIter: Gtk_Tree_Iter := Get_Iter_First(FilesList);
   begin
      if TemporaryStop or not Settings.AutoRefresh or ItemsList.Length = 0 then
         ItemsList.Clear;
         return True;
      end if;
      Foreach
        (Gtk_List_Store(Get_Object(Builder, "fileslist")), UpdateItem'Access);
      if ItemsList.Length = 0 then
         return True;
      end if;
      Set_Sort_Func
        (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
         EmptySortFiles'Access);
      for I in ItemsList.Iterate loop
         if ItemsList(I) = Closed_Write or ItemsList(I) = Moved_To then
            AddItem(FilesList, FileIter, Items_Container.Key(I));
         end if;
      end loop;
      Set_Sort_Func
        (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
         SortFiles'Access);
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")));
      ItemsList.Clear;
      return True;
   end CheckItems;

   task body InotifyTask is
      procedure Handle_Event
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String) is
         pragma Unreferenced(Subject, Is_Directory);
      begin
         ItemsList.Include(Name, Event);
      end Handle_Event;
   begin
      accept Start;
      InotifyInstance.Process_Events(Handle_Event'Access);
   end InotifyTask;

   procedure StartTimer(Path: String := "") is
   begin
      if Source_Id /= No_Source_Id then
         Remove(Source_Id);
      end if;
      if Path /= "" then
         UpdateWatch(Path);
         InotifyTask.Start;
      end if;
      Source_Id :=
        Timeout_Add
          (Guint(Settings.AutoRefreshInterval) * 1000, CheckItems'Access);
   end StartTimer;

   procedure UpdateWatch(Path: String) is
   begin
      ItemsList.Clear;
      InotifyInstance.Add_Watch
        (Path,
         (Created | Metadata | Closed_Write | Moved_From | Moved_To |
          Deleted =>
            True,
          others => False));
   end UpdateWatch;

end RefreshData;
