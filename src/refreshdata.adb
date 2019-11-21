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

   InotifyInstance: Instance;
   package Items_Container is new Indefinite_Hashed_Maps(String, Event_Kind,
      Ada.Strings.Hash, "=");
   ItemsList: Items_Container.Map;

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

   function CheckItems return Boolean is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "fileslist"));
      FileIter: Gtk_Tree_Iter := Get_Iter_First(FilesList);
      Size: File_Size;
      SubDirectory: Dir_Type;
      SubLast: Natural;
      SubFileName: String(1 .. 1024);
      MimeType: Unbounded_String;
   begin
      if not TemporaryStop then
         Foreach
           (Gtk_List_Store(Get_Object(Builder, "fileslist")),
            UpdateItem'Access);
         if ItemsList.Length = 0 then
            return True;
         end if;
         -- Fixme: not adding new files
         Set_Sort_Func
           (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
            EmptySortFiles'Access);
         for I in ItemsList.Iterate loop
            if ItemsList(I) /= Created and ItemsList(I) /= Moved_To then
               goto End_Of_Loop;
            end if;
            Append(FilesList, FileIter);
            begin
               Set
                 (FilesList, FileIter, 5,
                  Ada.Calendar.Formatting.Image
                    (Date => Modification_Time(Items_Container.Key(I)),
                     Time_Zone => UTC_Time_Offset));
            exception
               when others =>
                  Set(FilesList, FileIter, 5, "unknown");
            end;
            Set(FilesList, FileIter, 6, Items_Container.Key(I));
            if Is_Directory(Items_Container.Key(I)) then
               if Simple_Name(Items_Container.Key(I))(1) = '.' then
                  Set(FilesList, FileIter, 1, 1);
               else
                  Set(FilesList, FileIter, 1, 2);
               end if;
               if Is_Symbolic_Link(Items_Container.Key(I)) then
                  Set(FilesList, FileIter, 2, "emblem-symbolic-link");
               else
                  Set(FilesList, FileIter, 2, "folder");
               end if;
               Set(FilesList, FileIter, 4, Gint'Last);
               if Is_Read_Accessible_File(Items_Container.Key(I)) then
                  Open(SubDirectory, Items_Container.Key(I));
                  Size := 0;
                  loop
                     Read(SubDirectory, SubFileName, SubLast);
                     exit when SubLast = 0;
                     if SubFileName(1 .. SubLast) /= "." and
                       SubFileName(1 .. SubLast) /= ".." then
                        Size := Size + 1;
                     end if;
                  end loop;
                  Close(SubDirectory);
                  Set(FilesList, FileIter, 3, File_Size'Image(Size));
               else
                  Set(FilesList, FileIter, 3, "?");
               end if;
            else
               if Simple_Name(Items_Container.Key(I))(1) = '.' then
                  Set(FilesList, FileIter, 1, 3);
               else
                  Set(FilesList, FileIter, 1, 4);
               end if;
               if Is_Symbolic_Link(Items_Container.Key(I)) then
                  Set(FilesList, FileIter, 2, "emblem-symbolic-link");
               elsif Is_Executable_File(Items_Container.Key(I)) then
                  Set(FilesList, FileIter, 2, "application-x-executable");
               else
                  MimeType :=
                    To_Unbounded_String(GetMimeType(Items_Container.Key(I)));
                  if Index(MimeType, "audio") > 0 then
                     Set(FilesList, FileIter, 2, "audio-x-generic");
                  elsif Index(MimeType, "font") > 0 then
                     Set(FilesList, FileIter, 2, "font-x-generic");
                  elsif Index(MimeType, "image") > 0 then
                     Set(FilesList, FileIter, 2, "image-x-generic");
                  elsif Index(MimeType, "video") > 0 then
                     Set(FilesList, FileIter, 2, "video-x-generic");
                  elsif Index(MimeType, "text/x-script") > 0 then
                     Set(FilesList, FileIter, 2, "text-x-script");
                  elsif MimeType = To_Unbounded_String("text/html") then
                     Set(FilesList, FileIter, 2, "text-html");
                  elsif Index(MimeType, "zip") > 0 or
                    Index(MimeType, "x-xz") > 0 then
                     Set(FilesList, FileIter, 2, "package-x-generic");
                  elsif Index(MimeType, "text") > 0 then
                     Set(FilesList, FileIter, 2, "text-x-generic");
                  else
                     Set(FilesList, FileIter, 2, "text-x-generic-template");
                  end if;
               end if;
               if not Is_Read_Accessible_File(Items_Container.Key(I)) then
                  Set(FilesList, FileIter, 3, "?");
                  Set(FilesList, FileIter, 4, 0);
                  goto End_Of_Loop;
               end if;
               if Is_Symbolic_Link(Items_Container.Key(I)) then
                  Set(FilesList, FileIter, 3, "->");
                  Set(FilesList, FileIter, 4, 0);
               elsif Is_Regular_File(Items_Container.Key(I)) then
                  Size := Ada.Directories.Size(Items_Container.Key(I));
                  Set(FilesList, FileIter, 3, CountFileSize(Size));
                  if Size > File_Size(Gint'Last) then
                     Size := File_Size(Gint'Last);
                  end if;
                  Set(FilesList, FileIter, 4, Gint(Size));
               else
                  Set(FilesList, FileIter, 3, "0");
                  Set(FilesList, FileIter, 4, 0);
               end if;
            end if;
            <<End_Of_Loop>>
         end loop;
         Set_Sort_Func
           (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
            SortFiles'Access);
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")));
      end if;
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
