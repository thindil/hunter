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
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with Glib.Main; use Glib.Main;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with Utils; use Utils;

package body RefreshData is

   -- ****iv* RefreshData/LastCheck
   -- FUNCTION
   -- Time when the program last check for modification time of files and
   -- directories
   -- SOURCE
   LastCheck: Time;
   -- ****

   -- ****iv* Messages/Source_Id
   -- FUNCTION
   -- ID of timer to hide messages
   -- SOURCE
   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   -- ****if* RefreshData/CheckItem
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
   function CheckItem
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      -- ****
      pragma Unreferenced(Path);
      FileName: constant String :=
        To_String(CurrentDirectory) & "/" & Get_String(Model, Iter, 0);
      ModificationTime: constant String := Get_String(Model, Iter, 5);
      Size: File_Size;
      Directory: Dir_Type;
      Last: Natural;
      SubFileName: String(1 .. 1024);
   begin
      if ModificationTime /= "unknown"
        and then Value(ModificationTime, UTC_Time_Offset) /=
          Modification_Time(FileName) then
         Set
           (-(Model), Iter, 5,
            Image
              (Date => Modification_Time(FileName),
               Time_Zone => UTC_Time_Offset));
         if not Is_Read_Accessible_File(FileName) then
            Set(-(Model), Iter, 3, "?");
            Set(-(Model), Iter, 4, 0);
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
      end if;
      return False;
   end CheckItem;

   -- ****if* RefreshData/CheckItems
   -- FUNCTION
   -- Timer function - check periodically for changes in current directory
   -- RESULT
   -- Always true to keep timer alive
   -- SOURCE
   function CheckItems return Boolean is
      -- ****
   begin
      if Settings.AutoRefresh then
         if Modification_Time(To_String(CurrentDirectory)) > LastCheck then
            Reload(Builder);
         else
            Foreach
              (Gtk_List_Store(Get_Object(Builder, "fileslist")),
               CheckItem'Access);
         end if;
      end if;
      LastCheck := Clock;
      return True;
   end CheckItems;

   procedure StartTimer is
   begin
      if Source_Id /= No_Source_Id then
         Remove(Source_Id);
      end if;
      LastCheck := Clock;
      Source_Id :=
        Timeout_Add
          (Guint(Settings.AutoRefreshInterval) * 1000, CheckItems'Access);
   end StartTimer;

end RefreshData;
