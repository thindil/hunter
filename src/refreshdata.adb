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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtkada.Builder; use Gtkada.Builder;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Utils; use Utils;

package body RefreshData is

   function CheckItem
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      FileName: constant String :=
        To_String(CurrentDirectory) & "/" & Get_String(Model, Iter, 0);
      ModificationTime: constant String := Get_String(Model, Iter, 5);
   begin
      if ModificationTime /= "unknown"
        and then Value(ModificationTime, UTC_Time_Offset) /=
          Modification_Time(FileName) then
         Set
           (-(Model), Iter, 5,
            Ada.Calendar.Formatting.Image
              (Date => Modification_Time(FileName),
               Time_Zone => UTC_Time_Offset));
         if Is_Regular_File(FileName) then
            Set(-(Model), Iter, 3, CountFileSize(Size(FileName)));
         end if;
      end if;
      return False;
   end CheckItem;

   task body RefreshTask is
   begin
      accept Start;
      loop
         delay 10.0;
         Set_Sort_Func
           (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
            EmptySortFiles'Access);
         Foreach
           (Gtk_List_Store(Get_Object(Builder, "fileslist")),
            CheckItem'Access);
         Set_Sort_Func
           (Gtk_List_Store(Get_Object(Builder, "fileslist")), 0,
            SortFiles'Access);
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")));
      end loop;
   end RefreshTask;

end RefreshData;
