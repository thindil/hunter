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
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtkada.Builder; use Gtkada.Builder;
with MainWindow; use MainWindow;

package body RefreshData is

   -- ****iv* RefreshData/LastCheck
   -- FUNCTION
   -- Time when the program last check for modification time of files and
   -- directories
   -- SOURCE
   LastCheck: Time;
   -- ****

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
          Reload(Builder);
          return True;
      end if;
      return False;
   end CheckItem;

   task body RefreshTask is
   begin
      accept Start;
      loop
         LastCheck := Clock;
         delay 10.0;
         if Modification_Time(To_String(CurrentDirectory)) > LastCheck then
            Reload(Builder);
         else
            Foreach
              (Gtk_List_Store(Get_Object(Builder, "fileslist")),
               CheckItem'Access);
         end if;
      end loop;
   end RefreshTask;

end RefreshData;
