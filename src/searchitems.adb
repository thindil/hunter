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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with MainWindow; use MainWindow;

package body SearchItems is

   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class) is
      SearchEntry: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "searchfile"));
   begin
      if not Is_Visible(SearchEntry) then
         Show_All(SearchEntry);
         Grab_Focus(SearchEntry);
      else
         Hide(SearchEntry);
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
      end if;
   end ToggleSearch;

   function VisibleFiles(Model: Gtk_Tree_Model;
      Iter: Gtk_Tree_Iter) return Boolean is
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "searchfile"));
   begin
      if Setting then
         return True;
      end if;
      if Get_Text(SearchEntry) = "" then
         return True;
      end if;
      if Index
          (To_Lower(Get_String(Model, Iter, 0)),
           To_Lower(Get_Text(SearchEntry)), 1) >
        0 then
         return True;
      end if;
      return False;
   end VisibleFiles;

   procedure SearchFiles(Object: access Gtkada_Builder_Record'Class) is
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Object, "fileslist")), Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Object, "treefiles")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Object, "searchfile"))) then
         Grab_Focus(Gtk_Widget(Get_Object(Object, "searchfile")));
      end if;
   end SearchFiles;

end SearchItems;
