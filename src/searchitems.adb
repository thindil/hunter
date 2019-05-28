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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
         Set_Text(Gtk_GEntry(SearchEntry), "");
         Hide(SearchEntry);
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
      end if;
   end ToggleSearch;

   function VisibleItems(Model: Gtk_Tree_Model;
      Iter: Gtk_Tree_Iter) return Boolean is
      SearchEntry: Gtk_GEntry;
   begin
      if Setting then
         return True;
      end if;
      if Model = +(Gtk_List_Store(Get_Object(Builder, "fileslist"))) then
         SearchEntry := Gtk_GEntry(Get_Object(Builder, "searchfile"));
      else
         SearchEntry := Gtk_GEntry(Get_Object(Builder, "searchapplication"));
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
   end VisibleItems;

   procedure SearchItem(User_Data: access GObject_Record'Class) is
      Name: Unbounded_String;
   begin
      if User_Data = Get_Object(Builder, "searchfile") then
         Name := To_Unbounded_String("file");
      else
         Name := To_Unbounded_String("application");
      end if;
      Refilter
        (Gtk_Tree_Model_Filter
           (Get_Object(Builder, To_String(Name) & "sfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, To_String(Name) & "slist")),
           Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "tree" & To_String(Name) & "s")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
      if Is_Visible
          (Gtk_Widget(Get_Object(Builder, "search" & To_String(Name)))) then
         Grab_Focus
           (Gtk_Widget(Get_Object(Builder, "search" & To_String(Name))));
      end if;
   end SearchItem;

end SearchItems;
