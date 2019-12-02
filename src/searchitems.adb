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
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with MainWindow; use MainWindow;
with Preferences; use Preferences;

package body SearchItems is

   -- ****if* SearchItems/ToggleSearch
   -- FUNCTION
   -- Show or hide search text entry
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class) is
      -- ****
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

   -- ****if* SearchItems/VisibleItems
   -- FUNCTION
   -- Check if selected file, directory or application should be visible,
   -- when user search for selected names.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model which contains all files and directories in
   --         current directory or all available applications
   -- Iter  - Gtk_Tree_Iter to currently checked file or directory or
   --         application
   -- RESULT
   -- True if selected file, directory or application should be visible,
   -- otherwise false.
   -- SOURCE
   function VisibleItems
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
      -- ****
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "searchfile"));
   begin
      if Setting then
         return True;
      end if;
      if (Get_Int(Model, Iter, 1) = 1 or Get_Int(Model, Iter, 1) = 3) and
        not Settings.ShowHidden then
         return False;
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

   -- ****if* SearchItems/SearchItem
   -- FUNCTION
   -- Search for files and directories as user enter text in search entry
   -- PARAMETERS
   -- User_Data - Which search entry was used for search
   -- SOURCE
   procedure SearchItem(User_Data: access GObject_Record'Class) is
      -- ****
      FilterName, TreeName, ListName: Unbounded_String;
   begin
      FilterName := To_Unbounded_String("filesfilter");
      TreeName := To_Unbounded_String("treefiles");
      ListName := To_Unbounded_String("fileslist");
      if Get_Active(Gtk_Toggle_Tool_Button(Get_Object(Builder, "btncut"))) or
        Get_Active(Gtk_Toggle_Tool_Button(Get_Object(Builder, "btncopy"))) then
         FilterName := To_Unbounded_String("filesfilter2");
         TreeName := To_Unbounded_String("treefiles2");
         ListName := To_Unbounded_String("fileslist2");
      end if;
      Refilter
        (Gtk_Tree_Model_Filter(Get_Object(Builder, To_String(FilterName))));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, To_String(ListName))),
           Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, To_String(TreeName))),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
      Grab_Focus(Gtk_Widget(User_Data));
   end SearchItem;

   procedure CreateSearchUI is
   begin
      Register_Handler(Builder, "Toggle_Search", ToggleSearch'Access);
      Register_Handler(Builder, "Search_Items", SearchItem'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")),
         VisibleItems'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter1")),
         VisibleItems'Access);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter2")),
         VisibleItems'Access);
   end CreateSearchUI;

end SearchItems;
