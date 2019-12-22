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
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body SearchItems is

   -- ****if* SearchItems/ToggleSearch
   -- FUNCTION
   -- Show or hide search text entry
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      -- ****
   begin
      if not Is_Visible(SearchEntry) then
         Show_All(SearchEntry);
         Grab_Focus(SearchEntry);
      else
         Set_Text(Gtk_GEntry(SearchEntry), "");
         Hide(SearchEntry);
         Grab_Focus(DirectoryView);
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
   -- Self - Which search entry was used for search.
   -- SOURCE
   procedure SearchItem(Self: access Gtk_Search_Entry_Record'Class) is
      -- ****
      FilterName: Unbounded_String;
      TreeView: Gtk_Tree_View := DirectoryView;
   begin
      FilterName := To_Unbounded_String("filesfilter");
      if Get_Visible_Child_Name(InfoStack) = "destination" then
         FilterName := To_Unbounded_String("filesfilter2");
         TreeView :=
           Gtk_Tree_View
             (Get_Child(Gtk_Scrolled_Window(Get_Visible_Child(InfoStack))));
      end if;
      Refilter
        (Gtk_Tree_Model_Filter(Get_Object(Builder, To_String(FilterName))));
      if Gtk.Tree_Model_Sort.N_Children(-(Get_Model(TreeView)), Null_Iter) >
        0 then
         Set_Cursor(TreeView, Gtk_Tree_Path_New_From_String("0"), null, False);
      else
         CurrentSelected := CurrentDirectory;
         PreviewItem(null);
      end if;
      Grab_Focus(Self);
      Select_Region(Self, 0, 0);
      Set_Position(Self, Get_Text(Self)'Length);
   end SearchItem;

   procedure CreateSearchUI is
   begin
      On_Search_Changed(SearchEntry, SearchItem'Access);
      Register_Handler(Builder, "Toggle_Search", ToggleSearch'Access);
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
