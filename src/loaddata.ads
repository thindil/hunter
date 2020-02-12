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

--with Gtk.List_Store; use Gtk.List_Store;
--with Gtk.Tree_Model; use Gtk.Tree_Model;
--with Glib; use Glib;
with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Hunter/LoadData
-- FUNCTION
-- Provide code to load directories information.
-- SOURCE
package LoadData is
-- ****

   -- ****f* LoadData/SortFiles
   -- FUNCTION
   -- Sort files and directories in current directory view
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content (files and directories) of current
   --         directory
   -- A     - Gtk_Tree_Iter to first Model element to compare
   -- B     - Gtk_Tree_Iter to second Model element to compare
   -- RESULT
   -- 1 if first element should be sort after second
   -- 0 if first element should be sort with second (equal)
   -- -1 if first element should be sort before second
   -- SOURCE
--   function SortFiles
--     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint;
   -- ****

   -- ****if* LoadData/EmptySortFiles
   -- FUNCTION
   -- Empty sort function used to speed up loading listing of current
   -- directory.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content (files and directories) of current
   --         directory
   -- A     - Gtk_Tree_Iter to first Model element to compare
   -- B     - Gtk_Tree_Iter to second Model element to compare
   -- RESULT
   -- This function always return 0;
   -- SOURCE
--   function EmptySortFiles
--     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint;
   -- ****

   -- ****f* LoadData/LoadDirectory
   -- FUNCTION
   -- Load selected directory with Name to Gtk_Store_List with ListName
   -- PARAMETERS
   -- Name     - Full path to the directory which content will be displayed
   -- ListName - Name of list which will be filled with data. Proper values
   --            are: "fileslist" for current directory and "fileslist1" for
   --            directory preview
   -- SOURCE
--   procedure LoadDirectory(Name, ListName: String);
   -- ****

   -- ****f* LoadData/AddItem
   -- FUNCTION
   -- Add file or directory to the directory listing list
   -- PARAMETERS
   -- FilesList - Gtk_List_Store with list of files and directories to which
   --             item will be added
   -- FileIter  - Gtk_Tree_Iter of added item in FilesList
   -- Path      - Full path to the file or directory which will be added to
   --             the FilesList.
   -- RESULT
   -- Parameter FileIter
   -- SOURCE
--   procedure AddItem
--     (FilesList: Gtk_List_Store; FileIter: out Gtk_Tree_Iter; Path: String);
   -- ****

   type Item_Record is record
      Name: Unbounded_String;
      Size: Integer;
      IsDirectory: Boolean;
      IsHidden: Boolean;
      Modified: Time;
      Image: Unbounded_String;
   end record;

   type SortingOrder is (NameAsc, NameDesc, ModifiedAsc, ModifiedDesc, SizeAsc, SizeDesc);

   SortOrder: SortingOrder := NameAsc;

   function "<" (Left, Right : Item_Record) return Boolean;
   function "=" (Left, Right : Item_Record) return Boolean;

   package Items_Container is new Vectors(Positive, Item_Record);
   package Items_Sorting is new Items_Container.Generic_Sorting;

   ItemsList: Items_Container.Vector;

   procedure LoadDirectory(DirectoryName: String);

end LoadData;
