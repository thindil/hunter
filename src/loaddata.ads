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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Glib; use Glib;

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
   function SortFiles
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint;
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
   function EmptySortFiles
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint;
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
   procedure LoadDirectory(Name, ListName: String);
   -- ****

end LoadData;
