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
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Button; use Gtk.Button;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Glib; use Glib;
with Glib.Values; use Glib.Values;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
with MainWindow; use MainWindow;
with Utils; use Utils;

package body LoadData is

   Accelerators: Gtk_Accel_Group;

   -- ****if* LoadData/SortFiles
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
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
      -- ****
      FileTypeA: constant Gint := Get_Int(Model, A, 1);
      FileTypeB: constant Gint := Get_Int(Model, B, 1);
      FileNameA: constant String := Get_String(Model, A, 0);
      FileNameB: constant String := Get_String(Model, B, 0);
   begin
      if FileTypeA > FileTypeB then
         return 1;
      end if;
      if FileTypeA < FileTypeB then
         return -1;
      end if;
      if To_Lower(FileNameA) > To_Lower(FileNameB) then
         return 1;
      end if;
      if To_Lower(FileNameA) < To_Lower(FileNameB) then
         return -1;
      end if;
      return 0;
   end SortFiles;

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
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
      pragma Unreferenced(Model);
      pragma Unreferenced(A);
      pragma Unreferenced(B);
      -- ****
   begin
      return 0;
   end EmptySortFiles;

   -- ****if* LoadData/RemovePathButtons
   -- FUNCTION
   -- Remove selected button from path buttons
   -- PARAMETERS
   -- Widget - Button to remove
   -- SOURCE
   procedure RemovePathButtons
     (Widget: not null access Gtk_Widget_Record'Class) is
   -- ****
   begin
      Set_Accel_Path(Widget, "", Accelerators);
      Destroy(Widget);
   end RemovePathButtons;

   -- ****if* LoadData/PathClicked
   -- FUNCTION
   -- Go to selected location and show it in current directory view.
   -- PARAMETERS
   -- Self - Button which was clicked by user
   -- SOURCE
   procedure PathClicked(Self: access Gtk_Button_Record'Class) is
      -- ****
      Value: GValue;
      Tokens: Slice_Set;
   begin
      Init_Set_Int(Value, 0);
      Child_Get_Property
        (Gtk_Box(Get_Parent(Self)), Gtk_Widget(Self), "position", Value);
      if Get_Int(Value) > 0 then
         Create(Tokens, To_String(CurrentDirectory), "/");
         CurrentDirectory := Null_Unbounded_String;
         for I in 2 .. (Get_Int(Value) + 1) loop
            Append(CurrentDirectory, "/" & Slice(Tokens, Slice_Number(I)));
         end loop;
      else
         CurrentDirectory := To_Unbounded_String("/");
      end if;
      if Get_Parent(Self) = Gtk_Widget(Get_Object(Builder, "boxpath")) then
         Reload(Builder);
      else
         DestinationPath := CurrentDirectory;
         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
      end if;
   end PathClicked;

   procedure LoadDirectory(Name, ListName: String) is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, ListName));
      FileIter: Gtk_Tree_Iter;
      Size: File_Size;
      Directory, SubDirectory: Dir_Type;
      Last, SubLast: Natural;
      FileName, SubFileName: String(1 .. 1024);
      MainWindow: constant Gdk_Window :=
        Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow")));
   begin
      Setting := True;
      if Accelerators = null then
         Accelerators := Gtk_Accel_Group(Get_Object(Builder, "accelerators"));
      end if;
      if MainWindow /= null then
         Set_Cursor(MainWindow, Gdk_Cursor_New(Watch));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), False);
         while Events_Pending loop
            if Main_Iteration_Do(False) then
               exit;
            end if;
         end loop;
      end if;
      FilesList.Clear;
      if not Is_Read_Accessible_File(Name) then
         Set_Text
           (Get_Buffer(Gtk_Text_View(Get_Object(Builder, "filetextview"))),
            "You don't have permissions to preview this directory.");
         Show_All(Gtk_Widget(Get_Object(Builder, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Builder, "scrolllist")));
         if MainWindow /= null then
            Set_Cursor
              (Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow"))),
               Gdk_Cursor_New(Arrow));
            Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), True);
            Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnopen")), False);
         end if;
         Setting := False;
         return;
      end if;
      if ListName /= "fileslist1" then
         Set_Sort_Func
           (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
            EmptySortFiles'Access);
      else
         Set_Sort_Func
           (Gtk_List_Store(Get_Object(Builder, ListName)), 0,
            EmptySortFiles'Access);
      end if;
      Open(Directory, Name);
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) = "." or FileName(1 .. Last) = ".." then
            goto End_Of_Loop;
         end if;
         Append(FilesList, FileIter);
         Set(FilesList, FileIter, 0, FileName(1 .. Last));
         if Is_Directory(Name & "/" & FileName(1 .. Last)) then
            if FileName(1) = '.' then
               Set(FilesList, FileIter, 1, 1);
            else
               Set(FilesList, FileIter, 1, 2);
            end if;
            if Is_Symbolic_Link(Name & "/" & FileName(1 .. Last)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "folder");
            end if;
            if ListName = "fileslist1" then
               goto End_Of_Loop;
            end if;
            Set(FilesList, FileIter, 4, Gint'Last);
            if Is_Read_Accessible_File(Name & "/" & FileName(1 .. Last)) then
               Open(SubDirectory, Name & "/" & FileName(1 .. Last));
               Size := 0;
               loop
                  Read(SubDirectory, SubFileName, SubLast);
                  exit when SubLast = 0;
                  Size := Size + 1;
               end loop;
               Close(SubDirectory);
               Set(FilesList, FileIter, 3, File_Size'Image(Size - 2));
            else
               Set(FilesList, FileIter, 3, "?");
            end if;
         else
            if FileName(1) = '.' then
               Set(FilesList, FileIter, 1, 3);
            else
               Set(FilesList, FileIter, 1, 4);
            end if;
            if Is_Symbolic_Link(Name & "/" & FileName(1 .. Last)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "text-x-generic-template");
            end if;
            if ListName = "fileslist1" then
               goto End_Of_Loop;
            end if;
            if not Is_Read_Accessible_File
                (Name & "/" & FileName(1 .. Last)) then
               Set(FilesList, FileIter, 3, "?");
               Set(FilesList, FileIter, 4, 0);
               goto End_Of_Loop;
            end if;
            if Is_Symbolic_Link(Name & "/" & FileName(1 .. Last)) then
               Set(FilesList, FileIter, 3, "->");
               Set(FilesList, FileIter, 4, 0);
            elsif Is_Regular_File(Name & "/" & FileName(1 .. Last)) then
               Size := Ada.Directories.Size(Name & "/" & FileName(1 .. Last));
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
      Close(Directory);
      if ListName /= "fileslist1" then
         declare
            FilesSort: Gtk_Tree_Model_Sort;
            Tokens: Slice_Set;
            Button: Gtk_Button;
            ButtonBox: Gtk_Box;
         begin
            if ListName = "fileslist" then
               FilesSort :=
                 Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort"));
               ButtonBox := Gtk_Box(Get_Object(Builder, "boxpath"));
            else
               FilesSort :=
                 Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort2"));
               ButtonBox := Gtk_Box(Get_Object(Builder, "boxpath2"));
            end if;
            Foreach(ButtonBox, RemovePathButtons'Access);
            CurrentDirectory :=
              To_Unbounded_String
                (Normalize_Pathname(To_String(CurrentDirectory)));
            Gtk_New(Button, "/");
            Pack_Start(ButtonBox, Button);
            On_Clicked(Button, PathClicked'Access);
            Create(Tokens, To_String(CurrentDirectory), "/");
            if CurrentDirectory = To_Unbounded_String("/") then
               if ListName = "fileslist" then
                  Set_Tooltip_Text
                    (Gtk_Widget(Button), "Reload current directory [ALT+R]");
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/reload", Accelerators);
               else
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     "Reload current directory [ALT+SHIFT+R]");
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/reload2", Accelerators);
               end if;
            elsif Slice_Count(Tokens) = 2 then
               if ListName = "fileslist" then
                  Set_Tooltip_Text
                    (Gtk_Widget(Button), "Go to upper directory [ALT+U]");
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/goup", Accelerators);
               else
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     "Go to upper directory [ALT+SHIFT+U]");
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/goup2", Accelerators);
               end if;
            else
               if ListName = "fileslist" then
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/path1", Accelerators);
                  Set_Tooltip_Text
                    (Gtk_Widget(Button), "Go to this directory [ALT+1]");
               else
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/path12", Accelerators);
                  Set_Tooltip_Text
                    (Gtk_Widget(Button), "Go to this directory [ALT+SHIFT+1]");
               end if;
            end if;
            for I in 2 .. Slice_Count(Tokens) loop
               if Slice(Tokens, I) /= "" then
                  Gtk_New(Button, Slice(Tokens, I));
                  Pack_Start(ButtonBox, Button);
                  On_Clicked(Button, PathClicked'Access);
                  if I = Slice_Count(Tokens) - 1 then
                     if ListName = "fileslist" then
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Go to upper directory [ALT+U]");
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/goup",
                           Accelerators);
                     else
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Go to upper directory [ALT+SHIFT+U]");
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/goup2",
                           Accelerators);
                     end if;
                  elsif I = Slice_Count(Tokens) then
                     if ListName = "fileslist" then
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Reload current directory [ALT+R]");
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/reload",
                           Accelerators);
                     else
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Reload current directory [ALT+SHIFT+R]");
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/reload2",
                           Accelerators);
                     end if;
                  elsif I < 11 then
                     if ListName = "fileslist" then
                        Set_Accel_Path
                          (Gtk_Widget(Button),
                           "<mainwindow>/path" & Slice_Number'Image(I)(2),
                           Accelerators);
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Go to this directory [ALT+" &
                           Slice_Number'Image(I)(2) & "]");
                     else
                        Set_Accel_Path
                          (Gtk_Widget(Button),
                           "<mainwindow>/path" & Slice_Number'Image(I)(2) &
                           "2",
                           Accelerators);
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           "Go to this directory [ALT+SHIFT+" &
                           Slice_Number'Image(I)(2) & "]");
                     end if;
                  end if;
               end if;
            end loop;
            Show_All(ButtonBox);
            Set_Sort_Func(FilesSort, 0, SortFiles'Access);
            Set_Sort_Column_Id(FilesSort, 0, Sort_Ascending);
         end;
      else
         Set_Sort_Func
           (Gtk_List_Store(Get_Object(Builder, ListName)), 0,
            SortFiles'Access);
         Set_Sort_Column_Id(FilesList, 0, Sort_Ascending);
      end if;
      if MainWindow /= null then
         Set_Cursor
           (Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow"))),
            Gdk_Cursor_New(Arrow));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), True);
      end if;
      Setting := False;
   end LoadDirectory;

end LoadData;
