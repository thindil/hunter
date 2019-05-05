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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Button; use Gtk.Button;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Stack; use Gtk.Stack;
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

package body LoadData is

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
   function SortFiles(Model: Gtk_Tree_Model; A: Gtk_Tree_Iter;
      B: Gtk_Tree_Iter) return Gint is
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
   function EmptySortFiles(Model: Gtk_Tree_Model; A: Gtk_Tree_Iter;
      B: Gtk_Tree_Iter) return Gint is
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
        (Gtk_Box(Get_Object(Builder, "boxpath")), Gtk_Widget(Self), "position",
         Value);
      if Get_Int(Value) > 0 then
         Create(Tokens, To_String(CurrentDirectory), "/");
         CurrentDirectory := Null_Unbounded_String;
         for I in 2 .. (Get_Int(Value) + 1) loop
            Append(CurrentDirectory, "/" & Slice(Tokens, Slice_Number(I)));
         end loop;
      else
         CurrentDirectory := To_Unbounded_String("/");
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoup")), False);
      end if;
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Reload(Builder);
   end PathClicked;

   procedure LoadDirectory(Name, ListName: String) is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, ListName));
      FileIter: Gtk_Tree_Iter;
      Files, Children: Search_Type;
      FoundFile, FoundChild: Directory_Entry_Type;
      Size: File_Size;
      Multiplier: Natural;
      SizeShortcuts: constant array(Natural range <>) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
      MainWindow: constant Gdk_Window :=
        Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow")));
   begin
      Setting := True;
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
      if ListName = "fileslist1" then
         Set_Sort_Func
           (Gtk_List_Store(Get_Object(Builder, ListName)), 0,
            EmptySortFiles'Access);
      else
         Set_Sort_Func
           (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
            EmptySortFiles'Access);
      end if;
      Start_Search(Files, Name, "");
      loop
         begin
            exit when not More_Entries(Files);
            Get_Next_Entry(Files, FoundFile);
         exception
            when Ada.Directories.Use_Error =>
               null;
            when Ada.Directories.Status_Error =>
               exit;
         end;
         if Simple_Name(FoundFile) = "." or Simple_Name(FoundFile) = ".." then
            goto End_Of_Loop;
         end if;
         Append(FilesList, FileIter);
         Set(FilesList, FileIter, 0, Simple_Name(FoundFile));
         if Is_Directory(Full_Name(FoundFile)) then
            if Simple_Name(FoundFile)(1) = '.' then
               Set(FilesList, FileIter, 1, 1);
            else
               Set(FilesList, FileIter, 1, 2);
            end if;
            if Is_Symbolic_Link(Full_Name(FoundFile)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "folder");
            end if;
            if ListName = "fileslist1" then
               goto End_Of_Loop;
            end if;
            Set(FilesList, FileIter, 4, Gint'Last);
            if not Is_Read_Accessible_File(Full_Name(FoundFile)) then
               Set(FilesList, FileIter, 3, "?");
               goto End_Of_Loop;
            end if;
            Size := 0;
            Start_Search(Children, Full_Name(FoundFile), "");
            loop
               Size := Size + 1;
               begin
                  exit when not More_Entries(Children);
                  Get_Next_Entry(Children, FoundChild);
               exception
                  when Ada.Directories.Use_Error =>
                     null;
                  when Ada.Directories.Status_Error =>
                     exit;
               end;
            end loop;
            End_Search(Children);
            Set(FilesList, FileIter, 3, File_Size'Image(Size - 3));
         else
            if Simple_Name(FoundFile)(1) = '.' then
               Set(FilesList, FileIter, 1, 3);
            else
               Set(FilesList, FileIter, 1, 4);
            end if;
            if Is_Symbolic_Link(Full_Name(FoundFile)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "text-x-generic-template");
            end if;
            if ListName = "fileslist1" then
               goto End_Of_Loop;
            end if;
            if not Is_Read_Accessible_File(Full_Name(FoundFile)) then
               Set(FilesList, FileIter, 3, "?");
               Set(FilesList, FileIter, 4, 0);
               goto End_Of_Loop;
            end if;
            if Kind(Full_Name(FoundFile)) = Ordinary_File then
               Size := Ada.Directories.Size(Full_Name(FoundFile));
               Multiplier := 0;
               while Size > 1024 loop
                  Size := Size / 1024;
                  Multiplier := Multiplier + 1;
               end loop;
               Set
                 (FilesList, FileIter, 3,
                  File_Size'Image(Size) & " " & SizeShortcuts(Multiplier));
               Size := Ada.Directories.Size(Full_Name(FoundFile));
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
      End_Search(Files);
      if ListName = "fileslist" then
         declare
            FileStack: constant Gtk_Stack :=
              Gtk_Stack(Get_Object(Builder, "filestack"));
            FilesSort: constant Gtk_Tree_Model_Sort :=
              Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort"));
            Value: GValue;
            Tokens: Slice_Set;
            Button: Gtk_Button;
            ButtonBox: constant Gtk_Box :=
              Gtk_Box(Get_Object(Builder, "boxpath"));
         begin
            Init_Set_String(Value, To_String(CurrentDirectory));
            Child_Set_Property
              (FileStack, Get_Visible_Child(FileStack), "title", Value);
            Foreach(ButtonBox, RemovePathButtons'Access);
            Gtk_New(Button, "/");
            Pack_Start(ButtonBox, Button);
            On_Clicked(Button, PathClicked'Access);
            if CurrentDirectory /= To_Unbounded_String("/") then
               Create(Tokens, To_String(CurrentDirectory), "/");
               for I in 2 .. Slice_Count(Tokens) loop
                  if Slice(Tokens, I) /= "" then
                     Gtk_New(Button, Slice(Tokens, I));
                     Pack_Start(ButtonBox, Button);
                     On_Clicked(Button, PathClicked'Access);
                  end if;
               end loop;
            end if;
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
