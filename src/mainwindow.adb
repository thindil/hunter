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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Paned; use Gtk.Paned;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Notebook; use Gtk.Notebook;
with Glib; use Glib;

package body MainWindow is

   Builder: Gtkada_Builder;
   CurrentDirectory: Unbounded_String;
   Setting: Boolean;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Gtk.Main.Main_Quit;
   end Quit;

   procedure ResizePaned(Object: access Gtkada_Builder_Record'Class) is
   begin
      Set_Position
        (Gtk_Paned(Get_Object(Object, "paned1")),
         Gint
           (Float
              (Get_Allocated_Width
                 (Gtk_Widget(Get_Object(Object, "mainwindow")))) *
            0.4));
   end ResizePaned;

   procedure LoadDirectory(Name, ListName: String) is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, ListName));
      FileIter: Gtk_Tree_Iter;
      Files, Children: Search_Type;
      FoundFile, FoundChild: Directory_Entry_Type;
      Size, Multiplier: Natural;
      type SizeShortcuts is (B, KiB, MiB, GiB, TiB, PiB);
   begin
      Setting := True;
      FilesList.Clear;
      Start_Search(Files, Name, "");
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         if Simple_Name(FoundFile) = "." or
           (Simple_Name(FoundFile) = ".." and ListName = "fileslist1") then
            goto End_Of_Loop;
         end if;
         Append(FilesList, FileIter);
         Set(FilesList, FileIter, 0, Simple_Name(FoundFile));
         if Kind(FoundFile) = Directory then
            if Simple_Name(FoundFile)(1) = '.' then
               Set(FilesList, FileIter, 2, 1);
            else
               Set(FilesList, FileIter, 2, 2);
            end if;
            Size := 0;
            Start_Search(Children, Full_Name(FoundFile), "");
            while More_Entries(Children) loop
               Get_Next_Entry(Children, FoundChild);
               Size := Size + 1;
            end loop;
            End_Search(Children);
            if Size > 1 then
               Size := Size - 2;
            end if;
            Set(FilesList, FileIter, 1, Natural'Image(Size));
            Set(FilesList, FileIter, 3, Gint'Last);
         else
            if Simple_Name(FoundFile)(1) = '.' then
               Set(FilesList, FileIter, 2, 3);
            else
               Set(FilesList, FileIter, 2, 4);
            end if;
            if Kind(Full_Name(FoundFile)) = Ordinary_File then
               Size := Natural(Ada.Directories.Size(Full_Name(FoundFile)));
               Multiplier := 0;
               while Size > 1024 loop
                  Size := Size / 1024;
                  Multiplier := Multiplier + 1;
               end loop;
               Set
                 (FilesList, FileIter, 1,
                  Natural'Image(Size) & " " &
                  SizeShortcuts'Image(SizeShortcuts'Val(Multiplier)));
               Set
                 (FilesList, FileIter, 3,
                  Gint(Ada.Directories.Size(Full_Name(FoundFile))));
            else
               Set(FilesList, FileIter, 1, "0");
               Set(FilesList, FileIter, 3, 0);
            end if;
         end if;
         <<End_Of_Loop>>
      end loop;
      End_Search(Files);
      Set_Sort_Column_Id(FilesList, 0, Sort_Ascending);
      if ListName = "fileslist" then
         declare
            Filesbook: constant Gtk_Notebook :=
              Gtk_Notebook(Get_Object(Builder, "filesbook"));
         begin
            Set_Tab_Label_Text
              (Filesbook, Get_Nth_Page(Filesbook, 0),
               To_String(CurrentDirectory));
         end;
      end if;
      Setting := False;
   end LoadDirectory;

   function SortFiles(Model: Gtk_Tree_Model; A: Gtk_Tree_Iter;
      B: Gtk_Tree_Iter) return Gint is
      FileTypeA: constant Gint := Get_Int(Model, A, 2);
      FileTypeB: constant Gint := Get_Int(Model, B, 2);
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

   procedure ShowFileInfo(Object: access Gtkada_Builder_Record'Class) is
      FilesIter: Gtk_Tree_Iter;
      FilesModel: Gtk_Tree_Model;
   begin
      if Setting then
         return;
      end if;
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         FilesModel, FilesIter);
      if FilesIter = Null_Iter then
         return;
      end if;
      if Get_Int(FilesModel, FilesIter, 2) < 3 then
         LoadDirectory
           (To_String(CurrentDirectory) & "/" &
            Get_String(FilesModel, FilesIter, 0),
            "fileslist1");
         Show_All(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
      else
         Show_All(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
      end if;
   end ShowFileInfo;

   procedure ActivateFile(Object: access Gtkada_Builder_Record'Class) is
      FilesIter: Gtk_Tree_Iter;
      FilesModel: Gtk_Tree_Model;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         FilesModel, FilesIter);
      if FilesIter = Null_Iter then
         return;
      end if;
      if Get_Int(FilesModel, FilesIter, 2) < 3 then
         CurrentDirectory :=
           CurrentDirectory &
           To_Unbounded_String("/" & Get_String(FilesModel, FilesIter, 0));
         LoadDirectory(To_String(CurrentDirectory), "fileslist");
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end ActivateFile;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Resize_Paned", ResizePaned'Access);
      Register_Handler(Builder, "Show_File_Info", ShowFileInfo'Access);
      Register_Handler(Builder, "Activate_File", ActivateFile'Access);
      Do_Connect(Builder);
      Set_Sort_Func
        (Gtk_List_Store(Get_Object(Builder, "fileslist")), 0,
         SortFiles'Access);
      CurrentDirectory := To_Unbounded_String(Value("HOME"));
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end CreateMainWindow;

end MainWindow;
