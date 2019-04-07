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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Main; use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Paned; use Gtk.Paned;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Window; use Gtk.Window;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Glib; use Glib;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;

package body MainWindow is

   Builder: Gtkada_Builder;
   CurrentDirectory, CurrentSelected: Unbounded_String;
   Setting: Boolean;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
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
      Size: File_Size;
      Multiplier: Natural;
      SizeShortcuts: constant array(Natural range <>) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
      MainWindow: constant Gdk_Window :=
        Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow")));
   begin
      Setting := True;
      if MainWindow /= null then
         Set_Cursor(MainWindow, Gdk_Cursor_New(Clock));
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
            Set(FilesList, FileIter, 2, "folder");
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
            Set(FilesList, FileIter, 2, "text-x-generic-template");
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
      if MainWindow /= null then
         Set_Cursor
           (Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow"))),
            Gdk_Cursor_New(Arrow));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), True);
      end if;
      Setting := False;
   end LoadDirectory;

   function SortFiles(Model: Gtk_Tree_Model; A: Gtk_Tree_Iter;
      B: Gtk_Tree_Iter) return Gint is
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
      if CurrentSelected =
        To_Unbounded_String(Get_String(FilesModel, FilesIter, 0)) then
         return;
      end if;
      CurrentSelected :=
        To_Unbounded_String(Get_String(FilesModel, FilesIter, 0));
      if Get_Int(FilesModel, FilesIter, 1) < 3 then
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btnopen")), True);
         Show_All(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
         LoadDirectory
           (To_String(CurrentDirectory) & "/" &
            Get_String(FilesModel, FilesIter, 0),
            "fileslist1");
      else
         Show_All(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
      end if;
   end ShowFileInfo;

   procedure ActivateFile(Object: access Gtkada_Builder_Record'Class) is
      FilesIter: Gtk_Tree_Iter;
      FilesModel: Gtk_Tree_Model;
      NewDirectory: Unbounded_String;
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal, Message_Error,
           Buttons_Close, "You can't enter this directory.");
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         FilesModel, FilesIter);
      if FilesIter = Null_Iter then
         return;
      end if;
      if Get_Int(FilesModel, FilesIter, 1) < 3 then
         NewDirectory :=
           To_Unbounded_String(Get_String(FilesModel, FilesIter, 0));
         if not Is_Read_Accessible_File
             (To_String(CurrentDirectory) & "/" & To_String(NewDirectory)) then
            if Run(MessageDialog) /= Gtk_Response_None then
               Destroy(MessageDialog);
            end if;
            return;
         end if;
         if CurrentDirectory = To_Unbounded_String("/") then
            CurrentDirectory := Null_Unbounded_String;
         end if;
         CurrentDirectory :=
           CurrentDirectory &
           To_Unbounded_String("/" & Get_String(FilesModel, FilesIter, 0));
         LoadDirectory(To_String(CurrentDirectory), "fileslist");
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
      end if;
      Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoup")), True);
   end ActivateFile;

   procedure GoUpDirectory(Object: access Gtkada_Builder_Record'Class) is
   begin
      CurrentDirectory :=
        Unbounded_Slice
          (CurrentDirectory, 1, Index(CurrentDirectory, "/", Backward) - 1);
      if CurrentDirectory = Null_Unbounded_String then
         CurrentDirectory := To_Unbounded_String("/");
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoup")), False);
      end if;
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Object, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end GoUpDirectory;

   procedure Reload(Object: access Gtkada_Builder_Record'Class) is
   begin
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Object, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
   end Reload;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Resize_Paned", ResizePaned'Access);
      Register_Handler(Builder, "Show_File_Info", ShowFileInfo'Access);
      Register_Handler(Builder, "Activate_File", ActivateFile'Access);
      Register_Handler(Builder, "Go_Up_Directory", GoUpDirectory'Access);
      Register_Handler(Builder, "Reload", Reload'Access);
      Do_Connect(Builder);
      Set_Sort_Func
        (Gtk_List_Store(Get_Object(Builder, "fileslist")), 0,
         SortFiles'Access);
      Set_Sort_Func
        (Gtk_List_Store(Get_Object(Builder, "fileslist1")), 0,
         SortFiles'Access);
      CurrentDirectory := To_Unbounded_String(Value("HOME"));
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
   end CreateMainWindow;

end MainWindow;
