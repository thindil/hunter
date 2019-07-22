-- /home/thindil/Projekty/hunter/hunter/src/trash.adb
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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
with Glib; use Glib;
with MainWindow; use MainWindow;
with LoadData; use LoadData;
with Messages; use Messages;
with Utils; use Utils;

package body Trash is

   procedure ClearTrash(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      NewAction := CLEARTRASH;
      ToggleToolButtons(NewAction);
      ShowMessage
        (Gettext("Remove all files and directories from Trash?"),
         Message_Question);
   end ClearTrash;

   procedure ShowTrash(Object: access Gtkada_Builder_Record'Class) is
      FilesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "fileslist"));
      FileIter: Gtk_Tree_Iter;
      MainWindow: constant Gdk_Window :=
        Get_Window(Gtk_Widget(Get_Object(Object, "mainwindow")));
      Directory, SubDirectory: Dir_Type;
      Last, SubLast: Natural;
      FileName, SubFileName: String(1 .. 1024);
      FilesSort: constant Gtk_Tree_Model_Sort :=
        Gtk_Tree_Model_Sort(Get_Object(Object, "filessort"));
      FileInfo: File_Type;
      Size: File_Size;
      FileLine: Unbounded_String;
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
      Set_Sort_Func
        (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
         EmptySortFiles'Access);
      Open(Directory, Value("HOME") & "/.local/share/Trash/files");
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) = "." or FileName(1 .. Last) = ".." then
            goto End_Of_Loop;
         end if;
         Append(FilesList, FileIter);
         Set
           (FilesList, FileIter, 6,
            Value("HOME") & "/.local/share/Trash/files/" &
            FileName(1 .. Last));
         Open
           (FileInfo, In_File,
            Value("HOME") & "/.local/share/Trash/info" & FileName(1 .. Last) &
            ".trashinfo");
         Skip_Line(FileInfo);
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Set
                 (FilesList, FileIter, 0,
                  Slice(FileLine, 5, Length(FileLine)));
            else
               Set
                 (FilesList, FileIter, 5,
                  Slice(FileLine, 13, Length(FileLine)));
            end if;
         end loop;
         Close(FileInfo);
         if Is_Directory
             (Value("HOME") & "/.local/share/Trash/files/" &
              FileName(1 .. Last)) then
            if FileName(1) = '.' then
               Set(FilesList, FileIter, 1, 1);
            else
               Set(FilesList, FileIter, 1, 2);
            end if;
            if Is_Symbolic_Link
                (Value("HOME") & "/.local/share/Trash/files/" &
                 FileName(1 .. Last)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "folder");
            end if;
            Set(FilesList, FileIter, 4, Gint'Last);
            if Is_Read_Accessible_File
                (Value("HOME") & "/.local/share/Trash/files/" &
                 FileName(1 .. Last)) then
               Open
                 (SubDirectory,
                  Value("HOME") & "/.local/share/Trash/files/" &
                  FileName(1 .. Last));
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
            if Is_Symbolic_Link
                (Value("HOME") & "/.local/share/Trash/files/" & "/" &
                 FileName(1 .. Last)) then
               Set(FilesList, FileIter, 2, "emblem-symbolic-link");
            else
               Set(FilesList, FileIter, 2, "text-x-generic-template");
            end if;
            if not Is_Read_Accessible_File
                (Value("HOME") & "/.local/share/Trash/files/" &
                 FileName(1 .. Last)) then
               Set(FilesList, FileIter, 3, "?");
               Set(FilesList, FileIter, 4, 0);
               goto End_Of_Loop;
            end if;
            if Is_Symbolic_Link
                (Value("HOME") & "/.local/share/Trash/files/" &
                 FileName(1 .. Last)) then
               Set(FilesList, FileIter, 3, "->");
               Set(FilesList, FileIter, 4, 0);
            elsif Is_Regular_File
                (Value("HOME") & "/.local/share/Trash/files/" &
                 FileName(1 .. Last)) then
               Size :=
                 Ada.Directories.Size
                   (Value("HOME") & "/.local/share/Trash/files/" &
                    FileName(1 .. Last));
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
      Set_Sort_Func(FilesSort, 0, SortFiles'Access);
      Set_Sort_Column_Id(FilesSort, 0, Sort_Ascending);
      if MainWindow /= null then
         Set_Cursor
           (Get_Window(Gtk_Widget(Get_Object(Object, "mainwindow"))),
            Gdk_Cursor_New(Arrow));
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "mainwindow")), True);
      end if;
      Setting := False;
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter")));
      Set_Cursor
         (Gtk_Tree_View(Get_Object(Object, "treefiles")), Gtk_Tree_Path_New_From_String("0"), null,
         False);
      Grab_Focus(Gtk_Widget(Get_Object(Object, "treefiles")));
   end ShowTrash;

end Trash;
