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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
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
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
      FilesSort: constant Gtk_Tree_Model_Sort :=
        Gtk_Tree_Model_Sort(Get_Object(Object, "filessort"));
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
      Hide(Gtk_Widget(Get_Object(Object, "boxpath")));
      Set_Sort_Func
        (Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort")), 0,
         EmptySortFiles'Access);
      Open(Directory, Value("HOME") & "/.local/share/Trash/files");
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         Append(FilesList, FileIter);
         Set
           (FilesList, FileIter, 6,
            Value("HOME") & "/.local/share/Trash/files/" &
            FileName(1 .. Last));
      end loop;
      Close(Directory);
      Set_Sort_Func(FilesSort, 0, SortFiles'Access);
      Set_Sort_Column_Id(FilesSort, 0, Sort_Ascending);
      if MainWindow /= null then
         Set_Cursor
           (Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow"))),
            Gdk_Cursor_New(Arrow));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), True);
      end if;
      Setting := False;
   end ShowTrash;

end Trash;
