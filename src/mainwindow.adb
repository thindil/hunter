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
with Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with ActivateItems; use ActivateItems;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with Messages; use Messages;
with MoveItems; use MoveItems;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;

package body MainWindow is

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
   end Quit;

   procedure Reload(Object: access Gtkada_Builder_Record'Class) is
   begin
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Object, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Grab_Focus(Gtk_Widget(Get_Object(Object, "treefiles")));
      PreviewItem(Object);
   end Reload;

   -- ****if* MainWindow/StartRename
   -- FUNCTION
   -- Show text entry to start renaming selected file or directory and fill it
   -- with current element name.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure StartRename(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      GEntry: constant Gtk_Widget := Gtk_Widget(Get_Object(Object, "entry"));
   begin
      NewAction := RENAME;
      if Is_Directory(To_String(CurrentSelected)) then
         Set_Icon_Tooltip_Text
           (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary, "Rename directory.");
      else
         Set_Icon_Tooltip_Text
           (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary, "Rename file.");
      end if;
      Set_Text(Gtk_GEntry(GEntry), Simple_Name(To_String(CurrentSelected)));
      Show_All(GEntry);
      Grab_Focus(GEntry);
   end StartRename;

   -- ****if* MainWindow/ShowFiles
   -- FUNCTION
   -- Back to files listing and preview/info
   -- PARAMETERS
   -- User_Data - Which button was clicked
   -- SOURCE
   procedure ShowFiles(User_Data: access GObject_Record'Class) is
      -- ****
      pragma Unreferenced(User_Data);
   begin
      Show_All(Gtk_Widget(Get_Object(Builder, "toolbar")));
      Show_All(Gtk_Widget(Get_Object(Builder, "boxpath")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolapply")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
      Reload(Builder);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "filestack")), "files");
   end ShowFiles;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder; Directory: String) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_Item", ShowItem'Access);
      Register_Handler(Builder, "Activate_File", ActivateFile'Access);
      Register_Handler(Builder, "Toggle_Search", ToggleSearch'Access);
      Register_Handler(Builder, "Search_Files", SearchFiles'Access);
      Register_Handler(Builder, "Add_New", AddNew'Access);
      Register_Handler(Builder, "Delete_Item", DeleteItem'Access);
      Register_Handler(Builder, "Create_New", CreateNew'Access);
      Register_Handler(Builder, "Start_Rename", StartRename'Access);
      Register_Handler(Builder, "Move_Items", MoveData'Access);
      Register_Handler(Builder, "Copy_Items", CopyData'Access);
      Register_Handler(Builder, "Go_Home", GoHome'Access);
      Register_Handler(Builder, "Hide_Message", HideMessage'Access);
      Register_Handler(Builder, "Set_Response", SetResponse'Access);
      Register_Handler(Builder, "Start_Open_With", StartOpenWith'Access);
      Register_Handler(Builder, "Execute_File", ExecuteFile'Access);
      Register_Handler(Builder, "Preview_Item", PreviewItem'Access);
      Register_Handler(Builder, "Show_Item_Info", ShowItemInfo'Access);
      Register_Handler(Builder, "Show_Files", ShowFiles'Access);
      Register_Handler(Builder, "Set_Associated", SetAssociated'Access);
      Register_Handler
        (Builder, "Create_Bookmark_Menu", CreateBookmarkMenu'Access);
      Do_Connect(Builder);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")),
         VisibleFiles'Access);
      On_Icon_Press
        (Gtk_GEntry(Get_Object(Builder, "entry")), IconPressed'Access);
      On_Response
        (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")),
         MessageResponse'Access);
      Add_Entry("<mainwindow>/reload", GDK_LC_r, 8);
      Add_Entry("<mainwindow>/goup", GDK_LC_u, 8);
      Add_Entry("<mainwindow>/path1", GDK_1, 8);
      Add_Entry("<mainwindow>/path2", GDK_2, 8);
      Add_Entry("<mainwindow>/path3", GDK_3, 8);
      Add_Entry("<mainwindow>/path4", GDK_4, 8);
      Add_Entry("<mainwindow>/path5", GDK_5, 8);
      Add_Entry("<mainwindow>/path6", GDK_6, 8);
      Add_Entry("<mainwindow>/path7", GDK_7, 8);
      Add_Entry("<mainwindow>/path8", GDK_9, 8);
      Add_Entry("<mainwindow>/path9", GDK_9, 8);
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists(To_String(CurrentDirectory)) then
            CurrentDirectory := To_Unbounded_String("/");
         end if;
      end if;
      declare
         ApplicationsPaths: constant array
           (Positive range <>) of Unbounded_String :=
           (To_Unbounded_String("/usr/share/applications"),
            To_Unbounded_String("/usr/share/applnk"),
            To_Unbounded_String("/usr/local/share/applications"),
            To_Unbounded_String("/usr/local/share/applnk"),
            To_Unbounded_String(Value("HOME") & "/.local/share/applications"),
            To_Unbounded_String(Value("HOME") & "/.local/share/applnk"));
         SubDirectory: Dir_Type;
         SubLast: Natural;
         SubFileName: String(1 .. 1024);
         File: File_Type;
         FileLine: Unbounded_String;
         FilesList: constant Gtk_List_Store :=
           Gtk_List_Store(Get_Object(Builder, "applicationsstore"));
         FileIter: Gtk_Tree_Iter;
      begin
         for Path of ApplicationsPaths loop
            if not Ada.Directories.Exists(To_String(Path)) then
               goto End_Of_Loop;
            end if;
            Open(SubDirectory, To_String(Path));
            loop
               Read(SubDirectory, SubFileName, SubLast);
               exit when SubLast = 0;
               if Extension(SubFileName(1 .. SubLast)) = "desktop" then
                  Open
                    (File, In_File,
                     To_String(Path) & "/" &
                     Simple_Name(SubFileName(1 .. SubLast)));
                  while not End_Of_File(File) loop
                     FileLine := To_Unbounded_String(Get_Line(File));
                     if Length(FileLine) > 5
                       and then Slice(FileLine, 1, 5) = "Name=" then
                        Append(FilesList, FileIter);
                        Set
                          (FilesList, FileIter, 0,
                           Slice(FileLine, 6, Length(FileLine)));
                        Set(FilesList, FileIter, 1, SubFileName(1 .. SubLast));
                        exit;
                     end if;
                  end loop;
                  Close(File);
               end if;
            end loop;
            Close(SubDirectory);
            <<End_Of_Loop>>
         end loop;
      end;
      Set_Menu
        (Gtk_Menu_Tool_Button(Get_Object(Builder, "btnnew")),
         Gtk_Widget(Get_Object(Builder, "newmenu")));
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Set_Menu
        (Gtk_Menu_Tool_Button(Get_Object(Builder, "btnbookmarks")),
         Gtk_Widget(Get_Object(Builder, "bookmarksmenu")));
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
      HideMessage(Builder);
      Hide(Gtk_Widget(Get_Object(Builder, "searchfile")));
      Hide(Gtk_Widget(Get_Object(Builder, "entry")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolapply")));
      Hide(Gtk_Widget(Get_Object(Builder, "btntoolcancel")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Set_Position
        (Gtk_Paned(Get_Object(Builder, "filespaned")),
         Gint
           (Float
              (Get_Allocated_Width
                 (Gtk_Widget(Get_Object(Builder, "mainwindow")))) *
            0.3));
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
   end CreateMainWindow;

end MainWindow;
