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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with LoadData; use LoadData;
with Messages; use Messages;
with MoveItems; use MoveItems;
with SearchItems; use SearchItems;
with Utils; use Utils;

package body MainWindow is

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
   end Quit;

   -- ****if* MainWindow/GetSelectedItems
   -- FUNCTION
   -- Add selected file or directory to SelectedItems list.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content of currently selected directory
   -- Path  - Gtk_Tree_Path to selected element in Model
   -- Iter  - Gtk_Tree_Iter to selected element in Model
   -- SOURCE
   procedure GetSelectedItems(Model: Gtk_Tree_Model; Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) is
      pragma Unreferenced(Path);
      -- ****
   begin
      if CurrentDirectory = To_Unbounded_String("/") then
         CurrentDirectory := Null_Unbounded_String;
      end if;
      SelectedItems.Append
        (CurrentDirectory &
         To_Unbounded_String("/" & Get_String(Model, Iter, 0)));
   end GetSelectedItems;

   -- ****if* MainWindow/ShowFileInfo
   -- FUNCTION
   -- Show info about selected item in preview view.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ShowFileInfo(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      if Setting then
         return;
      end if;
      SelectedItems.Clear;
      Selected_Foreach
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         GetSelectedItems'Access);
      if SelectedItems.Length /= 1 then
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
         return;
      end if;
      if CurrentSelected = SelectedItems(1) then
         return;
      end if;
      CurrentSelected := SelectedItems(1);
      Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnopen")), True);
      if Is_Directory(To_String(CurrentSelected)) then
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btnopen")), True);
         Show_All(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
         LoadDirectory(To_String(CurrentSelected), "fileslist1");
      else
         Show_All(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
            Buffer: constant Gtk_Text_Buffer :=
              Get_Buffer(Gtk_Text_View(Get_Object(Builder, "filetextview")));
            Iter: Gtk_Text_Iter;
            File: File_Type;
         begin
            Set_Text(Buffer, "");
            Get_Start_Iter(Buffer, Iter);
            if MimeType(1 .. 4) = "text" then
               Open(File, In_File, To_String(CurrentSelected));
               while not End_Of_File(File) loop
                  Insert(Buffer, Iter, Get_Line(File) & LF);
               end loop;
               Close(File);
            else
               Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
               if not CanBeOpened(MimeType) and
                 not Is_Executable_File(To_String(CurrentSelected)) then
                  Set_Sensitive
                    (Gtk_Widget(Get_Object(Builder, "btnopen")), False);
               end if;
            end if;
         end;
      end if;
   end ShowFileInfo;

   -- ****if* MainWindow/ActivateFile
   -- FUNCTION
   -- "Activate" selected file or directory. Action depends on what selected
   -- item is. For example: it go to selected directory, opens text files in
   -- editor and so on.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure ActivateFile(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      if Is_Directory(To_String(CurrentSelected)) then
         if not Is_Read_Accessible_File(To_String(CurrentSelected)) then
            ShowMessage("You can't enter this directory.");
            return;
         end if;
         if CurrentDirectory = To_Unbounded_String("/") then
            CurrentDirectory := Null_Unbounded_String;
         end if;
         CurrentDirectory := CurrentSelected;
         LoadDirectory(To_String(CurrentDirectory), "fileslist");
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Object, "treefiles")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
         Grab_Focus(Gtk_Widget(Get_Object(Object, "treefiles")));
      else
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
            Pid: GNAT.OS_Lib.Process_Id;
            Openable: Boolean := CanBeOpened(MimeType);
         begin
            if MimeType(1 .. 4) = "text" and not Openable then
               Openable := CanBeOpened("text/plain");
            end if;
            if not Openable and
              not Is_Executable_File(To_String(CurrentSelected)) then
               ShowMessage("I can't open this file.");
               return;
            elsif Openable then
               Pid :=
                 Non_Blocking_Spawn
                   (Containing_Directory(Command_Name) & "/xdg-open",
                    Argument_String_To_List(To_String(CurrentSelected)).all);
            else
               Pid :=
                 Non_Blocking_Spawn
                   (To_String(CurrentSelected),
                    Argument_String_To_List("").all);
            end if;
            if Pid = GNAT.Os_Lib.Invalid_Pid then
               ShowMessage("I can't open this file.");
            end if;
         end;
      end if;
      Set_Sensitive(Gtk_Widget(Get_Object(Object, "btngoup")), True);
   end ActivateFile;

   procedure Reload(Object: access Gtkada_Builder_Record'Class) is
   begin
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Object, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
      ShowFileInfo(Object);
   end Reload;

   -- ****if* MainWindow/GoUpDirectory
   -- FUNCTION
   -- Go to upper directory in path.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure GoUpDirectory(Object: access Gtkada_Builder_Record'Class) is
   -- ****
   begin
      CurrentDirectory :=
        Unbounded_Slice
          (CurrentDirectory, 1, Index(CurrentDirectory, "/", Backward) - 1);
      if CurrentDirectory = Null_Unbounded_String then
         CurrentDirectory := To_Unbounded_String("/");
      end if;
      if CurrentDirectory = To_Unbounded_String("/") then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoup")), False);
      end if;
      Reload(Object);
   end GoUpDirectory;

   -- ****if* MainWindow/DeleteItem
   -- FUNCTION
   -- Show message to start deleting selected files and directories.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure DeleteItem(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      -- ****
      Message: Unbounded_String := To_Unbounded_String("Delete?" & LF);
   begin
      for I in SelectedItems.First_Index .. SelectedItems.Last_Index loop
         Append(Message, SelectedItems(I));
         if Is_Directory(To_String(SelectedItems(I))) then
            Append(Message, "(and its content)");
         end if;
         if I /= SelectedItems.Last_Index then
            Append(Message, LF);
         end if;
      end loop;
      NewAction := DELETE;
      ShowMessage(To_String(Message), MESSAGE_QUESTION);
   end DeleteItem;

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

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder; Directory: String) is
      XDGBookmarks: constant array(Positive range <>) of Bookmark_Record :=
        ((To_Unbounded_String("Desktop"),
          To_Unbounded_String("XDG_DESKTOP_DIR")),
         (To_Unbounded_String("Download"),
          To_Unbounded_String("XDG_DOWNLOAD_DIR")),
         (To_Unbounded_String("Public"),
          To_Unbounded_String("XDG_PUBLICSHARE_DIR")),
         (To_Unbounded_String("Documents"),
          To_Unbounded_String("XDG_DOCUMENTS_DIR")),
         (To_Unbounded_String("Music"), To_Unbounded_String("XDG_MUSIC_DIR")),
         (To_Unbounded_String("Pictures"),
          To_Unbounded_String("XDG_PICTURES_DIR")),
         (To_Unbounded_String("Videos"),
          To_Unbounded_String("XDG_VIDEOS_DIR")));
      BookmarksMenus: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("bookmarkhome"),
         To_Unbounded_String("bookmarkdesktop"),
         To_Unbounded_String("bookmarkdownload"),
         To_Unbounded_String("bookmarkpublic"),
         To_Unbounded_String("bookmarkdocuments"),
         To_Unbounded_String("bookmarkmusic"),
         To_Unbounded_String("bookmarkpictures"),
         To_Unbounded_String("bookmarkvideos"));
      function GetXDGDirectory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String;
         EqualIndex: Natural;
      begin
         if not Ada.Environment_Variables.Exists(Name)
           or else Value(Name) = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            while not End_Of_File(File) loop
               Line := To_Unbounded_String(Get_Line(File));
               EqualIndex := Index(Line, "=");
               if EqualIndex > 0 then
                  if Slice(Line, 1, EqualIndex - 1) = Name then
                     Set(Name, Slice(Line, EqualIndex + 2, Length(Line) - 1));
                     exit;
                  end if;
               end if;
            end loop;
            Close(File);
         end if;
         return To_Unbounded_String(Expand_Path(Value(Name)));
      end GetXDGDirectory;
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_File_Info", ShowFileInfo'Access);
      Register_Handler(Builder, "Activate_File", ActivateFile'Access);
      Register_Handler(Builder, "Go_Up_Directory", GoUpDirectory'Access);
      Register_Handler(Builder, "Reload", Reload'Access);
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
      Register_Handler(Builder, "Message_Yes", MessageYes'Access);
      Register_Handler(Builder, "Message_No", MessageNo'Access);
      Do_Connect(Builder);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")),
         VisibleFiles'Access);
      On_Icon_Press
        (Gtk_GEntry(Get_Object(Builder, "entry")), IconPressed'Access);
      On_Response
        (Gtk_Info_Bar(Get_Object(Builder, "actioninfo")),
         MessageResponse'Access);
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists(To_String(CurrentDirectory)) then
            CurrentDirectory := To_Unbounded_String("/");
         end if;
      end if;
      Set_Menu
        (Gtk_Menu_Tool_Button(Get_Object(Builder, "btnnew")),
         Gtk_Widget(Get_Object(Builder, "newmenu")));
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      -- Set bookmarks - XDG directories and GTK bookmarks
      Set_Menu
        (Gtk_Menu_Tool_Button(Get_Object(Builder, "btnbookmarks")),
         Gtk_Widget(Get_Object(Builder, "bookmarksmenu")));
      BookmarksList.Append
        (New_Item =>
           (MenuName => To_Unbounded_String("Home"),
            Path => To_Unbounded_String(Value("HOME"))));
      for I in XDGBookmarks'Range loop
         BookmarksList.Append
           (New_Item =>
              (MenuName => XDGBookmarks(I).MenuName,
               Path => GetXDGDirectory(To_String(XDGBookmarks(I).Path))));
         On_Activate
           (Gtk_Menu_Item(Get_Object(Builder, To_String(BookmarksMenus(I)))),
            GoToBookmark'Access);
      end loop;
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         declare
            File: File_Type;
            Line, Path: Unbounded_String;
            BookmarkExist: Boolean;
            MenuItem: Gtk_Menu_Item;
         begin
            Open(File, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
            while not End_Of_File(File) loop
               Line := To_Unbounded_String(Get_Line(File));
               if Slice(Line, 1, 7) = "file://" then
                  Path := Unbounded_Slice(Line, 8, Length(Line));
                  BookmarkExist := False;
                  for I in BookmarksList.Iterate loop
                     if BookmarksList(I).Path = Path then
                        BookmarkExist := True;
                        exit;
                     end if;
                  end loop;
                  if not BookmarkExist then
                     BookmarksList.Append
                       (New_Item =>
                          (MenuName =>
                             To_Unbounded_String(Simple_Name(To_String(Path))),
                           Path => Path));
                     MenuItem :=
                       Gtk_Menu_Item_New_With_Label
                         (Simple_Name(To_String(Path)));
                     On_Activate(MenuItem, GoToBookmark'Access);
                     Append
                       (Gtk_Menu_Shell(Get_Object(Builder, "bookmarksmenu")),
                        MenuItem);
                     Show_All(Gtk_Widget(MenuItem));
                  end if;
               end if;
            end loop;
            Close(File);
         end;
      end if;
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
      HideMessage(Builder);
      Hide(Gtk_Widget(Get_Object(Builder, "searchfile")));
      Hide(Gtk_Widget(Get_Object(Builder, "entry")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treefiles")),
         Gtk_Tree_Path_New_From_String("0"), null, False);
      Set_Position
        (Gtk_Paned(Get_Object(Builder, "paned1")),
         Gint
           (Float
              (Get_Allocated_Width
                 (Gtk_Widget(Get_Object(Builder, "mainwindow")))) *
            0.3));
      Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
   end CreateMainWindow;

end MainWindow;
