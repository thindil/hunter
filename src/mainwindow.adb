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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Expect; use GNAT.Expect;
with Gtk.Main; use Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Paned; use Gtk.Paned;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Window; use Gtk.Window;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Glib.Values; use Glib.Values;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gdk.Event;
with Gdk.Types; use Gdk.Types;

package body MainWindow is

   Builder: Gtkada_Builder;
   CurrentDirectory, CurrentSelected: Unbounded_String;
   Setting: Boolean;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
   end Quit;

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
      Set_Sort_Column_Id(FilesList, 0, Sort_Ascending);
      if ListName = "fileslist" then
         declare
            FileStack: constant Gtk_Stack :=
              Gtk_Stack(Get_Object(Builder, "filestack"));
            Value: GValue;
         begin
            Init_Set_String(Value, To_String(CurrentDirectory));
            Child_Set_Property
              (FileStack, Get_Visible_Child(FileStack), "title", Value);
            Set_Markup
              (Gtk_Label(Get_Object(Builder, "lblpath")),
               "<span font_weight=""bold"">" & To_String(CurrentDirectory) &
               "</span>");
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

   function GetMimeType(FileName: String) return String is
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
   begin
      Non_Blocking_Spawn
        (ProcessDesc, "file",
         Argument_String_To_List("-b --mime-type " & FileName).all);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      case Result is
         when 1 =>
            declare
               MimeType: constant String := Expect_Out(ProcessDesc);
            begin
               Close(ProcessDesc);
               return MimeType;
            end;
         when others =>
            null;
      end case;
      Close(ProcessDesc);
      return "";
   end GetMimeType;

   function CanBeOpened(MimeType: String) return Boolean is
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
   begin
      Non_Blocking_Spawn
        (ProcessDesc, Containing_Directory(Command_Name) & "/xdg-mime",
         Argument_String_To_List("query default " & MimeType).all);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      Close(ProcessDesc);
      return True;
   exception
      when Process_Died =>
         return False;
   end CanBeOpened;

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
      Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnopen")), True);
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
         declare
            MimeType: constant String :=
              GetMimeType
                (To_String(CurrentDirectory) & "/" &
                 To_String(CurrentSelected));
            Buffer: constant Gtk_Text_Buffer :=
              Get_Buffer(Gtk_Text_View(Get_Object(Builder, "filetextview")));
            Iter: Gtk_Text_Iter;
            File: File_Type;
         begin
            Set_Text(Buffer, "");
            Get_Start_Iter(Buffer, Iter);
            if MimeType(1 .. 4) = "text" then
               Open
                 (File, In_File,
                  To_String(CurrentDirectory) & "/" &
                  To_String(CurrentSelected));
               while not End_Of_File(File) loop
                  Insert(Buffer, Iter, Get_Line(File) & LF);
               end loop;
               Close(File);
            else
               Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
               if not CanBeOpened(MimeType) and
                 not Is_Executable_File
                   (To_String(CurrentDirectory) & "/" &
                    To_String(CurrentSelected)) then
                  Set_Sensitive
                    (Gtk_Widget(Get_Object(Builder, "btnopen")), False);
               end if;
            end if;
         end;
      end if;
   end ShowFileInfo;

   procedure ActivateFile(Object: access Gtkada_Builder_Record'Class) is
      FilesIter: Gtk_Tree_Iter;
      FilesModel: Gtk_Tree_Model;
      NewDirectory: Unbounded_String;
      procedure ShowMessage(Message: String) is
         MessageDialog: constant Gtk_Message_Dialog :=
           Gtk_Message_Dialog_New
             (Gtk_Window(Get_Object(Object, "mainwindow")), Modal,
              Message_Error, Buttons_Close, Message);
      begin
         if Run(MessageDialog) /= Gtk_Response_None then
            Destroy(MessageDialog);
         end if;
      end ShowMessage;
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
            ShowMessage("You can't enter this directory.");
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
      else
         declare
            MimeType: constant String :=
              GetMimeType
                (To_String(CurrentDirectory) & "/" &
                 To_String(CurrentSelected));
            Pid: GNAT.OS_Lib.Process_Id;
            Openable: constant Boolean := CanBeOpened(MimeType);
         begin
            if not Openable and
              not Is_Executable_File
                (To_String(CurrentDirectory) & "/" &
                 To_String(CurrentSelected)) then
               ShowMessage("I can't open this file.");
               return;
            elsif Openable then
               Pid :=
                 Non_Blocking_Spawn
                   (Containing_Directory(Command_Name) & "/xdg-open",
                    Argument_String_To_List
                      (To_String(CurrentDirectory) & "/" &
                       To_String(CurrentSelected)).all);
            else
               Pid :=
                 Non_Blocking_Spawn
                   (To_String(CurrentDirectory) & "/" &
                    To_String(CurrentSelected),
                    Argument_String_To_List("").all);
            end if;
            if Pid = GNAT.Os_Lib.Invalid_Pid then
               ShowMessage("I can't open this file.");
            end if;
         end;
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

   function KeyPressed(Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced(Self);
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      Key: Gtk_Accel_Key;
      Found: Boolean;
   begin
      Lookup_Entry("<mainwindow>/BtnQuit", Key, Found);
      if not Found then
         return False;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         Quit(Builder);
      end if;
      Lookup_Entry("<mainwindow>/BtnGoUp", Key, Found);
      if not Found then
         return False;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         GoUpDirectory(Builder);
      end if;
      Lookup_Entry("<mainwindow>/BtnOpen", Key, Found);
      if not Found then
         return False;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         ActivateFile(Builder);
      end if;
      Lookup_Entry("<mainwindow>/BtnReload", Key, Found);
      if not Found then
         return False;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         Reload(Builder);
      end if;
      return False;
   end KeyPressed;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Main_Quit", Quit'Access);
      Register_Handler(Builder, "Show_File_Info", ShowFileInfo'Access);
      Register_Handler(Builder, "Activate_File", ActivateFile'Access);
      Register_Handler(Builder, "Go_Up_Directory", GoUpDirectory'Access);
      Register_Handler(Builder, "Reload", Reload'Access);
      Do_Connect(Builder);
      Add_Entry("<mainwindow>/BtnQuit", GDK_LC_q, 4);
      Add_Entry("<mainwindow>/BtnGoUp", GDK_LC_u, 8);
      Add_Entry("<mainwindow>/BtnOpen", GDK_LC_o, 8);
      Add_Entry("<mainwindow>/BtnReload", GDK_LC_r, 8);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "mainwindow")), KeyPressed'Access);
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
