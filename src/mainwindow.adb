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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
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
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with MainWindow.LoadData; use MainWindow.LoadData;
with Utils; use Utils;

package body MainWindow is

   CurrentSelected: Unbounded_String;
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   SelectedItems: UnboundedString_Container.Vector;
   type NewActions is (FILE, DIRECTORY);
   NewAction: NewActions;

   procedure Quit(Object: access Gtkada_Builder_Record'Class) is
   begin
      Unref(Object);
      Main_Quit;
   end Quit;

   procedure GetSelectedItems(Model: Gtk_Tree_Model; Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) is
      pragma Unreferenced(Path);
   begin
      SelectedItems.Append
        (CurrentDirectory &
         To_Unbounded_String("/" & Get_String(Model, Iter, 0)));
   end GetSelectedItems;

   procedure ShowFileInfo(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Setting then
         return;
      end if;
      SelectedItems.Clear;
      Selected_Foreach
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         GetSelectedItems'Access);
      if SelectedItems.Length = 0 then
         return;
      elsif SelectedItems.Length > 1 then
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

   procedure ActivateFile(Object: access Gtkada_Builder_Record'Class) is
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

   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class) is
      SearchEntry: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "searchfile"));
   begin
      if not Is_Visible(SearchEntry) then
         Show_All(SearchEntry);
         Grab_Focus(SearchEntry);
      else
         Hide(SearchEntry);
         Grab_Focus(Gtk_Widget(Get_Object(Builder, "treefiles")));
      end if;
   end ToggleSearch;

   function VisibleFiles(Model: Gtk_Tree_Model;
      Iter: Gtk_Tree_Iter) return Boolean is
      SearchEntry: constant Gtk_GEntry :=
        Gtk_GEntry(Get_Object(Builder, "searchfile"));
   begin
      if Setting then
         return True;
      end if;
      if Get_Text(SearchEntry) = "" then
         return True;
      end if;
      if Index
          (To_Lower(Get_String(Model, Iter, 0)),
           To_Lower(Get_Text(SearchEntry)), 1) >
        0 then
         return True;
      end if;
      return False;
   end VisibleFiles;

   procedure SearchFiles(Object: access Gtkada_Builder_Record'Class) is
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Object, "fileslist")), Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Object, "treefiles")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Object, "searchfile"))) then
         Grab_Focus(Gtk_Widget(Get_Object(Object, "searchfile")));
      end if;
   end SearchFiles;

   procedure AddNew(User_Data: access GObject_Record'Class) is
      GEntry: constant Gtk_Widget := Gtk_Widget(Get_Object(Builder, "entry"));
   begin
      if User_Data = Get_Object(Builder, "newmenudirectory") then
         NewAction := DIRECTORY;
         Set_Icon_Tooltip_Text
           (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary,
            "Create new directory.");
      else
         NewAction := FILE;
         Set_Icon_Tooltip_Text
           (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary, "Create new file.");
      end if;
      Show_All(GEntry);
      Grab_Focus(GEntry);
   end AddNew;

   procedure CreateItem(Self: access Gtk_Entry_Record'Class;
      Icon_Pos: Gtk_Entry_Icon_Position) is
      Name: constant String :=
        To_String(CurrentDirectory) & "/" & Get_Text(Self);
      File: File_Type;
   begin
      if Icon_Pos = Gtk_Entry_Icon_Primary then
         Set_Text(Self, "");
         Hide(Gtk_Widget(Self));
         return;
      end if;
      if Get_Text(Self) = "" then
         return;
      end if;
      if NewAction = DIRECTORY then
         Create_Path(Name);
      else
         Create_Path(Containing_Directory(Name));
         Create(File, Out_File, Name);
         Close(File);
      end if;
      Set_Text(Self, "");
      Hide(Gtk_Widget(Self));
      CurrentDirectory := To_Unbounded_String(Containing_Directory(Name));
      Reload(Builder);
   end CreateItem;

   procedure IconPressed(Self: access Gtk_Entry_Record'Class;
      Icon_Pos: Gtk_Entry_Icon_Position; Event: Gdk_Event_Button) is
      pragma Unreferenced(Event);
   begin
      CreateItem(Self, Icon_Pos);
   end IconPressed;

   procedure DeleteItem(Object: access Gtkada_Builder_Record'Class) is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Gtk_Window(Get_Object(Object, "mainwindow")), Modal, Message_Error,
           Buttons_Yes_No, "");
      Message: Unbounded_String := To_Unbounded_String("Delete?" & LF);
      Response: Gtk_Response_Type;
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
      Set_Markup(MessageDialog, To_String(Message));
      Response := Run(MessageDialog);
      if Response = GTK_RESPONSE_YES then
         for Item of SelectedItems loop
            if Is_Directory(To_String(Item)) then
               Delete_Tree(To_String(Item));
            else
               Delete_File(To_String(Item));
            end if;
         end loop;
         Reload(Object);
      end if;
      Destroy(MessageDialog);
   end DeleteItem;

   procedure CreateNew(Object: access Gtkada_Builder_Record'Class) is
   begin
      CreateItem
        (Gtk_GEntry(Get_Object(Object, "entry")), Gtk_Entry_Icon_Secondary);
   end CreateNew;

   procedure CreateMainWindow(NewBuilder: Gtkada_Builder; Directory: String) is
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
      Do_Connect(Builder);
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")),
         VisibleFiles'Access);
      On_Icon_Press
        (Gtk_GEntry(Get_Object(Builder, "entry")), IconPressed'Access);
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
      end if;
      Set_Menu
        (Gtk_Menu_Tool_Button(Get_Object(Builder, "btnnew")),
         Gtk_Widget(Get_Object(Builder, "newmenu")));
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      Show_All(Gtk_Widget(Get_Object(Builder, "mainwindow")));
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
