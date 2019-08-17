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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Flow_Box; use Gtk.Flow_Box;
with Gtk.Flow_Box_Child; use Gtk.Flow_Box_Child;
with Gtk.Image; use Gtk.Image;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Gdk; use Gdk;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
with MainWindow; use MainWindow;
with Utils; use Utils;
with Trash;

package body LoadData is

   -- ****iv* LoadData/Accelerators
   -- FUNCTION
   -- Keyboard shortcuts for path buttons
   -- SOURCE
   Accelerators: Gtk_Accel_Group;
   -- ****

   function SortFiles
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
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

   function EmptySortFiles
     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
      pragma Unreferenced(Model);
      pragma Unreferenced(A);
      pragma Unreferenced(B);
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
      Tokens: Slice_Set;
      Index: constant Gint := Get_Index(Gtk_Flow_Box_Child(Get_Parent(Self)));
   begin
      if Index > 0 then
         Create(Tokens, To_String(CurrentDirectory), "/");
         CurrentDirectory := Null_Unbounded_String;
         for I in 2 .. (Index + 1) loop
            Append(CurrentDirectory, "/" & Slice(Tokens, Slice_Number(I)));
         end loop;
      else
         CurrentDirectory := To_Unbounded_String("/");
      end if;
      if Get_Parent(Get_Parent(Self)) =
        Gtk_Widget(Get_Object(Builder, "boxpath")) then
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
      MimeType: Unbounded_String;
   begin
      Setting := True;
      if Accelerators = null then
         Accelerators := Gtk_Accel_Group(Get_Object(Builder, "accelerators"));
      end if;
      if MainWindow /= null then
         Set_Cursor(MainWindow, Gdk_Cursor_New(Watch));
         if not Is_Visible
             (Gtk_Widget(Get_Object(Builder, "poppreferences"))) then
            Set_Sensitive
              (Gtk_Widget(Get_Object(Builder, "mainwindow")), False);
         end if;
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
            Gettext("You don't have permissions to preview this directory."));
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
         if ListName = "fileslist" then
            begin
               Set
                 (FilesList, FileIter, 5,
                  Ada.Calendar.Formatting.Image
                    (Date =>
                       Modification_Time(Name & "/" & FileName(1 .. Last)),
                     Time_Zone => UTC_Time_Offset));
            exception
               when others =>
                  Set(FilesList, FileIter, 5, "unknown");
            end;
            Set(FilesList, FileIter, 6, Name & "/" & FileName(1 .. Last));
         end if;
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
            elsif Is_Executable_File(Name & "/" & FileName(1 .. Last)) then
               Set(FilesList, FileIter, 2, "application-x-executable");
            else
               MimeType :=
                 To_Unbounded_String
                   (GetMimeType(Name & "/" & FileName(1 .. Last)));
               if Index(MimeType, "audio") > 0 then
                  Set(FilesList, FileIter, 2, "audio-x-generic");
               elsif Index(MimeType, "font") > 0 then
                  Set(FilesList, FileIter, 2, "font-x-generic");
               elsif Index(MimeType, "image") > 0 then
                  Set(FilesList, FileIter, 2, "image-x-generic");
               elsif Index(MimeType, "video") > 0 then
                  Set(FilesList, FileIter, 2, "video-x-generic");
               elsif Index(MimeType, "text/x-script") > 0 then
                  Set(FilesList, FileIter, 2, "text-x-script");
               elsif MimeType = To_Unbounded_String("text/html") then
                  Set(FilesList, FileIter, 2, "text-html");
               elsif Index(MimeType, "zip") > 0 or
                 Index(MimeType, "x-xz") > 0 then
                  Set(FilesList, FileIter, 2, "package-x-generic");
               elsif Index(MimeType, "text") > 0 then
                  Set(FilesList, FileIter, 2, "text-x-generic");
               else
                  Set(FilesList, FileIter, 2, "text-x-generic-template");
               end if;
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
            ButtonBox: Gtk_Flow_Box;
            FileInfo: File_Type;
            TmpPath, FileLine: Unbounded_String;
         begin
            if ListName = "fileslist" then
               FilesSort :=
                 Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort"));
               ButtonBox := Gtk_Flow_Box(Get_Object(Builder, "boxpath"));
            else
               FilesSort :=
                 Gtk_Tree_Model_Sort(Get_Object(Builder, "filessort2"));
               ButtonBox := Gtk_Flow_Box(Get_Object(Builder, "boxpath2"));
            end if;
            Foreach(ButtonBox, RemovePathButtons'Access);
            CurrentDirectory :=
              To_Unbounded_String
                (Normalize_Pathname(To_String(CurrentDirectory)));
            if not Is_Visible
                (Gtk_Widget(Get_Object(Builder, "btntoolrestore"))) then
               Gtk_New(Button, "/");
               Set_Image
                 (Button,
                  Gtk_Widget
                    (Gtk_Image_New_From_Stock
                       ("gtk-harddisk", Icon_Size_Button)));
               Insert(ButtonBox, Button, -1);
               On_Clicked(Button, PathClicked'Access);
               Create(Tokens, To_String(CurrentDirectory), "/");
            else
               Gtk_New(Button, "Trash");
               Insert(ButtonBox, Button, -1);
               On_Clicked(Button, Trash.PathClicked'Access);
               Create
                 (Tokens,
                  Slice
                    (CurrentDirectory,
                     Ada.Environment_Variables.Value("HOME")'Length + 26,
                     Length(CurrentDirectory)),
                  "/");
               Open
                 (FileInfo, In_File,
                  Ada.Environment_Variables.Value("HOME") &
                  "/.local/share/Trash/info/" & Slice(Tokens, 2) &
                  ".trashinfo");
               Skip_Line(FileInfo);
               for I in 1 .. 2 loop
                  FileLine := To_Unbounded_String(Get_Line(FileInfo));
                  if Slice(FileLine, 1, 4) = "Path" then
                     TmpPath :=
                       To_Unbounded_String
                         ("/" &
                          Simple_Name(Slice(FileLine, 6, Length(FileLine))));
                     for I in 3 .. Slice_Count(Tokens) loop
                        Append(TmpPath, "/" & Slice(Tokens, I));
                     end loop;
                  end if;
                  Create(Tokens, To_String(TmpPath), "/");
               end loop;
               Close(FileInfo);
            end if;
            if CurrentDirectory = To_Unbounded_String("/") then
               if ListName = "fileslist" then
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Reload current directory [ALT+R]"));
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/reload", Accelerators);
               else
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Reload current directory [ALT+SHIFT+R]"));
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/reload2", Accelerators);
               end if;
            elsif Slice_Count(Tokens) = 2 then
               if ListName = "fileslist" then
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Go to upper directory [ALT+U]"));
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/goup", Accelerators);
               else
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Go to upper directory [ALT+SHIFT+U]"));
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/goup2", Accelerators);
               end if;
            else
               if ListName = "fileslist" then
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/path1", Accelerators);
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Go to this directory [ALT+1]"));
               else
                  Set_Accel_Path
                    (Gtk_Widget(Button), "<mainwindow>/path12", Accelerators);
                  Set_Tooltip_Text
                    (Gtk_Widget(Button),
                     Gettext("Go to this directory [ALT+SHIFT+1]"));
               end if;
            end if;
            for I in 2 .. Slice_Count(Tokens) loop
               if Slice(Tokens, I) /= "" then
                  Gtk_New(Button, Slice(Tokens, I));
                  if Slice(Tokens, I) =
                    Ada.Environment_Variables.Value("USER") then
                     Set_Image
                       (Button,
                        Gtk_Widget
                          (Gtk_Image_New_From_Stock
                             ("gtk-home", Icon_Size_Button)));
                  end if;
                  Insert(ButtonBox, Button, -1);
                  if not Is_Visible
                      (Gtk_Widget(Get_Object(Builder, "btntoolrestore"))) then
                     On_Clicked(Button, PathClicked'Access);
                  else
                     On_Clicked(Button, Trash.PathClicked'Access);
                  end if;
                  if I = Slice_Count(Tokens) - 1 then
                     if ListName = "fileslist" then
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           Gettext("Go to upper directory [ALT+U]"));
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/goup",
                           Accelerators);
                     else
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           Gettext("Go to upper directory [ALT+SHIFT+U]"));
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/goup2",
                           Accelerators);
                     end if;
                  elsif I = Slice_Count(Tokens) then
                     if ListName = "fileslist" then
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           Gettext("Reload current directory [ALT+R]"));
                        Set_Accel_Path
                          (Gtk_Widget(Button), "<mainwindow>/reload",
                           Accelerators);
                     else
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           Gettext("Reload current directory [ALT+SHIFT+R]"));
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
                           Gettext("Go to this directory [ALT+") &
                           Slice_Number'Image(I)(2) & "]");
                     else
                        Set_Accel_Path
                          (Gtk_Widget(Button),
                           "<mainwindow>/path" & Slice_Number'Image(I)(2) &
                           "2",
                           Accelerators);
                        Set_Tooltip_Text
                          (Gtk_Widget(Button),
                           Gettext("Go to this directory [ALT+SHIFT+") &
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
      if ListName = "fileslist" then
         Set_Title
           (Gtk_Window(Get_Object(Builder, "mainwindow")),
            "Hunter - " & To_String(CurrentDirectory));
      end if;
      Setting := False;
      if ListName = "fileslist" then
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter")));
      elsif ListName = "fileslist2" then
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter2")));
      else
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Builder, "filesfilter1")));
      end if;
   end LoadDirectory;

end LoadData;
