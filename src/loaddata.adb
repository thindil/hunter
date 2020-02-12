-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Utils; use Utils;
--with Ada.Calendar.Formatting;
--with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
--with Ada.Characters.Handling; use Ada.Characters.Handling;
--with Ada.Environment_Variables;
--with Ada.Text_IO; use Ada.Text_IO;
--with GNAT.String_Split; use GNAT.String_Split;
--with Gtk.Box; use Gtk.Box;
--with Gtk.Bin; use Gtk.Bin;
--with Gtk.Button; use Gtk.Button;
--with Gtk.Enums; use Gtk.Enums;
--with Gtk.Flow_Box; use Gtk.Flow_Box;
--with Gtk.Flow_Box_Child; use Gtk.Flow_Box_Child;
--with Gtk.Image; use Gtk.Image;
--with Gtk.Main; use Gtk.Main;
--with Gtk.Paned; use Gtk.Paned;
--with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
--with Gtk.Stack; use Gtk.Stack;
--with Gtk.Text_Buffer; use Gtk.Text_Buffer;
--with Gtk.Text_View; use Gtk.Text_View;
--with Gtk.Toolbar; use Gtk.Toolbar;
--with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
--with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
--with Gtk.Tree_View; use Gtk.Tree_View;
--with Gtk.Widget; use Gtk.Widget;
--with Gtk.Window; use Gtk.Window;
--with Gtkada.Intl; use Gtkada.Intl;
--with Gdk; use Gdk;
--with Gdk.Cursor; use Gdk.Cursor;
--with Gdk.Window; use Gdk.Window;
--with MainWindow; use MainWindow;
--with Preferences; use Preferences;
--with RefreshData; use RefreshData;
--with ShowItems; use ShowItems;
--with Toolbars; use Toolbars;
--with Trash;

package body LoadData is

   function "="(Left, Right: Item_Record) return Boolean is
   begin
      return Left.Name = Right.Name;
   end "=";

   function "<"(Left, Right: Item_Record) return Boolean is
   begin
      if Left.IsDirectory and not Right.IsDirectory then
         return True;
      end if;
      if not Left.IsDirectory and Right.IsDirectory then
         return False;
      end if;
      if Left.IsHidden and not Right.IsHidden then
         return True;
      end if;
      if not Left.IsHidden and Right.IsHidden then
         return False;
      end if;
      return Translate(Left.Name, Lower_Case_Map) <
        Translate(Right.Name, Lower_Case_Map);
   end "<";

   procedure AddItem(Path: String) is
      FileName: constant String := Simple_Name(Path);
      Size: File_Size;
      SubDirectory: Dir_Type;
      SubLast: Natural;
      SubFileName: String(1 .. 1024);
      MimeType: Unbounded_String;
      Item: Item_Record;
   begin
      Item.Name := To_Unbounded_String(FileName);
      begin
         Item.Modified := Modification_Time(Path);
      exception
         when others =>
            Item.Modified := Time_Of(1901, 1, 1);
      end;
      if FileName(1) = '.' then
         Item.IsHidden := True;
      else
         Item.IsHidden := False;
      end if;
      if Is_Directory(Path) then
         Item.IsDirectory := True;
         if Is_Symbolic_Link(Path) then
            Item.Image := To_Unbounded_String("emblem-symbolic-link");
         else
            Item.Image := To_Unbounded_String("folder");
         end if;
         if Is_Read_Accessible_File(Path) then
            Open(SubDirectory, Path);
            Size := 0;
            loop
               Read(SubDirectory, SubFileName, SubLast);
               exit when SubLast = 0;
               if SubFileName(1 .. SubLast) /= "." and
                 SubFileName(1 .. SubLast) /= ".." then
                  Size := Size + 1;
               end if;
            end loop;
            Close(SubDirectory);
            Item.Size := To_Unbounded_String(File_Size'Image(Size));
         else
            Item.Size := To_Unbounded_String("unknown");
         end if;
      else
         Item.IsDirectory := False;
         if Is_Symbolic_Link(Path) then
            Item.Image := To_Unbounded_String("emblem-symbolic-link");
         elsif Is_Executable_File(Path) then
            Item.Image := To_Unbounded_String("application-x-executable");
         else
            MimeType := To_Unbounded_String(GetMimeType(Path));
            if Index(MimeType, "audio") > 0 then
               Item.Image := To_Unbounded_String("audio-x-generic");
            elsif Index(MimeType, "font") > 0 then
               Item.Image := To_Unbounded_String("font-x-generic");
            elsif Index(MimeType, "image") > 0 then
               Item.Image := To_Unbounded_String("image-x-generic");
            elsif Index(MimeType, "video") > 0 then
               Item.Image := To_Unbounded_String("video-x-generic");
            elsif Index(MimeType, "text/x-script") > 0 then
               Item.Image := To_Unbounded_String("text-x-script");
            elsif MimeType = To_Unbounded_String("text/html") then
               Item.Image := To_Unbounded_String("text-html");
            elsif Index(MimeType, "zip") > 0 or
              Index(MimeType, "x-xz") > 0 then
               Item.Image := To_Unbounded_String("package-x-generic");
            elsif Index(MimeType, "text") > 0 then
               Item.Image := To_Unbounded_String("text-x-generic");
            else
               Item.Image := To_Unbounded_String("text-x-generic-template");
            end if;
         end if;
         if not Is_Read_Accessible_File(Path) then
            Item.Size := To_Unbounded_String("unknown");
            ItemsList.Insert(Item);
            return;
         end if;
         if Is_Symbolic_Link(Path) then
            Item.Size := To_Unbounded_String("->");
         elsif Is_Regular_File(Path) then
            Item.Size :=
              To_Unbounded_String(CountFileSize(Ada.Directories.Size(Path)));
         else
            Item.Size := To_Unbounded_String("0");
         end if;
      end if;
      ItemsList.Insert(Item);
   end AddItem;

   procedure LoadDirectory(DirectoryName: String) is
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
   begin
      ItemsList.Clear;
      if not Is_Read_Accessible_File(DirectoryName) then
         return;
      end if;
      Open(Directory, DirectoryName);
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) /= "." and FileName(1 .. Last) /= ".." then
            AddItem(DirectoryName & "/" & FileName(1 .. Last));
         end if;
      end loop;
      Close(Directory);
   end LoadDirectory;

--   function SortFiles
--     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
--      FileTypeA: constant Gint := Get_Int(Model, A, 1);
--      FileTypeB: constant Gint := Get_Int(Model, B, 1);
--      FileNameA: constant String := Get_String(Model, A, 0);
--      FileNameB: constant String := Get_String(Model, B, 0);
--   begin
--      if FileTypeA > FileTypeB then
--         return 1;
--      end if;
--      if FileTypeA < FileTypeB then
--         return -1;
--      end if;
--      if To_Lower(FileNameA) > To_Lower(FileNameB) then
--         return 1;
--      end if;
--      if To_Lower(FileNameA) < To_Lower(FileNameB) then
--         return -1;
--      end if;
--      return 0;
--   end SortFiles;
--
--   function EmptySortFiles
--     (Model: Gtk_Tree_Model; A: Gtk_Tree_Iter; B: Gtk_Tree_Iter) return Gint is
--      pragma Unreferenced(Model);
--      pragma Unreferenced(A);
--      pragma Unreferenced(B);
--   begin
--      return 0;
--   end EmptySortFiles;
--
--   -- ****if* LoadData/RemovePathButtons
--   -- FUNCTION
--   -- Remove selected button from path buttons
--   -- PARAMETERS
--   -- Widget - Button to remove
--   -- SOURCE
--   procedure RemovePathButtons
--     (Widget: not null access Gtk_Widget_Record'Class) is
--   -- ****
--   begin
--      Set_Accel_Path(Widget, "", Accelerators);
--      Destroy(Widget);
--   end RemovePathButtons;
--
--   -- ****if* LoadData/PathClicked
--   -- FUNCTION
--   -- Go to selected location and show it in current directory view.
--   -- PARAMETERS
--   -- Self - Button which was clicked by user
--   -- SOURCE
--   procedure PathClicked(Self: access Gtk_Button_Record'Class) is
--      -- ****
--      Tokens: Slice_Set;
--      Index: constant Gint := Get_Index(Gtk_Flow_Box_Child(Get_Parent(Self)));
--   begin
--      if Index > 0 then
--         Create(Tokens, To_String(CurrentDirectory), "/");
--         CurrentDirectory := Null_Unbounded_String;
--         for I in 2 .. (Index + 1) loop
--            Append(CurrentDirectory, "/" & Slice(Tokens, Slice_Number(I)));
--         end loop;
--      else
--         CurrentDirectory := To_Unbounded_String("/");
--      end if;
--      if Get_Parent(Get_Parent(Self)) =
--        Get_Child(Gtk_Box(Get_Child1(FilesPaned)), 0) then
--         Reload;
--         UpdateWatch(To_String(CurrentDirectory));
--      else
--         DestinationPath := CurrentDirectory;
--         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
--      end if;
--   end PathClicked;
--
--   procedure LoadDirectory(Name, ListName: String) is
--      FilesList: Gtk_List_Store;
--      FileIter: Gtk_Tree_Iter;
--      Directory: Dir_Type;
--      Last: Natural;
--      FileName: String(1 .. 1024);
--   begin
--      Setting := True;
--      if ListName = "fileslist2" then
--         FilesList :=
--           -(Gtk.Tree_Model_Filter.Get_Model
--              (-(Gtk.Tree_Model_Sort.Get_Model
--                  (-(Gtk.Tree_View.Get_Model
--                      (Gtk_Tree_View
--                         (Get_Child
--                            (Gtk_Scrolled_Window
--                               (Get_Child_By_Name
--                                  (InfoStack, "destination"))))))))));
--      elsif ListName = "fileslist" then
--         FilesList :=
--           -(Gtk.Tree_Model_Filter.Get_Model
--              (-(Gtk.Tree_Model_Sort.Get_Model
--                  (-(Gtk.Tree_View.Get_Model(DirectoryView))))));
--      else
--         FilesList :=
--           -(Gtk.Tree_Model_Filter.Get_Model
--              (-(Get_Model
--                  (Gtk_Tree_View
--                     (Get_Child
--                        (Gtk_Scrolled_Window
--                           (Get_Child_By_Name(InfoStack, "preview"))))))));
--      end if;
--      if MainWindow.Window /= null then
--         Set_Cursor
--           (Get_Window(Gtk_Widget(MainWindow.Window)), Gdk_Cursor_New(Watch));
--         if not Is_Visible(Gtk_Widget(PreferencesPopup)) then
--            Set_Sensitive(MainWindow.Window, False);
--         end if;
--         while Events_Pending loop
--            if Main_Iteration_Do(False) then
--               exit;
--            end if;
--         end loop;
--      end if;
--      FilesList.Clear;
--      if not Is_Read_Accessible_File(Name) then
--         declare
--            PreviewScroll: constant Gtk_Scrolled_Window :=
--              Gtk_Scrolled_Window(Get_Child_By_Name(InfoStack, "preview"));
--            TextView: constant Gtk_Text_View := Gtk_Text_View_New;
--            Buffer: constant Gtk_Text_Buffer := Get_Buffer(TextView);
--         begin
--            Foreach(PreviewScroll, RemoveChild'Access);
--            Set_Wrap_Mode(TextView, Wrap_Word);
--            Set_Editable(TextView, False);
--            Set_Cursor_Visible(TextView, False);
--            Set_Text
--              (Buffer,
--               Gettext
--                 ("You don't have permissions to preview this directory."));
--            Add(PreviewScroll, TextView);
--            Show_All(PreviewScroll);
--            if MainWindow.Window /= null then
--               Set_Cursor
--                 (Get_Window(MainWindow.Window), Gdk_Cursor_New(Arrow));
--               Set_Sensitive(Gtk_Widget(MainWindow.Window), True);
--               Set_Sensitive(Gtk_Widget(Get_Nth_Item(ItemToolBar, 1)), False);
--            end if;
--         end;
--         Setting := False;
--         return;
--      end if;
--      if ListName = "fileslist1" then
--         Set_Sort_Func(FilesList, 0, EmptySortFiles'Access);
--      else
--         Gtk.Tree_Model_Sort.Set_Sort_Func
--           (-(Gtk.Tree_View.Get_Model(DirectoryView)), 0,
--            EmptySortFiles'Access);
--      end if;
--      Open(Directory, Name);
--      loop
--         Read(Directory, FileName, Last);
--         exit when Last = 0;
--         if FileName(1 .. Last) /= "." and FileName(1 .. Last) /= ".." then
--            AddItem(FilesList, FileIter, Name & "/" & FileName(1 .. Last));
--         end if;
--      end loop;
--      Close(Directory);
--      if ListName /= "fileslist1" then
--         declare
--            FilesSort: Gtk_Tree_Model_Sort;
--            Tokens: Slice_Set;
--            Button: Gtk_Button;
--            ButtonBox: Gtk_Flow_Box;
--            FileInfo: File_Type;
--            TmpPath, FileLine: Unbounded_String;
--         begin
--            if ListName = "fileslist" then
--               FilesSort := -(Gtk.Tree_View.Get_Model(DirectoryView));
--               ButtonBox :=
--                 Gtk_Flow_Box(Get_Child(Gtk_Box(Get_Child1(FilesPaned)), 0));
--            else
--               FilesSort :=
--                 -(Gtk.Tree_View.Get_Model
--                    (Gtk_Tree_View
--                       (Get_Child
--                          (Gtk_Bin
--                             (Get_Child_By_Name(InfoStack, "destination"))))));
--               ButtonBox :=
--                 Gtk_Flow_Box(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
--            end if;
--            Foreach(ButtonBox, RemovePathButtons'Access);
--            CurrentDirectory :=
--              To_Unbounded_String
--                (Normalize_Pathname(To_String(CurrentDirectory)));
--            if not Is_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10))) then
--               Gtk_New(Button, "/");
--               Set_Image
--                 (Button,
--                  Gtk_Widget
--                    (Gtk_Image_New_From_Stock
--                       ("gtk-harddisk", Icon_Size_Button)));
--               Insert(ButtonBox, Button, -1);
--               On_Clicked(Button, PathClicked'Access);
--               Create(Tokens, To_String(CurrentDirectory), "/");
--            else
--               Gtk_New(Button, "Trash");
--               Insert(ButtonBox, Button, -1);
--               On_Clicked(Button, Trash.PathClicked'Access);
--               Create
--                 (Tokens,
--                  Slice
--                    (CurrentDirectory,
--                     Ada.Environment_Variables.Value("HOME")'Length + 26,
--                     Length(CurrentDirectory)),
--                  "/");
--               Open
--                 (FileInfo, In_File,
--                  Ada.Environment_Variables.Value("HOME") &
--                  "/.local/share/Trash/info/" & Slice(Tokens, 2) &
--                  ".trashinfo");
--               Skip_Line(FileInfo);
--               for I in 1 .. 2 loop
--                  FileLine := To_Unbounded_String(Get_Line(FileInfo));
--                  if Slice(FileLine, 1, 4) = "Path" then
--                     TmpPath :=
--                       To_Unbounded_String
--                         ("/" &
--                          Simple_Name(Slice(FileLine, 6, Length(FileLine))));
--                     for I in 3 .. Slice_Count(Tokens) loop
--                        Append(TmpPath, "/" & Slice(Tokens, I));
--                     end loop;
--                  end if;
--                  Create(Tokens, To_String(TmpPath), "/");
--               end loop;
--               Close(FileInfo);
--            end if;
--            if CurrentDirectory = To_Unbounded_String("/") then
--               if ListName = "fileslist" then
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Reload current directory [ALT+R]"));
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/reload", Accelerators);
--               else
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Reload current directory [ALT+SHIFT+R]"));
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/reload2", Accelerators);
--               end if;
--            elsif Slice_Count(Tokens) = 2 then
--               if ListName = "fileslist" then
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Go to upper directory [ALT+U]"));
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/goup", Accelerators);
--               else
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Go to upper directory [ALT+SHIFT+U]"));
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/goup2", Accelerators);
--               end if;
--            else
--               if ListName = "fileslist" then
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/path1", Accelerators);
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Go to this directory [ALT+1]"));
--               else
--                  Set_Accel_Path
--                    (Gtk_Widget(Button), "<mainwindow>/path12", Accelerators);
--                  Set_Tooltip_Text
--                    (Gtk_Widget(Button),
--                     Gettext("Go to this directory [ALT+SHIFT+1]"));
--               end if;
--            end if;
--            for I in 2 .. Slice_Count(Tokens) loop
--               if Slice(Tokens, I) /= "" then
--                  Gtk_New(Button, Slice(Tokens, I));
--                  if Slice(Tokens, I) =
--                    Ada.Environment_Variables.Value("USER") then
--                     Set_Image
--                       (Button,
--                        Gtk_Widget
--                          (Gtk_Image_New_From_Stock
--                             ("gtk-home", Icon_Size_Button)));
--                  end if;
--                  Insert(ButtonBox, Button, -1);
--                  if not Is_Visible
--                      (Gtk_Widget(Get_Nth_Item(ActionToolBar, 10))) then
--                     On_Clicked(Button, PathClicked'Access);
--                  else
--                     On_Clicked(Button, Trash.PathClicked'Access);
--                  end if;
--                  if I = Slice_Count(Tokens) - 1 then
--                     if ListName = "fileslist" then
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Go to upper directory [ALT+U]"));
--                        Set_Accel_Path
--                          (Gtk_Widget(Button), "<mainwindow>/goup",
--                           Accelerators);
--                     else
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Go to upper directory [ALT+SHIFT+U]"));
--                        Set_Accel_Path
--                          (Gtk_Widget(Button), "<mainwindow>/goup2",
--                           Accelerators);
--                     end if;
--                  elsif I = Slice_Count(Tokens) then
--                     if ListName = "fileslist" then
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Reload current directory [ALT+R]"));
--                        Set_Accel_Path
--                          (Gtk_Widget(Button), "<mainwindow>/reload",
--                           Accelerators);
--                     else
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Reload current directory [ALT+SHIFT+R]"));
--                        Set_Accel_Path
--                          (Gtk_Widget(Button), "<mainwindow>/reload2",
--                           Accelerators);
--                     end if;
--                  elsif I < 11 then
--                     if ListName = "fileslist" then
--                        Set_Accel_Path
--                          (Gtk_Widget(Button),
--                           "<mainwindow>/path" & Slice_Number'Image(I)(2),
--                           Accelerators);
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Go to this directory [ALT+") &
--                           Slice_Number'Image(I)(2) & "]");
--                     else
--                        Set_Accel_Path
--                          (Gtk_Widget(Button),
--                           "<mainwindow>/path" & Slice_Number'Image(I)(2) &
--                           "2",
--                           Accelerators);
--                        Set_Tooltip_Text
--                          (Gtk_Widget(Button),
--                           Gettext("Go to this directory [ALT+SHIFT+") &
--                           Slice_Number'Image(I)(2) & "]");
--                     end if;
--                  end if;
--               end if;
--            end loop;
--            Show_All(ButtonBox);
--            Set_Sort_Func(FilesSort, 0, SortFiles'Access);
--            Set_Sort_Column_Id(FilesSort, 0, Sort_Ascending);
--         end;
--      else
--         Set_Sort_Func(FilesList, 0, SortFiles'Access);
--         Set_Sort_Column_Id(FilesList, 0, Sort_Ascending);
--      end if;
--      if MainWindow.Window /= null then
--         Set_Cursor(Get_Window(MainWindow.Window), Gdk_Cursor_New(Arrow));
--         Set_Sensitive(MainWindow.Window, True);
--      end if;
--      if ListName = "fileslist" then
--         Set_Title
--           (MainWindow.Window, "Hunter - " & To_String(CurrentDirectory));
--      end if;
--      Setting := False;
--      if ListName = "fileslist" then
--         Refilter
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model(DirectoryView)))));
--      elsif ListName = "fileslist2" then
--         Refilter
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model
--                   (Gtk_Tree_View
--                      (Get_Child
--                         (Gtk_Scrolled_Window
--                            (Get_Child_By_Name
--                               (InfoStack, "destination")))))))));
--      elsif NewAction in COPY | MOVE | CREATELINK then
--         Refilter
--           (-(Get_Model
--               (Gtk_Tree_View
--                  (Get_Child
--                     (Gtk_Bin(Get_Child_By_Name(InfoStack, "preview")))))));
--      end if;
--   end LoadDirectory;
--
--   procedure AddItem
--     (FilesList: Gtk_List_Store; FileIter: out Gtk_Tree_Iter; Path: String) is
--      FileName: constant String := Simple_Name(Path);
--      Size: File_Size;
--      SubDirectory: Dir_Type;
--      SubLast: Natural;
--      SubFileName: String(1 .. 1024);
--      MimeType: Unbounded_String;
--   begin
--      Append(FilesList, FileIter);
--      Set(FilesList, FileIter, 0, Simple_Name(Path));
--      if Get_N_Columns(FilesList) = 7 then
--         begin
--            Set
--              (FilesList, FileIter, 5,
--               Ada.Calendar.Formatting.Image
--                 (Date => Modification_Time(Path),
--                  Time_Zone => UTC_Time_Offset));
--         exception
--            when others =>
--               Set(FilesList, FileIter, 5, "unknown");
--         end;
--         Set(FilesList, FileIter, 6, Path);
--      end if;
--      if Is_Directory(Path) then
--         if FileName(1) = '.' then
--            Set(FilesList, FileIter, 1, 1);
--         else
--            Set(FilesList, FileIter, 1, 2);
--         end if;
--         if Is_Symbolic_Link(Path) then
--            Set(FilesList, FileIter, 2, "emblem-symbolic-link");
--         else
--            Set(FilesList, FileIter, 2, "folder");
--         end if;
--         if Get_N_Columns(FilesList) = 3 then
--            return;
--         end if;
--         Set(FilesList, FileIter, 4, Gint'Last);
--         if Is_Read_Accessible_File(Path) then
--            Open(SubDirectory, Path);
--            Size := 0;
--            loop
--               Read(SubDirectory, SubFileName, SubLast);
--               exit when SubLast = 0;
--               if SubFileName(1 .. SubLast) /= "." and
--                 SubFileName(1 .. SubLast) /= ".." then
--                  Size := Size + 1;
--               end if;
--            end loop;
--            Close(SubDirectory);
--            Set(FilesList, FileIter, 3, File_Size'Image(Size));
--         else
--            Set(FilesList, FileIter, 3, "?");
--         end if;
--      else
--         if FileName(1) = '.' then
--            Set(FilesList, FileIter, 1, 3);
--         else
--            Set(FilesList, FileIter, 1, 4);
--         end if;
--         if Is_Symbolic_Link(Path) then
--            Set(FilesList, FileIter, 2, "emblem-symbolic-link");
--         elsif Is_Executable_File(Path) then
--            Set(FilesList, FileIter, 2, "application-x-executable");
--         else
--            MimeType := To_Unbounded_String(GetMimeType(Path));
--            if Index(MimeType, "audio") > 0 then
--               Set(FilesList, FileIter, 2, "audio-x-generic");
--            elsif Index(MimeType, "font") > 0 then
--               Set(FilesList, FileIter, 2, "font-x-generic");
--            elsif Index(MimeType, "image") > 0 then
--               Set(FilesList, FileIter, 2, "image-x-generic");
--            elsif Index(MimeType, "video") > 0 then
--               Set(FilesList, FileIter, 2, "video-x-generic");
--            elsif Index(MimeType, "text/x-script") > 0 then
--               Set(FilesList, FileIter, 2, "text-x-script");
--            elsif MimeType = To_Unbounded_String("text/html") then
--               Set(FilesList, FileIter, 2, "text-html");
--            elsif Index(MimeType, "zip") > 0 or
--              Index(MimeType, "x-xz") > 0 then
--               Set(FilesList, FileIter, 2, "package-x-generic");
--            elsif Index(MimeType, "text") > 0 then
--               Set(FilesList, FileIter, 2, "text-x-generic");
--            else
--               Set(FilesList, FileIter, 2, "text-x-generic-template");
--            end if;
--         end if;
--         if Get_N_Columns(FilesList) = 3 then
--            return;
--         end if;
--         if not Is_Read_Accessible_File(Path) then
--            Set(FilesList, FileIter, 3, "?");
--            Set(FilesList, FileIter, 4, 0);
--            return;
--         end if;
--         if Is_Symbolic_Link(Path) then
--            Set(FilesList, FileIter, 3, "->");
--            Set(FilesList, FileIter, 4, 0);
--         elsif Is_Regular_File(Path) then
--            Size := Ada.Directories.Size(Path);
--            Set(FilesList, FileIter, 3, CountFileSize(Size));
--            if Size > File_Size(Gint'Last) then
--               Size := File_Size(Gint'Last);
--            end if;
--            Set(FilesList, FileIter, 4, Gint(Size));
--         else
--            Set(FilesList, FileIter, 3, "0");
--            Set(FilesList, FileIter, 4, 0);
--         end if;
--      end if;
--   end AddItem;

end LoadData;
