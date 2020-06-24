-- /home/thindil/Projekty/hunter/hunter/src/trash.adb
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

--with GNAT.String_Split; use GNAT.String_Split;
--with Messages; use Messages;
--with ShowItems; use ShowItems;
--with Toolbars; use Toolbars;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with RefreshData; use RefreshData;
with Utils; use Utils;

package body Trash is

   -- ****if* Trash/ClearTrash
   -- FUNCTION
   -- Show message to start clearing the trash.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was clicked. Unused.
   -- SOURCE
--   procedure ClearTrash(Self: access Gtk_Menu_Item_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--   begin
--      NewAction := CLEARTRASH;
--      ToggleToolButtons(NewAction);
--      ShowMessage
--        (Gettext("Remove all files and directories from Trash?"),
--         Message_Question);
--   end ClearTrash;
--
--   -- ****if* Trash/RemovePathButtons
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
--   procedure PathClicked(Self: access Gtk_Button_Record'Class) is
--      Tokens: Slice_Set;
--      Index: constant Gint := Get_Index(Gtk_Flow_Box_Child(Get_Parent(Self)));
--   begin
--      if Index = 0 then
--         ShowTrash(null);
--         return;
--      end if;
--      Create(Tokens, To_String(CurrentDirectory), "/");
--      CurrentDirectory :=
--        To_Unbounded_String(Value("HOME") & "/.local/share/Trash/files");
--      for I in 2 .. (Index + 1) loop
--         Append(CurrentDirectory, "/" & Slice(Tokens, Slice_Number(I)));
--      end loop;
--      Reload;
--   end PathClicked;

   function Show_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   -- ****if* Trash/ShowTrash
   -- FUNCTION
   -- Show content of the Trash
   -- PARAMETERS
   -- Self - Gtk_Menu_Item which was clicked. Unused.
   -- SOURCE
   function Show_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      Directory, SubDirectory: Dir_Type;
      Last, SubLast: Natural;
      FileName, SubFileName: String(1 .. 1024);
      FileInfo: File_Type;
      Size: File_Size;
      FileLine, FullName, MimeType: Unbounded_String;
      Item: Item_Record;
   begin
      TemporaryStop := True;
      NewAction := SHOWTRASH;
      ToggleToolButtons(SHOWTRASH);
      ItemsList.Clear;
      Open(Directory, Value("HOME") & "/.local/share/Trash/files");
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) = "." or FileName(1 .. Last) = ".." then
            goto End_Of_Loop;
         end if;
         FullName :=
           To_Unbounded_String
             (Value("HOME") & "/.local/share/Trash/files/" &
              FileName(1 .. Last));
         Item.Path := FullName;
         Open
           (FileInfo, In_File,
            Value("HOME") & "/.local/share/Trash/info/" & FileName(1 .. Last) &
            ".trashinfo");
         Skip_Line(FileInfo);
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Item.Name :=
                 To_Unbounded_String
                   (Simple_Name(Slice(FileLine, 6, Length(FileLine))));
            else
               FileLine := Unbounded_Slice(FileLine, 14, Length(FileLine));
               Replace_Slice(FileLine, 11, 11, " ");
               Item.Modified := Value(To_String(FileLine));
            end if;
         end loop;
         Close(FileInfo);
         if Is_Directory(To_String(FullName)) then
            Item.IsDirectory := True;
            if FileName(1) = '.' then
               Item.IsHidden := True;
            else
               Item.IsHidden := False;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Image := To_Unbounded_String("emblem-symbolic-link");
            else
               Item.Image := To_Unbounded_String("folder");
            end if;
            if Is_Read_Accessible_File(To_String(FullName)) then
               Open(SubDirectory, To_String(FullName));
               Size := 0;
               loop
                  Read(SubDirectory, SubFileName, SubLast);
                  exit when SubLast = 0;
                  Size := Size + 1;
               end loop;
               Close(SubDirectory);
               Item.Size := Integer(Size - 2);
            else
               Item.Size := -1;
            end if;
         else
            Item.IsDirectory := False;
            if FileName(1) = '.' then
               Item.IsHidden := True;
            else
               Item.IsHidden := False;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Image := To_Unbounded_String("emblem-symbolic-link");
            elsif Is_Executable_File(To_String(FullName)) then
               Item.Image := To_Unbounded_String("application-x-executable");
            else
               MimeType :=
                 To_Unbounded_String(GetMimeType(To_String(FullName)));
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
            if not Is_Read_Accessible_File(To_String(FullName)) then
               Item.Size := -1;
               ItemsList.Append(Item);
               goto End_Of_Loop;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Size := -2;
            elsif Is_Regular_File(To_String(FullName)) then
               Item.Size := Integer(Ada.Directories.Size(To_String(FullName)));
            else
               Item.Size := 0;
            end if;
         end if;
         ItemsList.Append(Item);
         <<End_Of_Loop>>
      end loop;
      Close(Directory);
      UpdateDirectoryList(True);
--      Foreach(ButtonBox, RemovePathButtons'Access);
--      Gtk_New(Button, "Trash");
--      Insert(ButtonBox, Button, -1);
--      On_Clicked(Button, PathClicked'Access);
--      Set_Tooltip_Text
--        (Gtk_Widget(Button), Gettext("Reload current directory [ALT+R]"));
--      Set_Accel_Path(Gtk_Widget(Button), "<mainwindow>/reload", Accelerators);
--      Show_All(ButtonBox);
--      Set_Sort_Func(FilesSort, 0, SortFiles'Access);
--      Set_Sort_Column_Id(FilesSort, 0, Sort_Ascending);
--      if MainWindow.Window /= null then
--         Set_Cursor(Get_Window(MainWindow.Window), Gdk_Cursor_New(Arrow));
--         Set_Sensitive(MainWindow.Window, True);
--      end if;
--      Setting := False;
--      Refilter
--        (-(Gtk.Tree_Model_Sort.Get_Model
--            (-(Gtk.Tree_View.Get_Model(DirectoryView)))));
--      if N_Children(Get_Model(DirectoryView), Null_Iter) = 0 then
--         CurrentSelected :=
--           To_Unbounded_String(Value("HOME") & "/.local/share/Trash/files/");
--      else
--         Set_Cursor
--           (DirectoryView, Gtk_Tree_Path_New_From_String("0"), null, False);
--         Grab_Focus(DirectoryView);
--      end if;
--      ShowItem(Get_Selection(DirectoryView));
      return TCL_OK;
   end Show_Trash_Command;

--   procedure RestoreItem(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--      RestoreInfo, FileLine, Destination, ItemType: Unbounded_String;
--      StartIndex: Positive;
--      FileInfo: File_Type;
--   begin
--      for Item of SelectedItems loop
--         StartIndex := Index(Item, "files");
--         RestoreInfo :=
--           Unbounded_Slice(Item, 1, StartIndex - 1) & "info" &
--           Unbounded_Slice(Item, StartIndex + 5, Length(Item)) &
--           To_Unbounded_String(".trashinfo");
--         Open(FileInfo, In_File, To_String(RestoreInfo));
--         Skip_Line(FileInfo);
--         for I in 1 .. 2 loop
--            FileLine := To_Unbounded_String(Get_Line(FileInfo));
--            if Slice(FileLine, 1, 4) = "Path" then
--               Destination := Unbounded_Slice(FileLine, 6, Length(FileLine));
--               if Ada.Directories.Exists(To_String(Destination)) then
--                  if Is_Directory(To_String(Destination)) then
--                     ItemType := To_Unbounded_String(Gettext("Directory"));
--                  else
--                     ItemType := To_Unbounded_String(Gettext("File"));
--                  end if;
--                  ShowMessage
--                    (Gettext("Can't restore ") & To_String(Destination) & " " &
--                     To_String(ItemType) & " " &
--                     Gettext("with that name exists."));
--                  Close(FileInfo);
--                  ShowTrash(null);
--                  return;
--               end if;
--               Rename(To_String(Item), Slice(FileLine, 6, Length(FileLine)));
--            end if;
--         end loop;
--         Close(FileInfo);
--         Delete_File(To_String(RestoreInfo));
--      end loop;
--      ShowTrash(null);
--   end RestoreItem;

   procedure CreateTrashUI is
   begin
      AddCommand("ShowTrash", Show_Trash_Command'Access);
   end CreateTrashUI;

end Trash;
