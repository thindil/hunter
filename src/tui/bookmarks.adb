-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Modules; use Modules;
with RefreshData; use RefreshData;

package body Bookmarks is

   procedure Create_Bookmarks_List is
      XDGBookmarks: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("XDG_DESKTOP_DIR"),
         To_Unbounded_String("XDG_DOWNLOAD_DIR"),
         To_Unbounded_String("XDG_PUBLICSHARE_DIR"),
         To_Unbounded_String("XDG_DOCUMENTS_DIR"),
         To_Unbounded_String("XDG_MUSIC_DIR"),
         To_Unbounded_String("XDG_PICTURES_DIR"),
         To_Unbounded_String("XDG_VIDEOS_DIR"));
      Path: Unbounded_String;
      function GetXDGDirectory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String;
         EqualIndex: Natural;
      begin
         if Value(Name, "") = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            Load_Bookmarks_Loop :
            while not End_Of_File(File) loop
               Line := Get_Line(File);
               EqualIndex := Index(Line, "=");
               if EqualIndex > 0 then
                  if Slice(Line, 1, EqualIndex - 1) = Name then
                     Set(Name, Slice(Line, EqualIndex + 2, Length(Line) - 1));
                     exit Load_Bookmarks_Loop;
                  end if;
               end if;
            end loop Load_Bookmarks_Loop;
            Close(File);
         end if;
         return To_Unbounded_String(Expand_Path(Value(Name)));
      end GetXDGDirectory;
   begin
      Set_XDGBookmarks_List_Loop :
      for I in XDGBookmarks'Range loop
         Path := GetXDGDirectory(To_String(XDGBookmarks(I)));
         if Ada.Directories.Exists(To_String(Path)) then
            BookmarksList.Include
              (Simple_Name(To_String(Path)), To_String(Path));
         end if;
      end loop Set_XDGBookmarks_List_Loop;
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         declare
            File: File_Type;
            Line, Path: Unbounded_String;
            BookmarkExist: Boolean;
         begin
            Open(File, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
            Load_User_Bookmarks_Loop :
            while not End_Of_File(File) loop
               Line := Get_Line(File);
               if Length(Line) < 7 or else Slice(Line, 1, 7) /= "file://" then
                  goto End_Of_Loop;
               end if;
               Path := Unbounded_Slice(Line, 8, Length(Line));
               BookmarkExist := False;
               Check_Bookmark_Existence_Loop :
               for I in BookmarksList.Iterate loop
                  if BookmarksList(I) = To_String(Path) then
                     BookmarkExist := True;
                     exit Check_Bookmark_Existence_Loop;
                  end if;
               end loop Check_Bookmark_Existence_Loop;
               if not BookmarkExist and
                 Ada.Directories.Exists(To_String(Path)) then
                  BookmarksList.Include
                    (Simple_Name(To_String(Path)), To_String(Path));
               end if;
               <<End_Of_Loop>>
            end loop Load_User_Bookmarks_Loop;
            Close(File);
         end;
      end if;
   end Create_Bookmarks_List;

   function Show_Bookmarks_Menu return Item_Array_Access is
      Menu_Items: Item_Array_Access;
      MenuIndex: Positive := 1;
   begin
      Menu_Items := new Item_Array(1 .. Positive(BookmarksList.Length) + 4);
      Menu_Items.all(MenuIndex) := New_Item(Mc(Interpreter, "{Home}"));
      MenuIndex := MenuIndex + 1;
      for I in BookmarksList.Iterate loop
         Menu_Items.all(MenuIndex) := New_Item(Bookmarks_Container.Key(I));
         MenuIndex := MenuIndex + 1;
      end loop;
      Menu_Items.all(MenuIndex) := New_Item("Enter destination");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := New_Item("Close");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := Null_Item;
      return Menu_Items;
   end Show_Bookmarks_Menu;

   DialogForm: Forms.Form;
   FormWindow: Window;

   -- ****if* BookmarksTUI/BookmarksTUI.ShowBookmarksForm
   -- FUNCTION
   -- Show dialog to enter the destination directory
   -- SOURCE
   procedure ShowBookmarksForm is
      -- ****
      Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result := Unknown_Request;
   begin
      Set_Cursor_Visibility(Visibility);
      Create_Fields.all(1) := New_Field(1, 30, 0, 8, 0, 0);
      Set_Buffer(Create_Fields.all(1), 0, "Enter the destination directory:");
      FieldOptions := Get_Options(Create_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Create_Fields.all(1), FieldOptions);
      Create_Fields.all(2) := New_Field(1, 40, 1, 0, 0, 0);
      Set_Buffer
        (Create_Fields.all(2), 0, To_String(MainWindow.Current_Directory));
      FieldOptions := Get_Options(Create_Fields.all(2));
      FieldOptions.Auto_Skip := False;
      Set_Options(Create_Fields.all(2), FieldOptions);
      Create_Fields.all(3) := New_Field(1, 8, 2, 7, 0, 0);
      Set_Buffer(Create_Fields.all(3), 0, "[Cancel]");
      FieldOptions := Get_Options(Create_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(3), FieldOptions);
      Create_Fields.all(4) := New_Field(1, 7, 2, 23, 0, 0);
      FieldOptions := Get_Options(Create_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(4), FieldOptions);
      Set_Buffer(Create_Fields.all(4), 0, "[Enter]");
      Create_Fields.all(5) := Null_Field;
      DialogForm := New_Form(Create_Fields);
      Set_Current(DialogForm, Create_Fields(2));
      Set_Options(DialogForm, (others => False));
      Scale(DialogForm, FormHeight, FormLength);
      FormWindow :=
        Create
          (FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Box(FormWindow, Default_Character, Default_Character);
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      UnusedResult := Driver(DialogForm, REQ_END_LINE);
      Refresh;
      Refresh(FormWindow);
   end ShowBookmarksForm;

   function Go_To_Bookmark(Bookmark: String) return UI_Locations is
   begin
      if Bookmark = "Close" then
         Update_Directory_List;
         return DIRECTORY_VIEW;
      end if;
      if BookmarksList.Contains(Bookmark) then
         MainWindow.Current_Directory :=
           To_Unbounded_String(BookmarksList(Bookmark));
      elsif Bookmark = Mc(Interpreter, "{Home}") then
         MainWindow.Current_Directory := To_Unbounded_String(Value("HOME"));
      else
         ShowBookmarksForm;
         return BOOKMARKS_FORM;
      end if;
      LoadDirectory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      Execute_Modules
        (On_Enter, "{" & To_String(MainWindow.Current_Directory) & "}");
      return DIRECTORY_VIEW;
   end Go_To_Bookmark;

   function Bookmarks_Form_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(DialogForm, F_Previous_Field);
            Result := Driver(DialogForm, F_End_Line);
         when KEY_DOWN =>
            Result := Driver(DialogForm, F_Next_Field);
            Result := Driver(DialogForm, F_End_Line);
         when KEY_LEFT =>
            if FieldIndex = 2 then
               Result := Driver(DialogForm, F_Previous_Char);
            end if;
         when KEY_RIGHT =>
            if FieldIndex = 2 then
               Result := Driver(DialogForm, F_Next_Char);
            end if;
         when 127 =>
            Result := Driver(DialogForm, F_Delete_Previous);
         when 10 =>
            if FieldIndex = 4 then
               if not Ada.Directories.Exists
                   (Trim(Get_Buffer(Fields(DialogForm, 2)), Both)) then
                  ShowMessage
                    (Mc(Interpreter, "{Directory}") & " " &
                     Trim(Get_Buffer(Fields(DialogForm, 2)), Both) &
                     " doesn't exists.");
                  return MESSAGE_FORM;
               end if;
               MainWindow.Current_Directory :=
                 To_Unbounded_String
                   (Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
               LoadDirectory(To_String(MainWindow.Current_Directory));
               UpdateWatch(To_String(MainWindow.Current_Directory));
               Execute_Modules
                 (On_Enter,
                  "{" & To_String(MainWindow.Current_Directory) & "}");
            end if;
            if FieldIndex /= 2 then
               Set_Cursor_Visibility(Visibility);
               Post(DialogForm, False);
               Delete(DialogForm);
               UILocation := DIRECTORY_VIEW;
               Update_Directory_List(True);
               return DIRECTORY_VIEW;
            end if;
         when others =>
            if Key /= 91 then
               Result := Driver(DialogForm, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return BOOKMARKS_FORM;
   end Bookmarks_Form_Keys;

end Bookmarks;
