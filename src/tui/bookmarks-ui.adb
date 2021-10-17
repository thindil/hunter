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
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData.UI; use LoadData.UI;
with Messages.UI; use Messages.UI;
with Modules; use Modules;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;

package body Bookmarks.UI is

   procedure Create_Bookmarks_List is
   begin
      Fill_Bookmarks_List;
   end Create_Bookmarks_List;

   function Show_Bookmarks_Menu return Item_Array_Access is
      Menu_Items: Item_Array_Access;
      MenuIndex: Positive := 1;
   begin
      Menu_Items := new Item_Array(1 .. Positive(Bookmarks_List.Length) + 4);
      Menu_Items.all(MenuIndex) := New_Item(Mc(Interpreter, "{Home}"));
      MenuIndex := MenuIndex + 1;
      for I in Bookmarks_List.Iterate loop
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
        (Create_Fields.all(2), 0, To_String(Common.Current_Directory));
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
      if Bookmarks_List.Contains(Bookmark) then
         New_Action := CREATEFILE;
         CreateProgramMenu(True);
         Common.Current_Directory :=
           To_Unbounded_String(Bookmarks_List(Bookmark));
      elsif Bookmark = Mc(Interpreter, "{Home}") then
         New_Action := CREATEFILE;
         CreateProgramMenu(True);
         Common.Current_Directory := To_Unbounded_String(Value("HOME"));
      else
         ShowBookmarksForm;
         return BOOKMARKS_FORM;
      end if;
      Load_Directory(To_String(Common.Current_Directory));
      UILocation := DIRECTORY_VIEW;
      Clear_Preview_Window;
      Update_Directory_List(True);
      ShowPreview;
      UpdateWatch(To_String(Common.Current_Directory));
      Execute_Modules
        (Interpreter, ON_ENTER,
         "{" & To_String(Common.Current_Directory) & "}");
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
                  Show_Message
                    (Mc(Interpreter, "{Directory}") & " " &
                     Trim(Get_Buffer(Fields(DialogForm, 2)), Both) &
                     " doesn't exists.");
                  return MESSAGE_FORM;
               end if;
               New_Action := CREATEFILE;
               Common.Current_Directory :=
                 To_Unbounded_String
                   (Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
               Load_Directory(To_String(Common.Current_Directory));
               UpdateWatch(To_String(Common.Current_Directory));
               Execute_Modules
                 (Interpreter, ON_ENTER,
                  "{" & To_String(Common.Current_Directory) & "}");
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

   procedure Add_Bookmark is
      File: File_Type;
   begin
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         Open(File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      else
         Create
           (File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Put_Line(File, "file://" & Current_Selected);
      Close(File);
      Create_Bookmarks_List;
   end Add_Bookmark;

   procedure Remove_Bookmark is
      NewFile, OldFile: File_Type;
      Line, Path: Unbounded_String;
      Added: Boolean := False;
   begin
      Rename
        (Value("HOME") & "/.config/gtk-3.0/bookmarks",
         Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Open(OldFile, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Create(NewFile, Out_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      Update_Bookmarks_Loop :
      while not End_Of_File(OldFile) loop
         Line := Get_Line(OldFile);
         if Length(Line) > 7 and then Slice(Line, 1, 7) = "file://" then
            Path := Unbounded_Slice(Line, 8, Length(Line));
            if Path /= Current_Selected then
               Put_Line(NewFile, Line);
               Added := True;
            end if;
         end if;
      end loop Update_Bookmarks_Loop;
      Close(NewFile);
      Close(OldFile);
      Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      if not Added then
         Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Create_Bookmarks_List;
   end Remove_Bookmark;

end Bookmarks.UI;
