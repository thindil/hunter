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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body DeleteItems.UI is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure ShowDeleteForm is
      Delete_Fields: constant Field_Array_Access := new Field_Array(1 .. 4);
      FormHeight: Line_Position;
      FormLength: constant Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      DeleteList: Unbounded_String;
      ListLength: Positive;
      UnusedResult: Forms.Driver_Result;
      File_Line: Unbounded_String := Null_Unbounded_String;
      File_Info: File_Type;
   begin
      if Selected_Items.Length = 0 then
         return;
      end if;
      Set_Cursor_Visibility(Visibility);
      if Settings.Delete_Files or New_Action = DELETETRASH then
         DeleteList := To_Unbounded_String(Mc(Interpreter, "{Delete?}") & LF);
      else
         DeleteList :=
           To_Unbounded_String(Mc(Interpreter, "{Move to Trash?}") & LF);
      end if;
      if Selected_Items.Length > 10 then
         ListLength := 10;
      else
         ListLength := Positive(Selected_Items.Length);
      end if;
      Set_Delete_List_Loop :
      for I in 1 .. ListLength loop
         if New_Action = DELETE then
            if Is_Directory
                (To_String
                   (Common.Current_Directory & "/" & Selected_Items(I))) then
               if Simple_Name(Name => To_String(Source => Selected_Items(I)))'
                   Length >
                 11 then
                  Append
                    (DeleteList,
                     "  " &
                     Simple_Name
                       (Name => To_String(Source => Selected_Items(I)))
                       (1 .. 11) &
                     "..." &
                     Mc(Interp => Interpreter,
                        Src_String => "{(and its content)}") &
                     LF);
               else
                  Append
                    (DeleteList,
                     "  " &
                     Simple_Name
                       (Name => To_String(Source => Selected_Items(I))) &
                     " " &
                     Mc(Interp => Interpreter,
                        Src_String => "{(and its content)}") &
                     LF);
               end if;
            elsif Simple_Name(Name => To_String(Source => Selected_Items(I)))'
                Length >
              27 then
               Append
                 (DeleteList,
                  "  " &
                  Simple_Name(Name => To_String(Source => Selected_Items(I)))
                    (1 .. 27) &
                  "..." & LF);
            else
               Append
                 (DeleteList,
                  "  " &
                  Simple_Name(Name => To_String(Source => Selected_Items(I))) &
                  LF);
            end if;
         else
            Open
              (File_Info, In_File,
               Ada.Environment_Variables.Value("HOME") &
               "/.local/share/Trash/info/" &
               Simple_Name(To_String(Selected_Items(I))) & ".trashinfo");
            Skip_Line(File_Info);
            Get_Item_Name_Loop :
            for J in 1 .. 2 loop
               File_Line := To_Unbounded_String(Get_Line(File_Info));
               if Slice(File_Line, 1, 4) = "Path" then
                  Append
                    (DeleteList,
                     "  " &
                     Simple_Name(Slice(File_Line, 6, Length(File_Line))));
               end if;
            end loop Get_Item_Name_Loop;
            Close(File_Info);
         end if;
      end loop Set_Delete_List_Loop;
      if ListLength = 10 and Selected_Items.Length > 10 then
         ListLength := 11;
         Append
           (DeleteList,
            " " & Mc(Interp => Interpreter, Src_String => "{(and more)}"));
      end if;
      ListLength := ListLength + 4;
      Delete_Fields.all(1) :=
        New_Field(Line_Position(ListLength), 30, 0, 0, 0, 0);
      Set_Buffer(Delete_Fields.all(1), 0, To_String(DeleteList));
      FieldOptions := Get_Options(Delete_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Delete_Fields.all(1), FieldOptions);
      Delete_Fields.all(2) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Cancel}", Interpreter)) + 2,
           1 + Line_Position(ListLength), 7, 0, 0);
      Set_Buffer
        (Delete_Fields.all(2), 0, "[" & Mc(Interpreter, "{Cancel}") & "]");
      FieldOptions := Get_Options(Delete_Fields.all(2));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(2), FieldOptions);
      Delete_Fields.all(3) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Delete}", Interpreter)) + 2,
           1 + Line_Position(ListLength), 23, 0, 0);
      FieldOptions := Get_Options(Delete_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(3), FieldOptions);
      Set_Buffer
        (Delete_Fields.all(3), 0, "[" & Mc(Interpreter, "{Delete}") & "]");
      Delete_Fields.all(4) := Null_Field;
      FormHeight := Line_Position(ListLength) + 2;
      DialogForm := New_Form(Delete_Fields);
      Set_Options(DialogForm, (others => False));
      FormWindow :=
        Create
          (FormHeight + 2, 34, ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      UnusedResult := Driver(DialogForm, F_First_Field);
      Box(FormWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(FormWindow);
   end ShowDeleteForm;

   function Delete_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(DialogForm, F_Previous_Field);
         when KEY_DOWN =>
            Result := Driver(DialogForm, F_Next_Field);
         when 10 =>
            if FieldIndex = 3 then
               if not Delete_Selected(Interpreter) then
                  Load_Directory(To_String(Common.Current_Directory));
               else
                  Load_Directory
                    (Ada.Directories.Containing_Directory
                       (To_String(Common.Current_Directory)));
               end if;
               Current_Selected := Common.Current_Directory;
               if Items_List.Length > 0 then
                  Current_Selected :=
                    Current_Selected & "/" & Items_List(1).Name;
               end if;
            end if;
            Show_Preview;
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            UILocation := DIRECTORY_VIEW;
            Update_Directory_List(True);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return DELETE_FORM;
   end Delete_Keys;

end DeleteItems.UI;
