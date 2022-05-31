-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body SearchItems is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure ShowSearchForm is
      Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result := Unknown_Request;
   begin
      Set_Cursor_Visibility(Visibility);
      Create_Fields.all(1) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Search for:}", Interpreter)), 0,
           8, 0, 0);
      Set_Buffer(Create_Fields.all(1), 0, Mc(Interpreter, "{Search for:}"));
      FieldOptions := Get_Options(Create_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Create_Fields.all(1), FieldOptions);
      Create_Fields.all(2) := New_Field(1, 40, 1, 0, 0, 0);
      Set_Buffer(Create_Fields.all(2), 0, "");
      FieldOptions := Get_Options(Create_Fields.all(2));
      FieldOptions.Auto_Skip := False;
      Set_Options(Create_Fields.all(2), FieldOptions);
      Create_Fields.all(3) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("Cancel", Interpreter)) + 2, 2, 7,
           0, 0);
      Set_Buffer
        (Create_Fields.all(3), 0, "[" & Mc(Interpreter, "Cancel") & "]");
      FieldOptions := Get_Options(Create_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(3), FieldOptions);
      Create_Fields.all(4) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("Search", Interpreter)) + 2, 2, 23,
           0, 0);
      FieldOptions := Get_Options(Create_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(4), FieldOptions);
      Set_Buffer
        (Create_Fields.all(4), 0, "[" & Mc(Interpreter, "Search") & "]");
      Create_Fields.all(5) := Null_Field;
      DialogForm := New_Form(Create_Fields);
      Set_Current(DialogForm, Create_Fields(2));
      Create_Dialog(DialogForm, FormWindow, FormHeight, FormLength);
   end ShowSearchForm;

   function Search_Form_Keys(Key: Key_Code) return Ui_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_UP =>
            Result := Go_Previous_Field(DialogForm);
         when KEY_DOWN =>
            Result := Go_Next_Field(DialogForm);
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
         when 27 =>
            Set_Cursor_Visibility(Visibility);
            Ui_Location := DIRECTORY_VIEW;
            Post(DialogForm, False);
            Delete(DialogForm);
            Update_Directory_List(True);
            Show_Preview;
            return DIRECTORY_VIEW;
         when 10 =>
            if FieldIndex = 2 then
               Result := Go_Previous_Field(DialogForm);
               return Search_Form_Keys(10);
            end if;
            if FieldIndex /= 2 then
               Set_Cursor_Visibility(Visibility);
               Ui_Location := DIRECTORY_VIEW;
               if FieldIndex = 4 then
                  Update_Directory_List
                    (True, Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
               else
                  Update_Directory_List(True);
               end if;
               Post(DialogForm, False);
               Delete(DialogForm);
               Show_Preview;
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
      return SEARCH_FORM;
   end Search_Form_Keys;

end SearchItems;
