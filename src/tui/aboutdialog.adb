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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;

package body AboutDialog is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure Show_About_Dialog is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 9);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
   begin
      Set_Cursor_Visibility(Visibility);
      About_Fields.all(1) := New_Field(1, 36, 0, 1, 0, 0);
      Set_Buffer
        (About_Fields.all(1), 0, "Hunter - Text file manager for Linux");
      FieldOptions := Get_Options(About_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(About_Fields.all(1), FieldOptions);
      About_Fields.all(2) := New_Field(1, 26, 1, 4, 0, 0);
      Set_Buffer(About_Fields.all(2), 0, "© Bartek Jasicki 2019-2021");
      Set_Options(About_Fields.all(2), FieldOptions);
      About_Fields.all(3) := New_Field(1, 18, 2, 8, 0, 0);
      Set_Buffer(About_Fields.all(3), 0, "License: GNU GPLv3");
      Set_Options(About_Fields.all(3), FieldOptions);
      About_Fields.all(4) := New_Field(1, 26, 3, 4, 0, 0);
      Set_Buffer(About_Fields.all(4), 0, "Version: 1.6 (development)");
      Set_Options(About_Fields.all(4), FieldOptions);
      About_Fields.all(5) := New_Field(1, 9, 5, 0, 0, 0);
      Set_Buffer(About_Fields.all(5), 0, "[Website]");
      FieldOptions := Get_Options(About_Fields.all(5));
      FieldOptions.Edit := False;
      Set_Options(About_Fields.all(5), FieldOptions);
      About_Fields.all(6) := New_Field(1, 13, 5, 9, 0, 0);
      Set_Buffer(About_Fields.all(6), 0, "[Programmers]");
      Set_Options(About_Fields.all(6), FieldOptions);
      About_Fields.all(7) := New_Field(1, 13, 5, 22, 0, 0);
      Set_Buffer(About_Fields.all(7), 0, "[Translators]");
      Set_Options(About_Fields.all(7), FieldOptions);
      About_Fields.all(8) := New_Field(1, 7, 5, 35, 0, 0);
      Set_Buffer(About_Fields.all(8), 0, "[Close]");
      Set_Options(About_Fields.all(8), FieldOptions);
      About_Fields.all(9) := Null_Field;
      DialogForm := New_Form(About_Fields);
      Set_Current(DialogForm, About_Fields(5));
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
      Refresh;
      Refresh(FormWindow);
   end Show_About_Dialog;

   procedure Show_Developers_Dialog(Developers: Boolean := True) is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 3);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      FieldOptions: Field_Option_Set;
   begin
      About_Fields.all(1) := New_Field(1, 36, 0, 1, 0, 0);
      if Developers then
         Set_Buffer
           (About_Fields.all(1), 0, "Bartek Jasicki <thindil@laeran.pl>");
      end if;
      FieldOptions := Get_Options(About_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(About_Fields.all(1), FieldOptions);
      About_Fields.all(2) := New_Field(1, 7, 2, 10, 0, 0);
      FieldOptions := Get_Options(About_Fields.all(2));
      FieldOptions.Edit := False;
      Set_Buffer(About_Fields.all(2), 0, "[Close]");
      Set_Options(About_Fields.all(2), FieldOptions);
      About_Fields.all(3) := Null_Field;
      DialogForm := New_Form(About_Fields);
      Set_Current(DialogForm, About_Fields(2));
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
      Refresh;
      Refresh(FormWindow);
   end Show_Developers_Dialog;

   function About_View_Keys(Key: Key_Code) return UI_Locations is
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
            case FieldIndex is
               when 5 =>
                  declare
                     ProcessId: Process_Id;
                  begin
                     ProcessId :=
                       Non_Blocking_Spawn
                         (Locate_Exec_On_Path("xdg-open").all,
                          Argument_String_To_List
                            ("https://www.laeran.pl/repositories/hunter/").all);
                     if ProcessId = Invalid_Pid then
                        return ABOUT_FORM;
                     end if;
                  end;
               when 6 | 7 =>
                  Post(DialogForm, False);
                  Delete(DialogForm);
                  UILocation := DIRECTORY_VIEW;
                  Update_Directory_List;
                  if FieldIndex = 6 then
                     Show_Developers_Dialog;
                  else
                     Show_Developers_Dialog(False);
                  end if;
                  return DEVELOPERS_VIEW;
               when 8 =>
                  Set_Cursor_Visibility(Visibility);
                  Post(DialogForm, False);
                  Delete(DialogForm);
                  UILocation := DIRECTORY_VIEW;
                  Update_Directory_List;
                  return DIRECTORY_VIEW;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return ABOUT_FORM;
   end About_View_Keys;

   function Developers_Keys(Key: Key_Code) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
   begin
      if Key = 10 then
         Set_Cursor_Visibility(Visibility);
         Post(DialogForm, False);
         Delete(DialogForm);
         UILocation := DIRECTORY_VIEW;
         Update_Directory_List;
         return DIRECTORY_VIEW;
      end if;
      return DEVELOPERS_VIEW;
   end Developers_Keys;

end AboutDialog;
