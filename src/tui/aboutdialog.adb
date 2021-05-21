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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;

package body AboutDialog is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure Show_About_Dialog is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 7);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
   begin
      Set_Cursor_Visibility(Visibility);
      About_Fields.all(1) := New_Field(1, 36, 0, 1, 0, 0);
      Set_Buffer(About_Fields.all(1), 0, "Hunter - Text file manager for Linux");
      FieldOptions := Get_Options(About_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(About_Fields.all(1), FieldOptions);
      About_Fields.all(2) := New_Field(1, 30, 1, 8, 0, 0);
      About_Fields.all(3) := New_Field(1, 30, 2, 8, 0, 0);
      About_Fields.all(4) := New_Field(1, 30, 3, 8, 0, 0);
      About_Fields.all(5) := New_Field(1, 30, 4, 8, 0, 0);
      About_Fields.all(6) := New_Field(1, 7, 5, 3, 0, 0);
      Set_Buffer(About_Fields.all(6), 0, "[Close]");
      FieldOptions := Get_Options(About_Fields.all(6));
      FieldOptions.Edit := False;
      Set_Options(About_Fields.all(6), FieldOptions);
      About_Fields.all(7) := Null_Field;
      DialogForm := New_Form(About_Fields);
      Set_Current(DialogForm, About_Fields(6));
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

   function About_View_Keys(Key: Key_Code) return UI_Locations is
      pragma Unreferenced(Key);
   begin
      return ABOUT_FORM;
   end About_View_Keys;

end AboutDialog;
