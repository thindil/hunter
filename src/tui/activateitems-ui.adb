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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with Messages.UI; use Messages.UI;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body ActivateItems.UI is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure Show_Execute_With_Dialog is
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
          (1,
           Column_Position'Value
             (Mc_Max("{Enter the application to execute:}", Interpreter)),
           0, 4, 0, 0);
      Set_Buffer
        (Create_Fields.all(1), 0,
         Mc(Interpreter, "{Enter the application to execute:}"));
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
          (1, Column_Position'Value(Mc_Max("{Cancel}", Interpreter)) + 2, 2, 7,
           0, 0);
      Set_Buffer
        (Create_Fields.all(3), 0, "[" & Mc(Interpreter, "{Cancel}") & "]");
      FieldOptions := Get_Options(Create_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(3), FieldOptions);
      Create_Fields.all(4) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Execute}", Interpreter)) + 2, 2,
           23, 0, 0);
      FieldOptions := Get_Options(Create_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(4), FieldOptions);
      Set_Buffer
        (Create_Fields.all(4), 0, "[" & Mc(Interpreter, "{Execute}") & "]");
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
   end Show_Execute_With_Dialog;

   function Execute_Form_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Value: constant String := Trim(Get_Buffer(Fields(DialogForm, 2)), Both);
      CommandName: Unbounded_String;
      Pid: GNAT.OS_Lib.Process_Id;
      SpaceIndex: Natural range 0 .. Value'Length;
      Arguments: Argument_List_Access;
      function Hide_Dialog
        (With_Message: Boolean := False) return UI_Locations is
         Visibility: Cursor_Visibility := Invisible;
      begin
         Post(DialogForm, False);
         Delete(DialogForm);
         if With_Message then
            UILocation := MESSAGE_FORM;
         else
            Set_Cursor_Visibility(Visibility);
            UILocation := DIRECTORY_VIEW;
            Update_Directory_List(True);
            Show_Preview;
         end if;
         return UILocation;
      end Hide_Dialog;
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
               SpaceIndex := Index(Value, " ");
               CommandName :=
                 (if SpaceIndex > 0 then
                    To_Unbounded_String(Value(1 .. SpaceIndex - 1))
                  else To_Unbounded_String(Value));
               CommandName :=
                 To_Unbounded_String(Find_Executable(To_String(CommandName)));
               if CommandName = Null_Unbounded_String then
                  Show_Message
                    (Mc(Interpreter, "{Can't find command:}") & " " &
                     (if SpaceIndex > 0 then Value(1 .. SpaceIndex - 1)
                      else Value));
                  return Hide_Dialog(True);
               end if;
               Arguments :=
                 (if SpaceIndex > 0 then
                    Argument_String_To_List
                      (Value(SpaceIndex .. Value'Length) & " @2")
                  else Argument_String_To_List("@2"));
               Replace_Substitutes_Loop :
               for I in Arguments'Range loop
                  if Arguments(I).all = "@2" then
                     Arguments(I) := new String'(To_String(Current_Selected));
                  end if;
               end loop Replace_Substitutes_Loop;
               Pid :=
                 Non_Blocking_Spawn
                   (Full_Name(To_String(CommandName)), Arguments.all);
               if Pid = GNAT.OS_Lib.Invalid_Pid then
                  Show_Message
                    (Mc(Interpreter, "{Can't execute this command}"));
                  return Hide_Dialog(True);
               end if;
            end if;
            if FieldIndex /= 2 then
               return Hide_Dialog;
            end if;
         when others =>
            if Key /= 91 then
               Result := Driver(DialogForm, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return EXECUTE_FORM;
   end Execute_Form_Keys;

end ActivateItems.UI;
