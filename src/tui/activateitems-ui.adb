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
with Utils.UI; use Utils.UI;

package body ActivateItems.UI is

   Dialog_Form: Forms.Form;
   Form_Window: Window;

   procedure Show_Execute_With_Dialog is
      Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      Form_Height: Line_Position;
      Form_Length: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      Field_Options: Field_Option_Set;
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
      Field_Options := Get_Options(Create_Fields.all(1));
      Field_Options.Active := False; --## rule line off ASSIGNMENTS
      Set_Options(Create_Fields.all(1), Field_Options);
      Create_Fields.all(2) := New_Field(1, 40, 1, 0, 0, 0);
      Set_Buffer(Create_Fields.all(2), 0, "");
      Field_Options := Get_Options(Create_Fields.all(2));
      Field_Options.Auto_Skip := False; --## rule line off ASSIGNMENTS
      Set_Options(Create_Fields.all(2), Field_Options);
      Create_Fields.all(3) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Cancel}", Interpreter)) + 2, 2, 7,
           0, 0);
      Set_Buffer
        (Create_Fields.all(3), 0, "[" & Mc(Interpreter, "{Cancel}") & "]");
      Field_Options := Get_Options(Create_Fields.all(3));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Create_Fields.all(3), Field_Options);
      Create_Fields.all(4) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Execute}", Interpreter)) + 2, 2,
           23, 0, 0);
      Field_Options := Get_Options(Create_Fields.all(4));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Create_Fields.all(4), Field_Options);
      Set_Buffer
        (Create_Fields.all(4), 0, "[" & Mc(Interpreter, "{Execute}") & "]");
      Create_Fields.all(5) := Null_Field;
      Dialog_Form := New_Form(Create_Fields);
      Set_Current(Dialog_Form, Create_Fields(2));
      Set_Options(Dialog_Form, (others => False));
      Create_Dialog(Dialog_Form, Form_Window, Form_Height, Form_Length);
   end Show_Execute_With_Dialog;

   function Execute_Form_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(Dialog_Form));
      Value: constant String := Trim(Get_Buffer(Fields(Dialog_Form, 2)), Both);
      CommandName: Unbounded_String;
      Pid: GNAT.OS_Lib.Process_Id;
      SpaceIndex: Natural range 0 .. Value'Length;
      Arguments: Argument_List_Access;
      function Hide_Dialog
        (With_Message: Boolean := False) return UI_Locations is
         Visibility: Cursor_Visibility := Invisible;
      begin
         Post(Dialog_Form, False);
         Delete(Dialog_Form);
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
            Result := Go_Previous_Field(Dialog_Form);
         when KEY_DOWN =>
            Result := Go_Next_Field(Dialog_Form);
         when KEY_LEFT =>
            if FieldIndex = 2 then
               Result := Driver(Dialog_Form, F_Previous_Char);
            end if;
         when KEY_RIGHT =>
            if FieldIndex = 2 then
               Result := Driver(Dialog_Form, F_Next_Char);
            end if;
         when 127 =>
            Result := Driver(Dialog_Form, F_Delete_Previous);
         when 27 =>
            return Hide_Dialog;
         when 10 =>
            if FieldIndex = 2 then
               Result := Go_Previous_Field(Dialog_Form);
               return Execute_Form_Keys(10);
            end if;
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
            return Hide_Dialog;
         when others =>
            if Key /= 91 then
               Result := Driver(Dialog_Form, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(Form_Window);
      end if;
      return EXECUTE_FORM;
   end Execute_Form_Keys;

end ActivateItems.UI;
