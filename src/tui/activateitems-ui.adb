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

   -- ****iv* ActivateItemsTUI/ActivateItemsTUI.Dialog_Form
   -- FUNCTION
   -- The form to set command to execute the selected item
   -- SOURCE
   Dialog_Form: Forms.Form;
   -- ****

   -- ****if* ActivateItemsTUI/ActivateItemsTUI.Get_Dialog_Form
   -- FUNCTION
   -- Get the execute items with dialog form
   -- RESULT
   -- The ncurses dialog form for set execute items with command
   -- SOURCE
   function Get_Dialog_Form return Forms.Form is
      -- ****
   begin
      return Dialog_Form;
   end Get_Dialog_Form;

   -- ****if* ActivateItemsTUI/ActivateItemsTUI.Set_Dialog_Form
   -- FUNCTION
   -- Set the new value for the dialog form for execute item with command
   -- PARAMETERS
   -- New_Form - The new value for the Dialog_Form
   -- SOURCE
   procedure Set_Dialog_Form(New_Form: Forms.Form) is
      -- ****
   begin
      Dialog_Form := New_Form;
   end Set_Dialog_Form;

   -- ****iv* ActivateItemsTUI/ActivateItemsTUI.Form_Window
   -- FUNCTION
   -- The window to show form to set execute item command
   -- SOURCE
   Form_Window: Window;
   -- ****

   procedure Show_Execute_With_Dialog is
      Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      Visibility: Cursor_Visibility := Normal;
      Field_Options: Field_Option_Set;
   begin
      Set_Cursor_Visibility(Visibility => Visibility);
      Create_Fields.all(1) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max
                  (Strings => "{Enter the application to execute:}",
                   Interp => Interpreter)),
           Top => 0, Left => 4, Off_Screen => 0, More_Buffers => 0);
      Set_Buffer
        (Fld => Create_Fields.all(1), Buffer => 0,
         Str =>
           Mc
             (Interp => Interpreter,
              Src_String => "{Enter the application to execute:}"));
      Field_Options := Get_Options(Fld => Create_Fields.all(1));
      Field_Options.Active := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => Create_Fields.all(1), Options => Field_Options);
      Create_Fields.all(2) :=
        New_Field
          (Height => 1, Width => 40, Top => 1, Left => 0, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer(Fld => Create_Fields.all(2), Buffer => 0, Str => "");
      Field_Options := Get_Options(Fld => Create_Fields.all(2));
      Field_Options.Auto_Skip := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => Create_Fields.all(2), Options => Field_Options);
      Create_Fields.all(3) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max(Strings => "{Cancel}", Interp => Interpreter)) +
             2,
           Top => 2, Left => 7, Off_Screen => 0, More_Buffers => 0);
      Set_Buffer
        (Fld => Create_Fields.all(3), Buffer => 0,
         Str =>
           "[" & Mc(Interp => Interpreter, Src_String => "{Cancel}") & "]");
      Field_Options := Get_Options(Fld => Create_Fields.all(3));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => Create_Fields.all(3), Options => Field_Options);
      Create_Fields.all(4) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max(Strings => "{Execute}", Interp => Interpreter)) +
             2,
           Top => 2, Left => 23, Off_Screen => 0, More_Buffers => 0);
      Field_Options := Get_Options(Fld => Create_Fields.all(4));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => Create_Fields.all(4), Options => Field_Options);
      Set_Buffer
        (Fld => Create_Fields.all(4), Buffer => 0,
         Str =>
           "[" & Mc(Interp => Interpreter, Src_String => "{Execute}") & "]");
      Create_Fields.all(5) := Null_Field;
      Create_Execute_With_Dialog_Block :
      declare
         New_Dialog_Form: Forms.Form := New_Form(Fields => Create_Fields);
         Form_Height: Line_Position;
         Form_Length: Column_Position;
      begin
         Set_Current(Frm => New_Dialog_Form, Fld => Create_Fields(2));
         Set_Options(Frm => New_Dialog_Form, Options => (others => False));
         Create_Dialog
           (DialogForm => New_Dialog_Form, FormWindow => Form_Window,
            Form_Height => Form_Height, Form_Length => Form_Length);
         Set_Dialog_Form(New_Form => New_Dialog_Form);
      end Create_Execute_With_Dialog_Block;
   end Show_Execute_With_Dialog;

   function Execute_Form_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      Dialog_Frm: Forms.Form := Get_Dialog_Form;
      Field_Index: constant Positive :=
        Get_Index(Fld => Current(Frm => Dialog_Frm));
      Value: constant String :=
        Trim
          (Source => Get_Buffer(Fld => Fields(Frm => Dialog_Frm, Index => 2)),
           Side => Both);
      Command_Name: Unbounded_String := Null_Unbounded_String;
      Pid: GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;
      Space_Index: Natural range 0 .. Value'Length := 0;
      Arguments: Argument_List_Access;
      function Hide_Dialog
        (With_Message: Boolean := False) return UI_Locations is
         Visibility: Cursor_Visibility := Invisible;
      begin
         Post(Frm => Dialog_Frm, Post => False);
         Delete(Frm => Dialog_Frm);
         Set_Dialog_Form(New_Form => Dialog_Frm);
         if With_Message then
            UILocation := MESSAGE_FORM;
         else
            Set_Cursor_Visibility(Visibility => Visibility);
            UILocation := DIRECTORY_VIEW;
            Update_Directory_List(Clear => True);
            Show_Preview;
         end if;
         return UILocation;
      end Hide_Dialog;
   begin
      case Key is
         when KEY_UP =>
            Result := Go_Previous_Field(DialogForm => Dialog_Frm);
         when KEY_DOWN =>
            Result := Go_Next_Field(DialogForm => Dialog_Frm);
         when KEY_LEFT =>
            if Field_Index = 2 then
               Result := Driver(Frm => Dialog_Frm, Key => F_Previous_Char);
            end if;
         when KEY_RIGHT =>
            if Field_Index = 2 then
               Result := Driver(Frm => Dialog_Frm, Key => F_Next_Char);
            end if;
         when 127 =>
            Result := Driver(Frm => Dialog_Frm, Key => F_Delete_Previous);
         when 27 =>
            return Hide_Dialog;
         when 10 =>
            if Field_Index = 2 then
               Result := Go_Previous_Field(DialogForm => Dialog_Frm);
               return Execute_Form_Keys(Key => 10);
            end if;
            if Field_Index = 4 then
               Space_Index := Index(Source => Value, Pattern => " ");
               Command_Name :=
                 (if Space_Index > 0 then
                    To_Unbounded_String(Source => Value(1 .. Space_Index - 1))
                  else To_Unbounded_String(Source => Value));
               Command_Name :=
                 To_Unbounded_String
                   (Source =>
                      Find_Executable
                        (Name => To_String(Source => Command_Name)));
               if Command_Name = Null_Unbounded_String then
                  Show_Message
                    (Mc(Interpreter, "{Can't find command:}") & " " &
                     (if Space_Index > 0 then Value(1 .. Space_Index - 1)
                      else Value));
                  return Hide_Dialog(True);
               end if;
               Arguments :=
                 (if Space_Index > 0 then
                    Argument_String_To_List
                      (Value(Space_Index .. Value'Length) & " @2")
                  else Argument_String_To_List("@2"));
               Replace_Substitutes_Loop :
               for I in Arguments'Range loop
                  if Arguments(I).all = "@2" then
                     Arguments(I) := new String'(To_String(Current_Selected));
                  end if;
               end loop Replace_Substitutes_Loop;
               Pid :=
                 Non_Blocking_Spawn
                   (Full_Name(To_String(Command_Name)), Arguments.all);
               if Pid = GNAT.OS_Lib.Invalid_Pid then
                  Show_Message
                    (Mc(Interpreter, "{Can't execute this command}"));
                  return Hide_Dialog(True);
               end if;
            end if;
            return Hide_Dialog;
         when others =>
            if Key /= 91 then
               Result := Driver(Dialog_Frm, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(Form_Window);
      end if;
      return EXECUTE_FORM;
   end Execute_Form_Keys;

end ActivateItems.UI;
