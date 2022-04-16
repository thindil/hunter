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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with Messages.UI; use Messages.UI;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body AboutDialog.UI is

   -- ****iv* AboutDialogTUI/AboutDialogTUI.Dialog_Form
   -- FUNCTION
   -- The form for showing information about the program
   -- SOURCE
   Dialog_Form: Forms.Form;
   -- ****

   -- ****if* AboutDialogTUI/Get_Dialog_Form
   -- FUNCTION
   -- Get the about dialog form
   -- RESULT
   -- The ncurses dialog form for information about the program
   -- SOURCE
   function Get_Dialog_Form return Forms.Form is
      -- ****
   begin
      return Dialog_Form;
   end Get_Dialog_Form;

   -- ****iv* AboutDialogTUI/AboutDialogTUI.Form_Window
   -- FUNCTION
   -- The window to show information about the program
   -- SOURCE
   Form_Window: Window;
   -- ****

   -- ****if* AboutDialogTUI/Get_Form_Window
   -- FUNCTION
   -- Get the about window
   -- RESULT
   -- The ncurses window for information about the program
   -- SOURCE
   function Get_Form_Window return Window is
      -- ****
   begin
      return Form_Window;
   end Get_Form_Window;

   procedure Show_About_Dialog is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 9);
      Visibility: Cursor_Visibility := Normal;
      Field_Options: Field_Option_Set;
      Field_Position: Column_Position := 0;
   begin
      Set_Cursor_Visibility(Visibility => Visibility);
      About_Fields.all(1) :=
        New_Field
          (Height => 1, Width => 36, Top => 0, Left => 1, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(1), Buffer => 0,
         Str => "Hunter - Text file manager for Linux");
      Field_Options := Get_Options(Fld => About_Fields.all(1));
      Field_Options.Active := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => About_Fields.all(1), Options => Field_Options);
      About_Fields.all(2) :=
        New_Field
          (Height => 1, Width => Column_Position(AboutDialog.Copyright'Length),
           Top => 1, Left => 4, Off_Screen => 0, More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(2), Buffer => 0,
         Str => AboutDialog.Copyright);
      Set_Options(Fld => About_Fields.all(2), Options => Field_Options);
      About_Fields.all(3) :=
        New_Field
          (Height => 1,
           Width => Column_Position(Length(Source => AboutDialog.License)),
           Top => 2, Left => 8, Off_Screen => 0, More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(3), Buffer => 0,
         Str => To_String(Source => AboutDialog.License));
      Set_Options(Fld => About_Fields.all(3), Options => Field_Options);
      About_Fields.all(4) :=
        New_Field
          (Height => 1,
           Width => Column_Position(Length(Source => AboutDialog.Version)),
           Top => 3, Left => 4, Off_Screen => 0, More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(4), Buffer => 0,
         Str => To_String(Source => AboutDialog.Version));
      Set_Options(Fld => About_Fields.all(4), Options => Field_Options);
      About_Fields.all(5) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position(Length(Source => AboutDialog.Website_Text)) + 2,
           Top => 5, Left => Field_Position, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(5), Buffer => 0,
         Str => "[" & To_String(Source => AboutDialog.Website_Text) & "]");
      Field_Options := Get_Options(Fld => About_Fields.all(5));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => About_Fields.all(5), Options => Field_Options);
      Field_Position :=
        Column_Position(Length(Source => AboutDialog.Website_Text)) + 2;
      About_Fields.all(6) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max(Strings => "{Programmers}", Interp => Interpreter)) +
             2,
           Top => 5, Left => Field_Position, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(6), Buffer => 0,
         Str => "[" & To_String(Source => AboutDialog.Programmers_Text) & "]");
      Set_Options(Fld => About_Fields.all(6), Options => Field_Options);
      Field_Position :=
        Field_Position +
        Column_Position'Value
          (Mc_Max(Strings => "{Programmers}", Interp => Interpreter)) +
        2;
      About_Fields.all(7) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max(Strings => "{Translators}", Interp => Interpreter)) +
             2,
           Top => 5, Left => Field_Position, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(7), Buffer => 0,
         Str => "[" & To_String(Source => AboutDialog.Translators_Text) & "]");
      Set_Options(Fld => About_Fields.all(7), Options => Field_Options);
      Field_Position :=
        Field_Position +
        Column_Position'Value
          (Mc_Max(Strings => "{Translators}", Interp => Interpreter)) +
        2;
      About_Fields.all(8) :=
        New_Field
          (Height => 1,
           Width =>
             Column_Position'Value
               (Mc_Max(Strings => "{Close}", Interp => Interpreter)) +
             2,
           Top => 5, Left => Field_Position, Off_Screen => 0,
           More_Buffers => 0);
      Set_Buffer
        (Fld => About_Fields.all(8), Buffer => 0,
         Str => "[" & To_String(Source => Close_Text) & "]");
      Set_Options(Fld => About_Fields.all(8), Options => Field_Options);
      About_Fields.all(9) := Null_Field;
      Create_About_Dialog_Block:
      declare
         New_Dialog_Form: Forms.Form := New_Form(Fields => About_Fields);
         --## rule off IMPROPER_INITIALIZATION
         Local_Form_Window: Window := Get_Form_Window;
         --## rule on IMPROPER_INITIALIZATION
         Form_Height: Line_Position := 0;
         Form_Length: Column_Position := 0;
      begin
         Set_Current(Frm => New_Dialog_Form, Fld => About_Fields(5));
         Create_Dialog
           (DialogForm => New_Dialog_Form, FormWindow => Local_Form_Window,
            Form_Height => Form_Height, Form_Length => Form_Length);
         Dialog_Form := New_Dialog_Form;
         Form_Window := Local_Form_Window;
      end Create_About_Dialog_Block;
   end Show_About_Dialog;

   procedure Show_Developers_Dialog(Developers: Boolean := True) is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 3);
      Field_Options: Field_Option_Set;
   begin
      About_Fields.all(1) :=
        New_Field
          (Height => 1, Width => 44, Top => 0, Left => 1, Off_Screen => 0,
           More_Buffers => 0);
      if Developers then
         Set_Buffer
           (Fld => About_Fields.all(1), Buffer => 0, Str => Programmer);
      else
         Set_Buffer
           (Fld => About_Fields.all(1), Buffer => 0, Str => Translator);
      end if;
      Field_Options := Get_Options(Fld => About_Fields.all(1));
      Field_Options.Active := False; --## rule line off ASSIGNMENTS
      Set_Options(Fld => About_Fields.all(1), Options => Field_Options);
      About_Fields.all(2) :=
        New_Field
          (Height => 1,
           Width => Column_Position(Length(Source => Close_Text) + 2),
           Top => 2, Left => 10, Off_Screen => 0, More_Buffers => 0);
      Field_Options := Get_Options(Fld => About_Fields.all(2));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Buffer
        (Fld => About_Fields.all(2), Buffer => 0,
         Str => "[" & To_String(Source => Close_Text) & "]");
      Set_Options(Fld => About_Fields.all(2), Options => Field_Options);
      About_Fields.all(3) := Null_Field;
      Create_Developers_Dialog_Block:
      declare
         New_Dialog_Form: Forms.Form := New_Form(About_Fields);
         --## rule off IMPROPER_INITIALIZATION
         Local_Form_Window: Window := Get_Form_Window;
         --## rule on IMPROPER_INITIALIZATION
         Form_Height: Line_Position := 0;
         Form_Length: Column_Position := 0;
      begin
         Set_Current(New_Dialog_Form, About_Fields(2));
         Create_Dialog
           (New_Dialog_Form, Local_Form_Window, Form_Height, Form_Length);
         Dialog_Form := New_Dialog_Form;
         Form_Window := Local_Form_Window;
      end Create_Developers_Dialog_Block;
   end Show_Developers_Dialog;

   function About_View_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(Get_Dialog_Form));
      Visibility: Cursor_Visibility := Invisible;
      Dialog_Frm: Forms.Form := Get_Dialog_Form;
      function HideAboutDialog
        (With_Message: Boolean := False) return UI_Locations is
      begin
         Set_Cursor_Visibility(Visibility);
         Post(Dialog_Frm, False);
         Delete(Dialog_Frm);
         Dialog_Form := Dialog_Frm;
         if With_Message then
            UILocation := MESSAGE_FORM;
         else
            Show_Preview;
            UILocation := DIRECTORY_VIEW;
            Update_Directory_List;
         end if;
         return UILocation;
      end HideAboutDialog;
   begin
      case Key is
         when KEY_UP | KEY_LEFT =>
            Result := Go_Previous_Field(Dialog_Frm);
         when KEY_DOWN | KEY_RIGHT =>
            Result := Go_Next_Field(Dialog_Frm);
         when 27 =>
            return HideAboutDialog;
         when 10 =>
            case FieldIndex is
               when 5 =>
                  declare
                     ProcessId: Process_Id;
                  begin
                     ProcessId :=
                       Non_Blocking_Spawn
                         (Locate_Exec_On_Path("xdg-open").all,
                          Argument_String_To_List(Website).all);
                     if ProcessId = Invalid_Pid then
                        return ABOUT_FORM;
                     end if;
                  exception
                     when Constraint_Error =>
                        Show_Message
                          (Mc
                             (Interpreter,
                              "{Can't find web browser to open the link}"));
                        return HideAboutDialog(True);
                  end;
               when 6 | 7 =>
                  Delete_Dialog(Dialog_Frm);
                  Dialog_Form := Dialog_Frm;
                  if FieldIndex = 6 then
                     Show_Developers_Dialog;
                  else
                     Show_Developers_Dialog(False);
                  end if;
                  return DEVELOPERS_VIEW;
               when 8 =>
                  return HideAboutDialog;
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(Get_Form_Window);
      end if;
      return ABOUT_FORM;
   end About_View_Keys;

   function Developers_Keys(Key: Key_Code) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
      Dialog_Frm: Forms.Form := Get_Dialog_Form;
   begin
      if Key = 10 then
         Set_Cursor_Visibility(Visibility);
         Delete_Dialog(Dialog_Frm);
         Dialog_Form := Dialog_Frm;
         return DIRECTORY_VIEW;
      end if;
      return DEVELOPERS_VIEW;
   end Developers_Keys;

end AboutDialog.UI;
