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

   -- ****iv* AboutDialogTUI/AboutDialogTUI.Form_Window
   -- FUNCTION
   -- The window to show information about the program
   -- SOURCE
   Form_Window: Window;
   -- ****

   procedure Show_About_Dialog is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 9);
      Form_Height: Line_Position;
      Form_Length: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      Field_Options: Field_Option_Set;
      Field_Position: Column_Position := 0;
   begin
      Set_Cursor_Visibility(Visibility);
      About_Fields.all(1) := New_Field(1, 36, 0, 1, 0, 0);
      Set_Buffer
        (About_Fields.all(1), 0, "Hunter - Text file manager for Linux");
      Field_Options := Get_Options(About_Fields.all(1));
      Field_Options.Active := False;
      Set_Options(About_Fields.all(1), Field_Options);
      About_Fields.all(2) :=
        New_Field
          (1, Column_Position(AboutDialog.Copyright'Length), 1, 4, 0, 0);
      Set_Buffer(About_Fields.all(2), 0, AboutDialog.Copyright);
      Set_Options(About_Fields.all(2), Field_Options);
      About_Fields.all(3) :=
        New_Field(1, Column_Position(Length(AboutDialog.License)), 2, 8, 0, 0);
      Set_Buffer(About_Fields.all(3), 0, To_String(AboutDialog.License));
      Set_Options(About_Fields.all(3), Field_Options);
      About_Fields.all(4) :=
        New_Field(1, Column_Position(Length(AboutDialog.Version)), 3, 4, 0, 0);
      Set_Buffer(About_Fields.all(4), 0, To_String(AboutDialog.Version));
      Set_Options(About_Fields.all(4), Field_Options);
      About_Fields.all(5) :=
        New_Field
          (1, Column_Position(Length(AboutDialog.Website_Text)) + 2, 5,
           Field_Position, 0, 0);
      Set_Buffer
        (About_Fields.all(5), 0,
         "[" & To_String(AboutDialog.Website_Text) & "]");
      Field_Options := Get_Options(About_Fields.all(5));
      Field_Options.Edit := False;
      Set_Options(About_Fields.all(5), Field_Options);
      Field_Position := Column_Position(Length(AboutDialog.Website_Text)) + 2;
      About_Fields.all(6) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Programmers}", Interpreter)) + 2,
           5, Field_Position, 0, 0);
      Set_Buffer
        (About_Fields.all(6), 0,
         "[" & To_String(AboutDialog.Programmers_Text) & "]");
      Set_Options(About_Fields.all(6), Field_Options);
      Field_Position :=
        Field_Position +
        Column_Position'Value(Mc_Max("{Programmers}", Interpreter)) + 2;
      About_Fields.all(7) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Translators}", Interpreter)) + 2,
           5, Field_Position, 0, 0);
      Set_Buffer
        (About_Fields.all(7), 0,
         "[" & To_String(AboutDialog.Translators_Text) & "]");
      Set_Options(About_Fields.all(7), Field_Options);
      Field_Position :=
        Field_Position +
        Column_Position'Value(Mc_Max("{Translators}", Interpreter)) + 2;
      About_Fields.all(8) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Close}", Interpreter)) + 2, 5,
           Field_Position, 0, 0);
      Set_Buffer
        (About_Fields.all(8), 0, "[" & To_String(Source => Close_Text) & "]");
      Set_Options(About_Fields.all(8), Field_Options);
      About_Fields.all(9) := Null_Field;
      Dialog_Form := New_Form(About_Fields);
      Set_Current(Dialog_Form, About_Fields(5));
      Create_Dialog(Dialog_Form, Form_Window, Form_Height, Form_Length);
   end Show_About_Dialog;

   procedure Show_Developers_Dialog(Developers: Boolean := True) is
      About_Fields: constant Field_Array_Access := new Field_Array(1 .. 3);
      Form_Height: Line_Position;
      Form_Length: Column_Position;
      Field_Options: Field_Option_Set;
   begin
      About_Fields.all(1) := New_Field(1, 44, 0, 1, 0, 0);
      if Developers then
         Set_Buffer(About_Fields.all(1), 0, Programmer);
      else
         Set_Buffer(About_Fields.all(1), 0, Translator);
      end if;
      Field_Options := Get_Options(About_Fields.all(1));
      Field_Options.Active := False;
      Set_Options(About_Fields.all(1), Field_Options);
      About_Fields.all(2) :=
        New_Field
          (1, Column_Position(Length(Source => Close_Text) + 2), 2, 10, 0, 0);
      Field_Options := Get_Options(About_Fields.all(2));
      Field_Options.Edit := False;
      Set_Buffer
        (About_Fields.all(2), 0, "[" & To_String(Source => Close_Text) & "]");
      Set_Options(About_Fields.all(2), Field_Options);
      About_Fields.all(3) := Null_Field;
      Dialog_Form := New_Form(About_Fields);
      Set_Current(Dialog_Form, About_Fields(2));
      Create_Dialog(Dialog_Form, Form_Window, Form_Height, Form_Length);
   end Show_Developers_Dialog;

   function About_View_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(Dialog_Form));
      Visibility: Cursor_Visibility := Invisible;
      function HideAboutDialog
        (With_Message: Boolean := False) return UI_Locations is
      begin
         Set_Cursor_Visibility(Visibility);
         Post(Dialog_Form, False);
         Delete(Dialog_Form);
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
            Result := Go_Previous_Field(Dialog_Form);
         when KEY_DOWN | KEY_RIGHT =>
            Result := Go_Next_Field(Dialog_Form);
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
                  Delete_Dialog(Dialog_Form);
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
         Refresh(Form_Window);
      end if;
      return ABOUT_FORM;
   end About_View_Keys;

   function Developers_Keys(Key: Key_Code) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
   begin
      if Key = 10 then
         Set_Cursor_Visibility(Visibility);
         Delete_Dialog(Dialog_Form);
         return DIRECTORY_VIEW;
      end if;
      return DEVELOPERS_VIEW;
   end Developers_Keys;

end AboutDialog.UI;
