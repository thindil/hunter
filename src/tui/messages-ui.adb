-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Bookmarks.UI; use Bookmarks.UI;
with Common; use Common;
with CopyItems.UI; use CopyItems.UI;
with DeleteItems; use DeleteItems;
with MoveItems.UI; use MoveItems.UI;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body Messages.UI is

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure Show_Message(Message: String; Message_Type: String := "error") is
      Buttons_Fields: constant Field_Array_Access :=
        (if Message_Type /= "error" then new Field_Array(1 .. 5)
         else new Field_Array(1 .. 2));
      FormHeight: constant Line_Position :=
        (Message'Length / 30) + (if Message_Type = "error" then 3 else 4) +
        Line_Position(Count(Message, "" & LF));
      FormLength: constant Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result;
      LineNumber: Line_Position := 1;
   begin
      Set_Cursor_Visibility(Visibility);
      if Message_Type /= "question" then
         Buttons_Fields.all(1) :=
           New_Field
             (1, Column_Position'Value(Mc_Max("{Close}", Interpreter)) + 2,
              (FormHeight - 1), 7, 0, 0);
         Set_Buffer
           (Buttons_Fields.all(1), 0, "[" & Mc(Interpreter, "Close") & "]");
         FieldOptions := Get_Options(Buttons_Fields.all(1));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(1), FieldOptions);
         Buttons_Fields.all(2) := Null_Field;
      else
         Buttons_Fields.all(1) :=
           New_Field
             (1, Column_Position'Value(Mc_Max("{Yes}", Interpreter)) + 2,
              (FormHeight - 2), 1, 0, 0);
         Set_Buffer
           (Buttons_Fields.all(1), 0, "[" & Mc(Interpreter, "Yes") & "]");
         FieldOptions := Get_Options(Buttons_Fields.all(1));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(1), FieldOptions);
         Buttons_Fields.all(2) :=
           New_Field
             (1, Column_Position'Value(Mc_Max("{No}", Interpreter)) + 2,
              (FormHeight - 2), 6, 0, 0);
         Set_Buffer
           (Buttons_Fields.all(2), 0, "[" & Mc(Interpreter, "No") & "]");
         FieldOptions := Get_Options(Buttons_Fields.all(2));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(2), FieldOptions);
         if New_Action = CLEARTRASH then
            Buttons_Fields.all(3) := Null_Field;
            Buttons_Fields.all(4) := Null_Field;
         else
            Buttons_Fields.all(3) :=
              New_Field
                (1,
                 Column_Position'Value(Mc_Max("{Overwrite all}", Interpreter)) +
                 2,
                 (FormHeight - 1), 1, 0, 0);
            Set_Buffer
              (Buttons_Fields.all(3), 0,
               "[" & Mc(Interpreter, "{Overwrite all}") & "]");
            FieldOptions := Get_Options(Buttons_Fields.all(3));
            FieldOptions.Edit := False;
            Set_Options(Buttons_Fields.all(3), FieldOptions);
            Buttons_Fields.all(4) :=
              New_Field
                (1, Column_Position'Value(Mc_Max("{Cancel}", Interpreter)) + 2,
                 (FormHeight - 1), 15, 0, 0);
            Set_Buffer
              (Buttons_Fields.all(4), 0,
               "[" & Mc(Interpreter, "{Cancel}") & "]");
            FieldOptions := Get_Options(Buttons_Fields.all(4));
            FieldOptions.Edit := False;
            Set_Options(Buttons_Fields.all(4), FieldOptions);
         end if;
         Buttons_Fields.all(5) := Null_Field;
      end if;
      DialogForm := New_Form(Buttons_Fields);
      Set_Options(DialogForm, (others => False));
      FormWindow :=
        Create
          (FormHeight + 2, FormLength + 2, ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      Move_Cursor(FormWindow, LineNumber, 1);
      declare
         Index: Positive := Message'First;
         Space_Index: Natural;
         End_Index: Positive;
      begin
         while Index <= Message'Last loop
            End_Index := Index + Positive(FormLength) - 1;
            if End_Index > Message'Last then
               End_Index := Message'Last;
            end if;
            Space_Index :=
              Ada.Strings.Fixed.Index(Message, " ", End_Index, Backward);
            if Space_Index = 0 or Space_Index < Index then
               Space_Index := End_Index;
            end if;
            for I in Index .. Space_Index loop
               Add(FormWindow, Message(I));
            end loop;
            Index := Space_Index + 1;
            LineNumber := LineNumber + 1;
            Move_Cursor(FormWindow, LineNumber, 1);
         end loop;
      end;
      Update_Directory_List;
      Show_Preview;
      UnusedResult := Driver(DialogForm, F_First_Field);
      Box(FormWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(FormWindow);
   end Show_Message;

   function Message_Keys(Key: Key_Code) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
      Option: constant String := Get_Buffer(Current(DialogForm));
      Overwrite: Boolean := True;
      Result: Forms.Driver_Result := Unknown_Request;
      function Hide_Dialog return UI_Locations is
      begin
         New_Action := Default_Item_Action;
         CreateProgramMenu(True);
         Update_Directory_List(True);
         Show_Preview;
         return DIRECTORY_VIEW;
      end Hide_Dialog;
   begin
      case Key is
         when KEY_UP =>
            if New_Action in COPY | MOVE | CLEARTRASH then
               Result := Go_Previous_Field(DialogForm);
            end if;
         when KEY_DOWN =>
            if New_Action in COPY | MOVE | CLEARTRASH then
               Result := Go_Next_Field(DialogForm);
            end if;
         when 10 =>
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            UILocation := DIRECTORY_VIEW;
            if Option in "[" & Mc(Interpreter, "Close") & "]" |
                  "[" & Mc(Interpreter, "{No for all}") & "]" then
               return Hide_Dialog;
            elsif Option = "[" & Mc(Interpreter, "{Overwrite all}") & "]" then
               Yes_For_All := True;
            elsif Option = "[" & Mc(Interpreter, "No") & "]" then
               if New_Action = COPY then
                  if SkipCopying = DIRECTORY_VIEW then
                     return Hide_Dialog;
                  end if;
                  return MESSAGE_FORM;
               elsif New_Action = MOVE then
                  if SkipMoving = DIRECTORY_VIEW then
                     return Hide_Dialog;
                  end if;
                  return MESSAGE_FORM;
               elsif New_Action = CLEARTRASH then
                  return Hide_Dialog;
               end if;
            end if;
            if New_Action = COPY then
               if CopySelected(Overwrite) = DIRECTORY_VIEW then
                  return Hide_Dialog;
               end if;
               return MESSAGE_FORM;
            elsif New_Action = MOVE then
               if MoveSelected(Overwrite) = DIRECTORY_VIEW then
                  return Hide_Dialog;
               end if;
               return MESSAGE_FORM;
            elsif New_Action = CLEARTRASH then
               begin
                  if Delete_Selected(Interpreter) then
                     Common.Current_Directory :=
                       To_Unbounded_String
                         (Normalize_Pathname
                            (To_String(Common.Current_Directory) & "/.."));
                  end if;
               exception
                  when others =>
                     return Go_To_Bookmark(Mc(Interpreter, "{Home}"));
               end;
               if Common.Current_Directory =
                 To_Unbounded_String
                   (Value("HOME") & "/.local/share/Trash/files") then
                  return Go_To_Bookmark(Mc(Interpreter, "{Home}"));
               else
                  New_Action := Default_Item_Action;
                  Update_Directory_List(True);
                  Show_Preview;
               end if;
               return DIRECTORY_VIEW;
            end if;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return MESSAGE_FORM;
   end Message_Keys;

end Messages.UI;
