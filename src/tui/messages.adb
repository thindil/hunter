-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Bookmarks.UI; use Bookmarks.UI;
with CopyItems; use CopyItems;
with DeleteItems; use DeleteItems;
with MoveItems; use MoveItems;
with Utils.UI; use Utils.UI;

package body Messages is

   DialogForm: Forms.Form;
   FormWindow: Window;

   function Close_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Close_Command;

   -- ****o* MessagesTUI/MessagesTUI.Response_Command
   -- FUNCTION
   -- Hide message frame and do action, depends on user response
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MessageResponse answer
   -- Answer is the answer which the user selected by clicking in button
   -- SOURCE
   function Response_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Response_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Response_Command;

   procedure CreateMessagesUI is
   begin
      Add_Command("CloseMessage", Close_Command'Access);
      Add_Command("MessageResponse", Response_Command'Access);
   end CreateMessagesUI;

   procedure Show_Message(Message: String; Message_Type: String := "error") is
      Buttons_Fields: constant Field_Array_Access :=
        (if Message_Type /= "error" then new Field_Array(1 .. 5)
         else new Field_Array(1 .. 2));
      FormHeight: constant Line_Position := 7;
      FormLength: constant Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result;
      LineNumber: Line_Position := 1;
   begin
      Set_Cursor_Visibility(Visibility);
      if Message_Type /= "question" then
         Buttons_Fields.all(1) := New_Field(1, 7, (FormHeight - 1), 7, 0, 0);
         Set_Buffer(Buttons_Fields.all(1), 0, "[Close]");
         FieldOptions := Get_Options(Buttons_Fields.all(1));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(1), FieldOptions);
         Buttons_Fields.all(2) := Null_Field;
      else
         Buttons_Fields.all(1) := New_Field(1, 5, (FormHeight - 2), 1, 0, 0);
         Set_Buffer(Buttons_Fields.all(1), 0, "[Yes]");
         FieldOptions := Get_Options(Buttons_Fields.all(1));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(1), FieldOptions);
         Buttons_Fields.all(2) := New_Field(1, 4, (FormHeight - 2), 6, 0, 0);
         Set_Buffer(Buttons_Fields.all(2), 0, "[No]");
         FieldOptions := Get_Options(Buttons_Fields.all(2));
         FieldOptions.Edit := False;
         Set_Options(Buttons_Fields.all(2), FieldOptions);
         if New_Action = CLEARTRASH then
            Buttons_Fields.all(3) := Null_Field;
            Buttons_Fields.all(4) := Null_Field;
         else
            Buttons_Fields.all(3) :=
              New_Field(1, 13, (FormHeight - 1), 1, 0, 0);
            Set_Buffer(Buttons_Fields.all(3), 0, "[Yes for all]");
            FieldOptions := Get_Options(Buttons_Fields.all(3));
            FieldOptions.Edit := False;
            Set_Options(Buttons_Fields.all(3), FieldOptions);
            Buttons_Fields.all(4) :=
              New_Field(1, 12, (FormHeight - 1), 15, 0, 0);
            Set_Buffer(Buttons_Fields.all(4), 0, "[No for all]");
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
          (FormHeight + 2, 34, ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      Move_Cursor(FormWindow, LineNumber, 1);
      for I in Message'Range loop
         Add(FormWindow, Message(I));
         if I mod Positive(FormLength) = 0 then
            LineNumber := LineNumber + 1;
            Move_Cursor(FormWindow, LineNumber, 1);
         end if;
      end loop;
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
   begin
      case Key is
         when KEY_UP =>
            if New_Action in COPY | MOVE | CLEARTRASH then
               Result := Driver(DialogForm, F_Previous_Field);
               Result := Driver(DialogForm, F_End_Line);
            end if;
         when KEY_DOWN =>
            if New_Action in COPY | MOVE | CLEARTRASH then
               Result := Driver(DialogForm, F_Next_Field);
               Result := Driver(DialogForm, F_End_Line);
            end if;
         when 10 =>
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            if Option in "[Close]" | "[No for all]" then
               New_Action := CREATEFILE;
               CreateProgramMenu(True);
               Update_Directory_List(True);
               return DIRECTORY_VIEW;
            elsif Option = "[Yes for all]" then
               YesForAll := True;
            elsif Option = "[No]" then
               if New_Action = COPY then
                  if SkipCopying = DIRECTORY_VIEW then
                     New_Action := CREATEFILE;
                     CreateProgramMenu(True);
                     Update_Directory_List(True);
                     return DIRECTORY_VIEW;
                  end if;
                  return MESSAGE_FORM;
               elsif New_Action = MOVE then
                  if SkipMoving = DIRECTORY_VIEW then
                     New_Action := CREATEFILE;
                     CreateProgramMenu(True);
                     Update_Directory_List(True);
                     return DIRECTORY_VIEW;
                  end if;
                  return MESSAGE_FORM;
               elsif New_Action = CLEARTRASH then
                  New_Action := CREATEFILE;
                  UILocation := DIRECTORY_VIEW;
                  Update_Directory_List(True);
                  return DIRECTORY_VIEW;
               end if;
            end if;
            if New_Action = COPY then
               if CopySelected(Overwrite) = DIRECTORY_VIEW then
                  New_Action := CREATEFILE;
                  CreateProgramMenu(True);
                  Update_Directory_List(True);
                  return DIRECTORY_VIEW;
               end if;
               return MESSAGE_FORM;
            elsif New_Action = MOVE then
               if MoveSelected(Overwrite) = DIRECTORY_VIEW then
                  New_Action := CREATEFILE;
                  CreateProgramMenu(True);
                  Update_Directory_List(True);
                  return DIRECTORY_VIEW;
               end if;
               return MESSAGE_FORM;
            elsif New_Action = CLEARTRASH then
               begin
                  if DeleteSelected then
                     Current_Directory :=
                       To_Unbounded_String
                         (Normalize_Pathname
                            (To_String(Current_Directory) & "/.."));
                  end if;
               exception
                  when others =>
                     return Go_To_Bookmark(Mc(Interpreter, "{Home}"));
               end;
               if Current_Directory =
                 To_Unbounded_String
                   (Value("HOME") & "/.local/share/Trash/files") then
                  return Go_To_Bookmark(Mc(Interpreter, "{Home}"));
               else
                  New_Action := CREATEFILE;
                  UILocation := DIRECTORY_VIEW;
                  Update_Directory_List(True);
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

end Messages;
