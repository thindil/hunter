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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Utils.UI; use Utils.UI;

package body Messages is

   DialogForm: Forms.Form;
   FormWindow: Window;

   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Response_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Response_Command;

   procedure CreateMessagesUI is
   begin
      AddCommand("CloseMessage", Close_Command'Access);
      AddCommand("MessageResponse", Response_Command'Access);
   end CreateMessagesUI;

   procedure ShowMessage(Message: String; MessageType: String := "error") is
      Buttons_Fields: constant Field_Array_Access :=
        (if MessageType /= "error" then new Field_Array(1 .. 5)
         else new Field_Array(1 .. 2));
      FormHeight: constant Line_Position := 6;
      FormLength: constant Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result;
      LineNumber: Line_Position := 1;
   begin
      Set_Cursor_Visibility(Visibility);
      Buttons_Fields.all(1) := New_Field(1, 8, (FormHeight - 1), 7, 0, 0);
      Set_Buffer(Buttons_Fields.all(1), 0, "[Close]");
      FieldOptions := Get_Options(Buttons_Fields.all(1));
      FieldOptions.Edit := False;
      Set_Options(Buttons_Fields.all(1), FieldOptions);
      Buttons_Fields.all(2) := Null_Field;
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
         if I mod Positive(FormLength - 1) = 0 then
            LineNumber := LineNumber + 1;
            Move_Cursor(FormWindow, LineNumber, 1);
         end if;
      end loop;
      UnusedResult := Driver(DialogForm, F_First_Field);
      Box(FormWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(FormWindow);
   end ShowMessage;

   function Message_Keys(Key: Key_Code) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when 10 =>
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            UpdateDirectoryList(True);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      return MESSAGE_FORM;
   end Message_Keys;

end Messages;
