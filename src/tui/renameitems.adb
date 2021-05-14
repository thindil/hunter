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
with Interfaces.C; use Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with RefreshData; use RefreshData;
with Utils.UI; use Utils.UI;

package body RenameItems is

   -- ****o* RenameItems/RenameItems.Rename_Command
   -- FUNCTION
   -- Rename currently selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Rename
   -- SOURCE
   function Rename_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Command
     (ClientData: Integer;
      Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      -- ****
      NewName, ActionBlocker: Unbounded_String;
      Success: Boolean;
   begin
      if Argc = 1 then
         Tcl_SetResult(Interp, "1");
         return TCL_OK;
      end if;
      NewName := MainWindow.Current_Directory & "/" & CArgv.Arg(Argv, 1);
      if Exists(To_String(NewName)) or
        Is_Symbolic_Link(To_String(NewName)) then
         ActionBlocker :=
           (if
              Is_Directory(To_String(NewName))
            then
              To_Unbounded_String(Mc(Interp, "{directory}"))
            else To_Unbounded_String(Mc(Interp, "{file}")));
         ShowMessage
           (Mc(Interp, "{You can't rename}") &
            " " &
            To_String(Current_Selected) &
            " " &
            Mc(Interp, "{to}") &
            " " &
            To_String(NewName) &
            " " &
            Mc(Interp, "{because there exists}") &
            " " &
            To_String(ActionBlocker) &
            " " &
            Mc(Interp, "{with that name}"));
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewName))) then
         ShowMessage
           (Mc(Interp, "{You don't have permissions to rename}") &
            " " &
            To_String(NewName));
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      Rename_File(To_String(Current_Selected), To_String(NewName), Success);
      if not Success then
         ShowMessage
           (Mc(Interp, "{Can't rename}") &
            " " &
            To_String(Current_Selected) &
            ".");
         return TCL_OK;
      end if;
      Current_Selected := NewName;
      LoadDirectory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Rename_Command;

   procedure AddCommands is
   begin
      Add_Command("Rename", Rename_Command'Access);
   end AddCommands;

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure ShowRenameForm is
      Rename_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result := Unknown_Request;
   begin
      Set_Cursor_Visibility(Visibility);
      Rename_Fields.all(1) := New_Field(1, 30, 0, 8, 0, 0);
      Set_Buffer(Rename_Fields.all(1), 0, "Enter a new name:");
      FieldOptions := Get_Options(Rename_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Rename_Fields.all(1), FieldOptions);
      Rename_Fields.all(2) := New_Field(1, 40, 1, 0, 0, 0);
      Set_Buffer
        (Rename_Fields.all(2),
         0,
         Simple_Name(To_String(Current_Selected)));
      FieldOptions := Get_Options(Rename_Fields.all(2));
      FieldOptions.Auto_Skip := False;
      Set_Options(Rename_Fields.all(2), FieldOptions);
      Rename_Fields.all(3) := New_Field(1, 8, 2, 7, 0, 0);
      Set_Buffer(Rename_Fields.all(3), 0, "[Cancel]");
      FieldOptions := Get_Options(Rename_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Rename_Fields.all(3), FieldOptions);
      Rename_Fields.all(4) := New_Field(1, 8, 2, 23, 0, 0);
      FieldOptions := Get_Options(Rename_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Rename_Fields.all(4), FieldOptions);
      Set_Buffer(Rename_Fields.all(4), 0, "[Rename]");
      Rename_Fields.all(5) := Null_Field;
      DialogForm := New_Form(Rename_Fields);
      Set_Current(DialogForm, Rename_Fields(2));
      Set_Options(DialogForm, (others => False));
      Scale(DialogForm, FormHeight, FormLength);
      FormWindow :=
        Create
          (FormHeight + 2,
           FormLength + 2,
           ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Box(FormWindow, Default_Character, Default_Character);
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm,
         Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      UnusedResult := Driver(DialogForm, REQ_END_LINE);
      Refresh;
      Refresh(FormWindow);
   end ShowRenameForm;

   function Rename_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
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
               Tcl_Eval
                 (Interpreter,
                  "Rename " & Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
               if Tcl_GetResult(Interpreter) = "0" then
                  return MESSAGE_FORM;
               end if;
            end if;
            if FieldIndex /= 2 then
               Set_Cursor_Visibility(Visibility);
               Post(DialogForm, False);
               Delete(DialogForm);
               Update_Directory_List(True);
               return DIRECTORY_VIEW;
            end if;
         when others =>
            if Key /= 91 then
               Result := Driver(DialogForm, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return RENAME_FORM;
   end Rename_Keys;

end RenameItems;
