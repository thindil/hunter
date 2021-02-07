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
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body CreateItems is

   -- ****o* CreateItemsTUI/CreateItemsTUI.Create_Item_Command
   -- FUNCTION
   -- Create the new item of the selected name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CreateItem itemname
   -- ItemName is the name of item to create
   -- SOURCE
   function Create_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Create_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      NewItemName, ActionString, ActionBlocker, Destination: Unbounded_String;
      File: File_Descriptor;
      Hunter_Create_Exception: exception;
   begin
      if Argc = 1 then
         Tcl_SetResult(Interp, "1");
         return TCL_OK;
      end if;
      NewItemName := CurrentDirectory & "/" & CArgv.Arg(Argv, 1);
      if Exists(To_String(NewItemName)) or
        Is_Symbolic_Link(To_String(NewItemName)) then
         if NewAction = CREATEFILE then
            ActionString :=
              To_Unbounded_String
                (Mc(Interp, "{create}") & " file " & Mc(Interp, "{with}"));
         elsif NewAction = CREATEDIRECTORY then
            ActionString :=
              To_Unbounded_String
                (Mc(Interp, "{create}") & " directory " &
                 Mc(Interp, "{with}"));
         else
            ActionString :=
              To_Unbounded_String
                (Mc(Interp, "{create}") & " link " & Mc(Interp, "{with}"));
         end if;
         ActionBlocker :=
           (if Is_Directory(To_String(NewItemName)) then
              To_Unbounded_String(Mc(Interp, "directory"))
            else To_Unbounded_String(Mc(Interp, "file")));
         ShowMessage
           (Mc(Interp, "{You can't}") & " " & To_String(ActionString) & " " &
            Mc(Interp, "{name}") & " '" & To_String(NewItemName) & "' " &
            Mc(Interp, "{because there exists}") & " " &
            To_String(ActionBlocker) & " " & Mc(Interp, "{with that name.}"));
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewItemName))) then
         ShowMessage
           (Mc(Interp, "{You don't have permissions to write to}") & " " &
            Containing_Directory(To_String(NewItemName)));
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      case NewAction is
         when CREATEDIRECTORY =>
            Create_Path(To_String(NewItemName));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(NewItemName)));
            File := Create_File(To_String(NewItemName), Binary);
            Close(File);
         when CREATELINK =>
            Destination := DestinationDirectory;
--            if Selection(DirectoryView)'Length > 0 then
--               Destination :=
--                 DestinationDirectory &
--                 SecondItemsList(Positive'Value(Selection(DirectoryView)))
--                   .Name;
--            end if;
            Tcl_Eval
              (Interp,
               "file link -symbolic {" & To_String(NewItemName) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interp, "{Invalid action type}");
      end case;
      if not Settings.StayInOld and then NewAction /= CREATELINK then
         CurrentDirectory :=
           To_Unbounded_String(Containing_Directory(To_String(NewItemName)));
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateWatch(To_String(CurrentDirectory));
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Create_Item_Command;

   procedure AddCommands is
   begin
      AddCommand("CreateItem", Create_Item_Command'Access);
   end AddCommands;

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure ShowCreateForm(Create_Type: String) is
      Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      UnusedResult: Forms.Driver_Result := Unknown_Request;
   begin
      Set_Cursor_Visibility(Visibility);
      Create_Fields.all(1) := New_Field(1, 30, 0, 8, 0, 0);
      Set_Buffer
        (Create_Fields.all(1), 0, "Enter a new " & Create_Type & " name:");
      FieldOptions := Get_Options(Create_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Create_Fields.all(1), FieldOptions);
      Create_Fields.all(2) := New_Field(1, 40, 1, 0, 0, 0);
      Set_Buffer(Create_Fields.all(2), 0, "");
      FieldOptions := Get_Options(Create_Fields.all(2));
      FieldOptions.Auto_Skip := False;
      Set_Options(Create_Fields.all(2), FieldOptions);
      Create_Fields.all(3) := New_Field(1, 8, 2, 7, 0, 0);
      Set_Buffer(Create_Fields.all(3), 0, "[Cancel]");
      FieldOptions := Get_Options(Create_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(3), FieldOptions);
      Create_Fields.all(4) := New_Field(1, 8, 2, 23, 0, 0);
      FieldOptions := Get_Options(Create_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Create_Fields.all(4), FieldOptions);
      Set_Buffer(Create_Fields.all(4), 0, "[Create]");
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
   end ShowCreateForm;

   function Create_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(DialogForm, F_Previous_Field);
            Result := Driver(DialogForm, F_End_Line);
         when 66 | KEY_DOWN =>
            Result := Driver(DialogForm, F_Next_Field);
            Result := Driver(DialogForm, F_End_Line);
         when 127 =>
            Result := Driver(DialogForm, F_Delete_Previous);
         when 10 =>
            if FieldIndex = 4 then
               Tcl_Eval
                 (Interpreter,
                  "CreateItem " &
                  Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
               if Tcl_GetResult(Interpreter) = "0" then
                  return MESSAGE_FORM;
               end if;
            end if;
            if FieldIndex /= 2 then
               Set_Cursor_Visibility(Visibility);
               Post(DialogForm, False);
               Delete(DialogForm);
               UpdateDirectoryList(True);
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
      return CREATE_FORM;
   end Create_Keys;

end CreateItems;
