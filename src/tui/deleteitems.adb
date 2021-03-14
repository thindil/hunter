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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body DeleteItems is

   function DeleteSelected return Boolean is
      GoUp, Success: Boolean := False;
      Arguments: Argument_List := (new String'("-rf"), new String'(""));
      OldSetting: Boolean;
      DeleteTime: String(1 .. 19);
      procedure MoveToTrash(Name: Unbounded_String) is
         NewName: Unbounded_String;
         TrashFile: File_Type;
      begin
         NewName :=
           Trim
             (To_Unbounded_String
                (Hash_Type'Image
                   (Ada.Strings.Unbounded.Hash
                      (Name & To_Unbounded_String(Image(Clock))))),
              Both);
         Create
           (TrashFile, Out_File,
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/info/" & To_String(NewName) & ".trashinfo");
         Put_Line(TrashFile, "[Trash Info]");
         Put_Line(TrashFile, "Path=" & To_String(Name));
         DeleteTime := Image(Date => Clock, Time_Zone => UTC_Time_Offset);
         DeleteTime(11) := 'T';
         Put_Line(TrashFile, "DeletionDate=" & DeleteTime);
         Close(TrashFile);
         Rename_File
           (To_String(Name),
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/files/" & To_String(NewName),
            Success);
      end MoveToTrash;
      procedure AddTrash(SubDirectory: String) is
         Search: Search_Type;
         Item: Directory_Entry_Type;
      begin
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/" &
            SubDirectory,
            "*");
         Add_Items_To_Trash_Loop :
         while More_Entries(Search) loop
            Get_Next_Entry(Search, Item);
            if Simple_Name(Item) /= "." and Simple_Name(Item) /= ".." then
               SelectedItems.Append
                 (New_Item => To_Unbounded_String(Full_Name(Item)));
            end if;
         end loop Add_Items_To_Trash_Loop;
         End_Search(Search);
      end AddTrash;
   begin
      Create_Path
        (Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/info");
      Create_Path
        (Ada.Environment_Variables.Value("HOME") &
         "/.local/share/Trash/files");
      if New_Action = CLEARTRASH then
         OldSetting := Settings.DeleteFiles;
         Settings.DeleteFiles := True;
         SelectedItems.Clear;
         AddTrash("info");
         AddTrash("files");
      end if;
      Delete_Items_Loop :
      for Item of SelectedItems loop
         UpdateProgressBar;
         if Is_Directory(To_String(Item)) then
            Arguments(2) := new String'(To_String(Item));
            if Settings.DeleteFiles or New_Action = DELETETRASH then
               Spawn(Locate_Exec_On_Path("rm").all, Arguments, Success);
               if not Success then
                  raise Directory_Error with To_String(Item);
               end if;
            else
               MoveToTrash(Item);
            end if;
            if Item = MainWindow.Current_Directory then
               GoUp := True;
            end if;
         else
            if Settings.DeleteFiles or New_Action = DELETETRASH then
               Delete_File(To_String(MainWindow.Current_Directory & "/" & Item));
            else
               MoveToTrash(MainWindow.Current_Directory & "/" & Item);
            end if;
         end if;
         if New_Action = DELETETRASH then
            Delete_File
              (Ada.Environment_Variables.Value("HOME") &
               "/.local/share/Trash/info/" & Simple_Name(To_String(Item)) &
               ".trashinfo");
         end if;
      end loop Delete_Items_Loop;
      if New_Action = CLEARTRASH then
         Settings.DeleteFiles := OldSetting;
      end if;
      SelectedItems.Clear;
      Current_Selected := MainWindow.Current_Directory;
      return GoUp;
   exception
      when An_Exception : Ada.Directories.Use_Error =>
         ShowMessage
           (Mc
              (Interpreter,
               "{Could not delete selected files or directories. Reason:}") &
            " " & Exception_Message(An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         ShowMessage
           (Mc(Interpreter, "{Can't delete selected directory:}") & " " &
            Exception_Message(An_Exception));
         raise;
      when others =>
         ShowMessage
           (Mc
              (Interpreter,
               "{Unknown error during deleting files or directories.}"));
         raise;
   end DeleteSelected;

   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure ShowDeleteForm is
      Delete_Fields: constant Field_Array_Access := new Field_Array(1 .. 3);
      FormHeight: Line_Position;
      FormLength: constant Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      DeleteList: Unbounded_String;
      ListLength: Positive;
      UnusedResult: Forms.Driver_Result;
   begin
      Set_Cursor_Visibility(Visibility);
      if SelectedItems.Length > 10 then
         ListLength := 10;
      else
         ListLength := Positive(SelectedItems.Length);
      end if;
      Set_Delete_List_Loop :
      for I in 1 .. ListLength loop
         if Is_Directory
             (To_String(MainWindow.Current_Directory & "/" & SelectedItems(I))) then
            Append
              (DeleteList, "  " & SelectedItems(I) & "(and its content)" & LF);
         else
            Append(DeleteList, "  " & SelectedItems(I) & LF);
         end if;
      end loop Set_Delete_List_Loop;
      if ListLength = 10 and SelectedItems.Length > 10 then
         ListLength := 11;
         Append(DeleteList, "and more");
      end if;
      Delete_Fields.all(1) :=
        New_Field(1, 8, 1 + Line_Position(ListLength), 7, 0, 0);
      Set_Buffer(Delete_Fields.all(1), 0, "[Cancel]");
      FieldOptions := Get_Options(Delete_Fields.all(1));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(1), FieldOptions);
      Delete_Fields.all(2) :=
        New_Field(1, 8, 1 + Line_Position(ListLength), 23, 0, 0);
      FieldOptions := Get_Options(Delete_Fields.all(2));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(2), FieldOptions);
      Set_Buffer(Delete_Fields.all(2), 0, "[Delete]");
      Delete_Fields.all(3) := Null_Field;
      DialogForm := New_Form(Delete_Fields);
      Set_Options(DialogForm, (others => False));
      FormHeight := Line_Position(ListLength) + 2;
      FormWindow :=
        Create
          (FormHeight + 2, 34, ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm, Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      if Settings.DeleteFiles or New_Action = DELETETRASH then
         Add(FormWindow, 1, 1, "Delete?");
      else
         Add(FormWindow, 1, 1, "Move to Trash?");
      end if;
      Add(FormWindow, 2, 0, To_String(DeleteList));
      UnusedResult := Driver(DialogForm, F_First_Field);
      Box(FormWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(FormWindow);
   end ShowDeleteForm;

   function Delete_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(DialogForm, F_Previous_Field);
         when 66 | KEY_DOWN =>
            Result := Driver(DialogForm, F_Next_Field);
         when 10 =>
            if FieldIndex = 2 then
               if not DeleteSelected then
                  LoadDirectory(To_String(MainWindow.Current_Directory));
               else
                  LoadDirectory
                    (Ada.Directories.Containing_Directory
                       (To_String(MainWindow.Current_Directory)));
               end if;
            end if;
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            UpdateDirectoryList(True);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
      return DELETE_FORM;
   end Delete_Keys;

end DeleteItems;
