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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body DeleteItems.UI is

   -- ****iv* DeleteItemsTUI/DeleteItemsTUI.Dialog_Form
   -- FUNCTION
   -- The form to show delete items confirmation dialog
   -- SOURCE
   Dialog_Form: Forms.Form;
   -- ****

   -- ****iv* DeleteItemsTUI/DeleteItemsTUI.Form_Window
   -- FUNCTION
   -- The window to show the form with confirmation of deleting items
   -- SOURCE
   Form_Window: Window;
   -- ****

   procedure Show_Delete_Form is
      Delete_Fields: constant Field_Array_Access := new Field_Array(1 .. 3);
      Form_Height: Line_Position := 0;
      Form_Length: Column_Position := 32;
      Visibility: Cursor_Visibility := Normal;
      Field_Options: Field_Option_Set := Forms.Default_Field_Options;
      Delete_List: Unbounded_String;
      List_Length: Positive;
      Unused_Result: Forms.Driver_Result := Forms.Form_Ok;
      File_Line: Unbounded_String := Null_Unbounded_String;
      File_Info: File_Type;
   begin
      if Selected_Items.Length = 0 then
         return;
      end if;
      Set_Cursor_Visibility(Visibility => Visibility);
      if Settings.Delete_Files or New_Action = DELETETRASH then
         Delete_List :=
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interpreter, Src_String => "{Delete?}") & LF &
                LF);
      else
         Delete_List :=
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interpreter, Src_String => "{Move to Trash?}") &
                LF);
      end if;
      if Selected_Items.Length > 10 then
         List_Length := 10;
      else
         List_Length := Positive(Selected_Items.Length);
      end if;
      --## rule off SIMPLIFIABLE_STATEMENTS
      Set_Delete_List_Loop :
      for I in 1 .. List_Length loop
         if New_Action = DELETE then
            if Is_Directory(Name => To_String(Source => Selected_Items(I))) then
               if Simple_Name(Name => To_String(Source => Selected_Items(I)))'
                   Length >
                 11 then
                  Append
                    (Source => Delete_List,
                     New_Item => "  " &
                     Simple_Name
                       (Name => To_String(Source => Selected_Items(I)))
                       (1 .. 11) &
                     "..." &
                     Mc(Interp => Interpreter,
                        Src_String => "{(and its content)}") &
                     LF);
               else
                  Append
                    (Source => Delete_List,
                     New_Item => "  " &
                     Simple_Name
                       (Name => To_String(Source => Selected_Items(I))) &
                     " " &
                     Mc(Interp => Interpreter,
                        Src_String => "{(and its content)}") &
                     LF);
               end if;
            elsif Simple_Name(Name => To_String(Source => Selected_Items(I)))'
                Length >
              27 then
               Append
                 (Source => Delete_List,
                  New_Item => "  " &
                  Simple_Name(Name => To_String(Source => Selected_Items(I)))
                    (1 .. 27) &
                  "..." & LF);
            else
               Append
                 (Source => Delete_List,
                  New_Item => "  " &
                  Simple_Name(Name => To_String(Source => Selected_Items(I))) &
                  LF);
            end if;
         else
            Open
              (File => File_Info, Mode => In_File,
               Name => Ada.Environment_Variables.Value(Name => "HOME") &
               "/.local/share/Trash/info/" &
               Simple_Name(Name => To_String(Source => Selected_Items(I))) & ".trashinfo");
            Skip_Line(File => File_Info);
            Get_Item_Name_Loop :
            for J in 1 .. 2 loop
               File_Line := To_Unbounded_String(Source => Get_Line(File => File_Info));
               if Slice(File_Line, 1, 4) = "Path" then
                  Append
                    (Delete_List,
                     "  " &
                     Simple_Name(Slice(File_Line, 6, Length(File_Line))));
               end if;
            end loop Get_Item_Name_Loop;
            Close(File_Info);
         end if;
      end loop Set_Delete_List_Loop;
      --## rule on SIMPLIFIABLE_STATEMENTS
      if List_Length = 10 and Selected_Items.Length > 10 then
         List_Length := 11;
         Append
           (Delete_List,
            " " & Mc(Interp => Interpreter, Src_String => "{(and more)}"));
      end if;
      List_Length := List_Length + 2;
      Delete_Fields.all(1) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Cancel}", Interpreter)) + 2,
           1 + Line_Position(List_Length), 7, 0, 0);
      Set_Buffer
        (Delete_Fields.all(1), 0, "[" & Mc(Interpreter, "{Cancel}") & "]");
      Field_Options := Get_Options(Delete_Fields.all(1));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Delete_Fields.all(1), Field_Options);
      Delete_Fields.all(2) :=
        New_Field
          (1, Column_Position'Value(Mc_Max("{Delete}", Interpreter)) + 2,
           1 + Line_Position(List_Length), 23, 0, 0);
      Field_Options := Get_Options(Delete_Fields.all(2));
      Field_Options.Edit := False; --## rule line off ASSIGNMENTS
      Set_Options(Delete_Fields.all(2), Field_Options);
      Set_Buffer
        (Delete_Fields.all(2), 0, "[" & Mc(Interpreter, "{Delete}") & "]");
      Delete_Fields.all(3) := Null_Field;
      Form_Height := Line_Position(List_Length) + 2;
      if Form_Height = 2 then
         return;
      end if;
      Dialog_Form := New_Form(Delete_Fields);
      Create_Dialog(Dialog_Form, Form_Window, Form_Height, Form_Length);
      Add(Form_Window, 1, 2, To_String(Delete_List));
      Box(Form_Window, Default_Character, Default_Character);
      Unused_Result := Driver(Dialog_Form, F_First_Field);
      Refresh;
      Refresh(Form_Window);
   end Show_Delete_Form;

   function Delete_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Positive := Get_Index(Current(Dialog_Form));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_UP =>
            Result := Go_Previous_Field(Dialog_Form);
         when KEY_DOWN =>
            Result := Go_Next_Field(Dialog_Form);
         when 27 =>
            if New_Action = DELETETRASH then
               New_Action := SHOWTRASH;
            end if;
            Show_Preview;
            Set_Cursor_Visibility(Visibility);
            Delete_Dialog(Dialog_Form, True);
            return DIRECTORY_VIEW;
         when 10 =>
            if FieldIndex = 2 then
               if not Delete_Selected(Interpreter) then
                  Load_Directory(To_String(Common.Current_Directory));
               else
                  Load_Directory
                    (Ada.Directories.Containing_Directory
                       (To_String(Common.Current_Directory)));
               end if;
               Current_Selected := Common.Current_Directory;
               if Items_List.Length > 0 then
                  Current_Selected :=
                    Current_Selected & "/" & Items_List(1).Name;
               end if;
            end if;
            if New_Action = DELETETRASH then
               New_Action := SHOWTRASH;
            end if;
            Show_Preview;
            Set_Cursor_Visibility(Visibility);
            Delete_Dialog(Dialog_Form, True);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Form_Ok then
         Refresh(Form_Window);
      end if;
      return DELETE_FORM;
   end Delete_Keys;

end DeleteItems.UI;
