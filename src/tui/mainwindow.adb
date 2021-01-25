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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl.Ada; use Tcl.Ada;
with ActivateItems;
with CreateItems; use CreateItems;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Modules; use Modules;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;

package body MainWindow is

   ListWindow: Window;
   PathButtons: Window;
   Path: Menu;
   ProgramMenu: Menu;
   MenuWindow: Window;
   SubMenuWindow: Window;
   SubMenu: Menu;
   DialogForm: Forms.Form;
   FormWindow: Window;

   procedure CreateMainWindow(Directory: String; Interp: Tcl_Interp) is
      Main_Menu_Array: constant array(1 .. 6) of Unbounded_String :=
        (To_Unbounded_String("Quit"), To_Unbounded_String("Bookmarks"),
         To_Unbounded_String("View"), To_Unbounded_String("Actions"),
         To_Unbounded_String("About"), To_Unbounded_String("Selected"));
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 7);
   begin
      Interpreter := Interp;
      ActivateItems.AddCommands;
      Create_Program_Menu_Loop:
      for I in Main_Menu_Array'Range loop
         Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
      end loop Create_Program_Menu_Loop;
      Menu_Items.all(7) := Null_Item;
      ProgramMenu := New_Menu(Menu_Items);
      Set_Format(ProgramMenu, 1, 6);
      Set_Mark(ProgramMenu, "");
      MenuWindow := Create(1, Columns, 0, 0);
      Set_Window(ProgramMenu, MenuWindow);
      Set_Sub_Window
        (ProgramMenu, Derived_Window(MenuWindow, 1, Columns, 0, 0));
      Post(ProgramMenu);
      PathButtons := Create(1, Columns / 2, 1, 0);
      ListWindow :=
        (if Settings.ShowPreview then Create(Lines - 2, Columns / 2, 2, 0)
         else Create(Lines - 2, Columns, 2, 0));
      Box(ListWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(MenuWindow);
      Refresh(ListWindow);
      CreateShowItemsUI;
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists(To_String(CurrentDirectory)) then
            CurrentDirectory := To_Unbounded_String("/");
         end if;
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      StartTimer(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
   end CreateMainWindow;

   procedure UpdateDirectoryList(Clear: Boolean := False) is
      Menu_Items: constant Item_Array_Access :=
        new Item_Array(ItemsList.First_Index .. ItemsList.Last_Index + 1);
      Index: Positive;
      Path_Items: Item_Array_Access;
      Tokens: Slice_Set;
      CurrentIndex: Positive := 1;
      Item: Unbounded_String;
   begin
      if Clear then
         Terminal_Interface.Curses.Clear(PathButtons);
         CurrentDirectory :=
           To_Unbounded_String
             (Normalize_Pathname(To_String(CurrentDirectory)));
         Index := Count(CurrentDirectory, "/") + 1;
         if CurrentDirectory /= To_Unbounded_String("/") then
            Path_Items := new Item_Array(1 .. Index + 1);
            Path_Items.all(1) := New_Item("/");
            Create(Tokens, To_String(CurrentDirectory), "/");
            for I in 2 .. Slice_Count(Tokens) loop
               Path_Items.all(Positive(I)) := New_Item(Slice(Tokens, I));
            end loop;
            Path_Items.all(Index + 1) := Null_Item;
         else
            Path_Items := new Item_Array(1 .. 2);
            Path_Items.all(1) := New_Item("/");
            Path_Items.all(2) := Null_Item;
            Index := 1;
         end if;
         Path := New_Menu(Path_Items);
         Set_Format(Path, 1, 5);
         Set_Mark(Path, "");
         Set_Window(Path, PathButtons);
         Set_Sub_Window
           (Path, Derived_Window(PathButtons, 1, (Columns / 2) - 2, 0, 1));
         Post(Path);
         Set_Current(Path, Path_Items.all(Index));
         Terminal_Interface.Curses.Clear(ListWindow);
         Box(ListWindow, Default_Character, Default_Character);
         Add(ListWindow, 1, 10, "Name");
         Index := ItemsList.First_Index;
         Load_Directory_View_Loop:
         for I in ItemsList.First_Index .. ItemsList.Last_Index loop
            if not Settings.ShowHidden and ItemsList(I).IsHidden then
               goto End_Of_Loop;
            end if;
            Menu_Items.all(Index) := New_Item(To_String(ItemsList(I).Name));
            Item := CurrentDirectory & "/" & ItemsList(I).Name;
            if Item = CurrentSelected then
               CurrentIndex := Index;
            end if;
            Index := Index + 1;
            <<End_Of_Loop>>
         end loop Load_Directory_View_Loop;
         Fill_Empty_Entries_Loop:
         for I in Index .. Menu_Items'Last loop
            Menu_Items.all(I) := Null_Item;
         end loop Fill_Empty_Entries_Loop;
         DirectoryList := New_Menu(Menu_Items);
         Switch_Options(DirectoryList, (One_Valued => False, others => <>));
         Set_Format(DirectoryList, Lines - 5, 1);
         Set_Mark(DirectoryList, "");
         Set_Window(DirectoryList, ListWindow);
         Set_Sub_Window
           (DirectoryList,
            Derived_Window(ListWindow, Lines - 5, (Columns / 2) - 2, 2, 1));
         Post(DirectoryList);
         Set_Current(DirectoryList, Menu_Items.all(CurrentIndex));
         Refresh;
         Refresh(PathButtons);
         Refresh(ListWindow);
         Show_Selected;
      end if;
   end UpdateDirectoryList;

   procedure Directory_Keys(Key: Key_Code) is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(DirectoryList, M_Up_Item);
         when 66 | KEY_DOWN =>
            Result := Driver(DirectoryList, M_Down_Item);
         when 32 =>
            Result := Driver(DirectoryList, M_Toggle_Item);
            Result := Driver(DirectoryList, M_Down_Item);
         when 72 | KEY_HOME =>
            Result := Driver(DirectoryList, M_First_Item);
         when 70 | KEY_END =>
            Result := Driver(DirectoryList, M_Last_Item);
         when 53 | KEY_NPAGE =>
            Result := Driver(DirectoryList, M_ScrollUp_Page);
         when 54 | KEY_PPAGE =>
            Result := Driver(DirectoryList, M_ScrollDown_Page);
         when 10 =>
            Tcl_Eval(Interpreter, "ActivateItem");
         when others =>
            null;
      end case;
      if Result = MENU_OK then
         Refresh(ListWindow);
         Show_Selected;
      end if;
   end Directory_Keys;

   function Path_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when 68 | KEY_LEFT =>
            Result := Driver(Path, M_Previous_Item);
         when 67 | KEY_RIGHT =>
            Result := Driver(Path, M_Next_Item);
         when 72 | KEY_HOME =>
            Result := Driver(Path, M_First_Item);
         when 70 | KEY_END =>
            Result := Driver(Path, M_Last_Item);
         when 10 =>
            CurrentDirectory := To_Unbounded_String("/");
            Update_Current_Directory_Loop:
            for I in 2 .. Get_Index(Current(Path)) loop
               Append(CurrentDirectory, Name(Items(Path, I)));
               if I < Get_Index(Current(Path)) then
                  Append(CurrentDirectory, "/");
               end if;
            end loop Update_Current_Directory_Loop;
            LoadDirectory(To_String(CurrentDirectory));
            UpdateDirectoryList(True);
            UpdateWatch(To_String(CurrentDirectory));
            Execute_Modules(On_Enter, "{" & To_String(CurrentDirectory) & "}");
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = MENU_OK then
         Refresh(PathButtons);
      end if;
      return PATH_BUTTONS;
   end Path_Keys;

   procedure Draw_Menu(Menu_Type: UI_Locations) is
      Menu_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      case Menu_Type is
         when ACTIONS_MENU =>
            Menu_Items := new Item_Array(1 .. 5);
            Menu_Items.all(1) := New_Item("Create new directory");
            Menu_Items.all(2) := New_Item("Create new file");
            Menu_Items.all(3) := New_Item("Delete selected");
            Menu_Items.all(4) := New_Item("Close");
            Menu_Items.all(5) := Null_Item;
         when others =>
            null;
      end case;
      SubMenu := New_Menu(Menu_Items);
      Set_Format(SubMenu, Lines - 5, 1);
      Set_Mark(SubMenu, "");
      Scale(SubMenu, MenuHeight, MenuLength);
      SubMenuWindow :=
        Create(MenuHeight + 2, MenuLength + 2, Lines / 3, Columns / 3);
      Set_Window(SubMenu, SubMenuWindow);
      Set_Sub_Window
        (SubMenu, Derived_Window(SubMenuWindow, MenuHeight, MenuLength, 1, 1));
      Box(SubMenuWindow, Default_Character, Default_Character);
      Post(SubMenu);
      Refresh;
      Refresh(SubMenuWindow);
   end Draw_Menu;

   function Menu_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(ProgramMenu));
   begin
      case Key is
         when 68 | KEY_LEFT =>
            Result := Driver(ProgramMenu, M_Previous_Item);
         when 67 | KEY_RIGHT =>
            Result := Driver(ProgramMenu, M_Next_Item);
         when 72 | KEY_HOME =>
            Result := Driver(ProgramMenu, M_First_Item);
         when 70 | KEY_END =>
            Result := Driver(ProgramMenu, M_Last_Item);
         when 10 =>
            case CurrentIndex is
               when 1 =>
                  return PATH_BUTTONS;
               when 4 =>
                  Draw_Menu(ACTIONS_MENU);
                  return ACTIONS_MENU;
               when others =>
                  return MAIN_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = MENU_OK then
         Refresh(MenuWindow);
      end if;
      return MAIN_MENU;
   end Menu_Keys;

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

   procedure ShowDeleteForm is
      Delete_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
      DeleteList: Unbounded_String;
      ListLength: Positive;
   begin
      Set_Cursor_Visibility(Visibility);
      Delete_Fields.all(1) := New_Field(1, 30, 0, 8, 0, 0);
      if Settings.DeleteFiles or NewAction = DELETETRASH then
         Set_Buffer
            (Delete_Fields.all(1), 0, "Delete?");
      else
         Set_Buffer
            (Delete_Fields.all(1), 0, "Move to Trash?");
      end if;
      FieldOptions := Get_Options(Delete_Fields.all(1));
      FieldOptions.Active := False;
      Set_Options(Delete_Fields.all(1), FieldOptions);
      if SelectedItems.Length > 10 then
         ListLength := 10;
      else
         ListLength := Positive(SelectedItems.Length);
      end if;
      Set_Delete_List_Loop:
      for I in 1 .. ListLength loop
         Append(DeleteList, SelectedItems(I) & LF);
      end loop Set_Delete_List_Loop;
      if ListLength = 10 and SelectedItems.Length > 10 then
         ListLength := 11;
         Append(DeleteList, "and more");
      end if;
      Delete_Fields.all(2) := New_Field(Line_Position(ListLength), 40, 1, 0, 0, 0);
      Set_Buffer(Delete_Fields.all(2), 0, To_String(DeleteList));
      FieldOptions := Get_Options(Delete_Fields.all(2));
      FieldOptions.Active := False;
      Set_Options(Delete_Fields.all(2), FieldOptions);
      Delete_Fields.all(3) := New_Field(1, 8, 2, 7, 0, 0);
      Set_Buffer(Delete_Fields.all(3), 0, "[Cancel]");
      FieldOptions := Get_Options(Delete_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(3), FieldOptions);
      Delete_Fields.all(4) := New_Field(1, 8, 2, 23, 0, 0);
      FieldOptions := Get_Options(Delete_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Delete_Fields.all(4), FieldOptions);
      Set_Buffer(Delete_Fields.all(4), 0, "[Delete]");
      Delete_Fields.all(5) := Null_Field;
      DialogForm := New_Form(Delete_Fields);
      Set_Current(DialogForm, Delete_Fields(3));
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
      Refresh;
      Refresh(FormWindow);
   end ShowDeleteForm;

   function Actions_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(SubMenu));
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(SubMenu, M_Up_Item);
         when 66 | KEY_DOWN =>
            Result := Driver(SubMenu, M_Down_Item);
         when 72 | KEY_HOME =>
            Result := Driver(SubMenu, M_First_Item);
         when 70 | KEY_END =>
            Result := Driver(SubMenu, M_Last_Item);
         when 10 =>
            Post(SubMenu, False);
            Delete(SubMenu);
            UpdateDirectoryList(True);
            case CurrentIndex is
               when 1 =>
                  NewAction := CREATEDIRECTORY;
                  ShowCreateForm("directory");
                  return CREATE_FORM;
               when 2 =>
                  NewAction := CREATEFILE;
                  ShowCreateForm("file");
                  return CREATE_FORM;
               when 3 =>
                  NewAction := (if NewAction /= SHOWTRASH then DELETE else DELETETRASH);
                  ShowDeleteForm;
                  return DELETE_FORM;
               when 4 =>
                  return DIRECTORY_VIEW;
               when others =>
                  return ACTIONS_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = MENU_OK then
         Refresh(SubMenuWindow);
      end if;
      return ACTIONS_MENU;
   end Actions_Keys;

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
               Create_Item(Trim(Get_Buffer(Fields(DialogForm, 2)), Both));
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
      if Result = Form_OK then
         Refresh(FormWindow);
      end if;
      return CREATE_FORM;
   end Create_Keys;

   function Delete_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      --FieldIndex: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(DialogForm, F_Previous_Field);
         when 66 | KEY_DOWN =>
            Result := Driver(DialogForm, F_Next_Field);
         when 10 =>
            Set_Cursor_Visibility(Visibility);
            Post(DialogForm, False);
            Delete(DialogForm);
            UpdateDirectoryList(True);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Form_OK then
         Refresh(FormWindow);
      end if;
      return DELETE_FORM;
   end Delete_Keys;

end MainWindow;
