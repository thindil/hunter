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

with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with ActivateItems;
with Bookmarks; use Bookmarks;
with CreateItems; use CreateItems;
with CopyItems; use CopyItems;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Modules; use Modules;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with RenameItems; use RenameItems;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body MainWindow is

   ListWindow: Window;
   PathButtons: Window;
   Path: Menu;
   ProgramMenu: Menu;
   MenuWindow: Window;
   SubMenuWindow: Window;
   SubMenu: Menu;

   procedure CreateProgramMenu(Update: Boolean := False) is
   begin
      Terminal_Interface.Curses.Clear(MenuWindow);
      case NewAction is
         when COPY | MOVE =>
            declare
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 4);
            begin
               Menu_Items.all(1) := New_Item("Quit");
               Menu_Items.all(2) :=
                 (if NewAction = COPY then New_Item("Copy selected")
                  else New_Item("Move selected"));
               Menu_Items.all(3) := New_Item("Cancel");
               Menu_Items.all(4) := Null_Item;
               ProgramMenu := New_Menu(Menu_Items);
               Set_Format(ProgramMenu, 1, 3);
               Set_Mark(ProgramMenu, "");
               Set_Window(ProgramMenu, MenuWindow);
               Set_Sub_Window
                 (ProgramMenu, Derived_Window(MenuWindow, 1, Columns, 0, 0));
               Post(ProgramMenu);
            end;
         when others =>
            declare
               Main_Menu_Array: constant array(1 .. 6) of Unbounded_String :=
                 (To_Unbounded_String("Quit"),
                  To_Unbounded_String("Bookmarks"),
                  To_Unbounded_String("View"), To_Unbounded_String("Actions"),
                  To_Unbounded_String("About"),
                  To_Unbounded_String("Selected"));
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 7);
            begin
               Create_Program_Menu_Loop :
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
            end;
      end case;
      if Update then
         Refresh(MenuWindow);
      end if;
   end CreateProgramMenu;

   procedure CreateMainWindow(Directory: String) is
   begin
      ActivateItems.AddCommands;
      CreateItems.AddCommands;
      RenameItems.AddCommands;
      MenuWindow := Create(1, Columns, 0, 0);
      CreateProgramMenu;
      PathButtons := Create(1, Columns / 2, 1, 0);
      ListWindow :=
        (if Settings.ShowPreview then Create(Lines - 2, Columns / 2, 2, 0)
         else Create(Lines - 2, Columns, 2, 0));
      Box(ListWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(MenuWindow);
      Refresh(ListWindow);
      CreateShowItemsUI;
      Create_Bookmarks_List;
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
      Width: Column_Position;
      Height: Line_Position;
   begin
      Terminal_Interface.Curses.Clear(PathButtons);
      CurrentDirectory :=
        To_Unbounded_String(Normalize_Pathname(To_String(CurrentDirectory)));
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
      Get_Size(ListWindow, Height, Width);
      Set_Sub_Window(Path, Derived_Window(PathButtons, 1, Width - 2, 0, 1));
      Post(Path);
      Set_Current(Path, Path_Items.all(Index));
      Terminal_Interface.Curses.Clear(ListWindow);
      Box(ListWindow, Default_Character, Default_Character);
      Add(ListWindow, 1, 10, "Name");
      Add(ListWindow, 1, Width - 10, "Size");
      Index := ItemsList.First_Index;
      declare
         Item_Entry: String(1 .. Positive(Width - 2));
      begin
         Load_Directory_View_Loop :
         for I in ItemsList.First_Index .. ItemsList.Last_Index loop
            if not Settings.ShowHidden and ItemsList(I).IsHidden then
               goto End_Of_Loop;
            end if;
            Move(To_String(ItemsList(I).Name), Item_Entry);
            Item := CurrentDirectory & "/" & ItemsList(I).Name;
            case ItemsList(I).Size is
               when -2 =>
                  Overwrite(Item_Entry, Item_Entry'Last - 8, "->");
               when -1 =>
                  Overwrite(Item_Entry, Item_Entry'Last - 8, "unknown");
               when others =>
                  if not ItemsList(I).IsDirectory then
                     Overwrite
                       (Item_Entry, Item_Entry'Last - 8,
                        CountFileSize
                          (Ada.Directories.File_Size(ItemsList(I).Size)));
                  else
                     if Settings.ShowHidden then
                        Overwrite
                          (Item_Entry, Item_Entry'Last - 8,
                           Item_Size'Image
                             (ItemsList(I).Size +
                              Item_Size(ItemsList(I).HiddenItems)));
                     else
                        Overwrite
                          (Item_Entry, Item_Entry'Last - 8,
                           Item_Size'Image(ItemsList(I).Size));
                     end if;
                  end if;
            end case;
            Menu_Items.all(Index) :=
              New_Item(Item_Entry, To_String(ItemsList(I).Name));
            if Item = CurrentSelected then
               CurrentIndex := Index;
            end if;
            Index := Index + 1;
            <<End_Of_Loop>>
         end loop Load_Directory_View_Loop;
      end;
      if Index > ItemsList.First_Index then
         Fill_Empty_Entries_Loop :
         for I in Index .. Menu_Items'Last loop
            Menu_Items.all(I) := Null_Item;
         end loop Fill_Empty_Entries_Loop;
         DirectoryList := New_Menu(Menu_Items);
         Set_Options(DirectoryList, (One_Valued => False, others => <>));
         Set_Format(DirectoryList, Lines - 5, 1);
         Set_Mark(DirectoryList, "");
         Set_Window(DirectoryList, ListWindow);
         Set_Sub_Window
           (DirectoryList,
            Derived_Window(ListWindow, Lines - 5, (Columns / 2) - 2, 2, 1));
         Post(DirectoryList);
         Set_Current(DirectoryList, Menu_Items.all(CurrentIndex));
         if not Clear then
            Update_Selected_Loop :
            for I in 1 .. Item_Count(DirectoryList) loop
               if SelectedItems.Contains
                   (To_Unbounded_String(Name(Items(DirectoryList, I)))) then
                  Set_Value(Items(DirectoryList, I), True);
               end if;
            end loop Update_Selected_Loop;
         end if;
         Show_Selected;
      end if;
      Refresh;
      Refresh(PathButtons);
      Refresh(ListWindow);
   end UpdateDirectoryList;

   function Directory_Keys(Key: Key_Code) return UI_Locations is
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
         when 72 | Key_Home =>
            Result := Driver(DirectoryList, M_First_Item);
         when 70 | Key_End =>
            Result := Driver(DirectoryList, M_Last_Item);
         when 53 | KEY_NPAGE =>
            Result := Driver(DirectoryList, M_ScrollUp_Page);
         when 54 | KEY_PPAGE =>
            Result := Driver(DirectoryList, M_ScrollDown_Page);
         when 10 =>
            Tcl_Eval(Interpreter, "ActivateItem");
            if Tcl_GetResult(Interpreter) = "0" then
               return MESSAGE_FORM;
            end if;
            return DIRECTORY_VIEW;
         when others =>
            return DIRECTORY_VIEW;
      end case;
      if Result = Menu_Ok then
         Refresh(ListWindow);
         if NewAction /= COPY then
            Show_Selected;
         end if;
      end if;
      return DIRECTORY_VIEW;
   end Directory_Keys;

   function Path_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when 68 | KEY_LEFT =>
            Result := Driver(Path, M_Previous_Item);
         when 67 | KEY_RIGHT =>
            Result := Driver(Path, M_Next_Item);
         when 72 | Key_Home =>
            Result := Driver(Path, M_First_Item);
         when 70 | Key_End =>
            Result := Driver(Path, M_Last_Item);
         when 10 =>
            CurrentDirectory := To_Unbounded_String("/");
            Update_Current_Directory_Loop :
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
      if Result = Menu_Ok then
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
            Menu_Items := new Item_Array(1 .. 8);
            Menu_Items.all(1) := New_Item("Create new directory");
            Menu_Items.all(2) := New_Item("Create new file");
            Menu_Items.all(3) := New_Item("Rename selected");
            Menu_Items.all(4) := New_Item("Start copying");
            Menu_Items.all(5) := New_Item("Start moving");
            Menu_Items.all(6) := New_Item("Delete selected");
            Menu_Items.all(7) := New_Item("Close");
            Menu_Items.all(8) := Null_Item;
         when BOOKMARKS_MENU =>
            Menu_Items := Show_Bookmarks_Menu;
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
      OverwriteItem: Boolean := False;
   begin
      case Key is
         when 68 | KEY_LEFT =>
            Result := Driver(ProgramMenu, M_Previous_Item);
         when 67 | KEY_RIGHT =>
            Result := Driver(ProgramMenu, M_Next_Item);
         when 72 | Key_Home =>
            Result := Driver(ProgramMenu, M_First_Item);
         when 70 | Key_End =>
            Result := Driver(ProgramMenu, M_Last_Item);
         when 10 =>
            case CurrentIndex is
               when 1 =>
                  return PATH_BUTTONS;
               when 2 =>
                  if NewAction = COPY then
                     CopyItemsList.Clear;
                     if Item_Count(DirectoryList) > 0 then
                        Update_Copy_Items_Loop :
                        for I in 1 .. Item_Count(DirectoryList) loop
                           if Value(Items(DirectoryList, I)) or
                             Current(DirectoryList) =
                               Items(DirectoryList, I) then
                              CopyItemsList.Append
                                (CurrentDirectory & "/" &
                                 Description(Items(DirectoryList, I)));
                           end if;
                        end loop Update_Copy_Items_Loop;
                     end if;
                     if CopySelected(OverwriteItem) = DIRECTORY_VIEW then
                        NewAction := CREATEFILE;
                        CreateProgramMenu;
                        Refresh(MenuWindow);
                        return DIRECTORY_VIEW;
                     else
                        return MESSAGE_FORM;
                     end if;
                  elsif NewAction = MOVE then
                     MoveItemsList.Clear;
                     if Item_Count(DirectoryList) > 0 then
                        Update_Move_Items_Loop :
                        for I in 1 .. Item_Count(DirectoryList) loop
                           if Value(Items(DirectoryList, I)) or
                             Current(DirectoryList) =
                               Items(DirectoryList, I) then
                              MoveItemsList.Append
                                (CurrentDirectory & "/" &
                                 Description(Items(DirectoryList, I)));
                           end if;
                        end loop Update_Move_Items_Loop;
                     end if;
                     if MoveSelected(OverwriteItem) = DIRECTORY_VIEW then
                        NewAction := CREATEFILE;
                        CreateProgramMenu;
                        Refresh(MenuWindow);
                        return DIRECTORY_VIEW;
                     else
                        return MESSAGE_FORM;
                     end if;
                  else
                     Draw_Menu(BOOKMARKS_MENU);
                     return BOOKMARKS_MENU;
                  end if;
               when 3 =>
                  if NewAction in COPY | MOVE then
                     NewAction := CREATEFILE;
                     UpdateDirectoryList;
                     CreateProgramMenu;
                     Refresh(MenuWindow);
                     return DIRECTORY_VIEW;
                  end if;
               when 4 =>
                  Draw_Menu(ACTIONS_MENU);
                  return ACTIONS_MENU;
               when others =>
                  return MAIN_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow);
      end if;
      return MAIN_MENU;
   end Menu_Keys;

   function Actions_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(SubMenu));
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(SubMenu, M_Up_Item);
         when 66 | KEY_DOWN =>
            Result := Driver(SubMenu, M_Down_Item);
         when 72 | Key_Home =>
            Result := Driver(SubMenu, M_First_Item);
         when 70 | Key_End =>
            Result := Driver(SubMenu, M_Last_Item);
         when 10 =>
            Post(SubMenu, False);
            Delete(SubMenu);
            UpdateDirectoryList;
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
                  NewAction := RENAME;
                  ShowRenameForm;
                  return RENAME_FORM;
               when 4 =>
                  NewAction := COPY;
                  CreateProgramMenu;
                  Refresh(MenuWindow);
                  DestinationDirectory := CurrentDirectory;
                  SecondItemsList := ItemsList;
                  ShowDestination;
                  return DESTINATION_VIEW;
               when 5 =>
                  NewAction := MOVE;
                  CreateProgramMenu;
                  Refresh(MenuWindow);
                  DestinationDirectory := CurrentDirectory;
                  SecondItemsList := ItemsList;
                  ShowDestination;
                  return DESTINATION_VIEW;
               when 6 =>
                  NewAction :=
                    (if NewAction /= SHOWTRASH then DELETE else DELETETRASH);
                  ShowDeleteForm;
                  return DELETE_FORM;
               when 7 =>
                  return DIRECTORY_VIEW;
               when others =>
                  return ACTIONS_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(SubMenuWindow);
      end if;
      return ACTIONS_MENU;
   end Actions_Keys;

   function Bookmarks_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      -- Bookmark: constant String := Name(Current(SubMenu));
   begin
      case Key is
         when 65 | KEY_UP =>
            Result := Driver(SubMenu, M_Up_Item);
         when 66 | KEY_DOWN =>
            Result := Driver(SubMenu, M_Down_Item);
         when 72 | Key_Home =>
            Result := Driver(SubMenu, M_First_Item);
         when 70 | Key_End =>
            Result := Driver(SubMenu, M_Last_Item);
         when 10 =>
            Post(SubMenu, False);
            Delete(SubMenu);
            UpdateDirectoryList;
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(SubMenuWindow);
      end if;
      return BOOKMARKS_MENU;
   end Bookmarks_Keys;

end MainWindow;
