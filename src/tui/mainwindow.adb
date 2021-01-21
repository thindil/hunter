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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with ActivateItems;
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

   procedure CreateMainWindow(Directory: String; Interp: Tcl_Interp) is
      Main_Menu_Array: constant array(1 .. 6) of Unbounded_String :=
        (To_Unbounded_String("Quit"), To_Unbounded_String("Bookmarks"),
         To_Unbounded_String("View"), To_Unbounded_String("Actions"),
         To_Unbounded_String("About"), To_Unbounded_String("Selected"));
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 7);
   begin
      Interpreter := Interp;
      ActivateItems.AddCommands;
      for I in Main_Menu_Array'Range loop
         Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
      end loop;
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
         for I in ItemsList.First_Index .. ItemsList.Last_Index loop
            if not Settings.ShowHidden and ItemsList(I).IsHidden then
               goto End_Of_Loop;
            end if;
            Menu_Items.all(Index) := New_Item(To_String(ItemsList(I).Name));
            Index := Index + 1;
            <<End_Of_Loop>>
         end loop;
         for I in Index .. Menu_Items'Last loop
            Menu_Items.all(I) := Null_Item;
         end loop;
         DirectoryList := New_Menu(Menu_Items);
         Switch_Options(DirectoryList, (One_Valued => False, others => <>));
         Set_Format(DirectoryList, Lines - 5, 1);
         Set_Mark(DirectoryList, "");
         Set_Window(DirectoryList, ListWindow);
         Set_Sub_Window
           (DirectoryList,
            Derived_Window(ListWindow, Lines - 5, (Columns / 2) - 2, 2, 1));
         Post(DirectoryList);
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
            for I in 2 .. Get_Index(Current(Path)) loop
               Append(CurrentDirectory, Name(Items(Path, I)));
               if I < Get_Index(Current(Path)) then
                  Append(CurrentDirectory, "/");
               end if;
            end loop;
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

   function Menu_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
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
            if Get_Index(Current(ProgramMenu)) = 1 then
               return PATH_BUTTONS;
            end if;
         when others =>
            null;
      end case;
      if Result = MENU_OK then
         Refresh(MenuWindow);
      end if;
      return MAIN_MENU;
   end Menu_Keys;

end MainWindow;
