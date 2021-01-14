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
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with ActivateItems;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;

package body MainWindow is

   ListWindow: Window;
   PathButtons: Window;

   procedure CreateMainWindow(Directory: String; Interp: Tcl_Interp) is
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 7);
      ProgramMenu: Menu;
      MenuWindow: Window;
   begin
      Interpreter := Interp;
      ActivateItems.AddCommands;
      Menu_Items.all(1) := New_Item("Quit");
      Menu_Items.all(2) := New_Item("Bookmarks");
      Menu_Items.all(3) := New_Item("View");
      Menu_Items.all(4) := New_Item("Actions");
      Menu_Items.all(5) := New_Item("About");
      Menu_Items.all(6) := New_Item("Selected");
      Menu_Items.all(7) := Null_Item;
      ProgramMenu := New_Menu(Menu_Items);
      Set_Format(ProgramMenu, 1, 11);
      Set_Mark(ProgramMenu, "");
      MenuWindow := Create(1, Columns, 0, 0);
      Set_Window(ProgramMenu, MenuWindow);
      Set_Sub_Window
        (ProgramMenu, Derived_Window(MenuWindow, 1, Columns, 0, 0));
      Post(ProgramMenu);
      PathButtons := Create(1, Columns / 2, 2, 0);
      ListWindow :=
        (if Settings.ShowPreview then Create(Lines - 3, Columns / 2, 3, 0)
         else Create(Lines - 3, Columns, 3, 0));
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
      Path: Menu;
      Tokens: Slice_Set;
   begin
      if Clear then
         Terminal_Interface.Curses.Clear(PathButtons);
         if Element(CurrentDirectory, Length(CurrentDirectory)) = '/' then
            Index := Count(CurrentDirectory, "/");
         else
            Index := Count(CurrentDirectory, "/") + 1;
         end if;
         Path_Items := new Item_Array(1 .. Index + 1);
         Path_Items.all(1) := New_Item("/");
         Create(Tokens, To_String(CurrentDirectory), "/");
         for I in 2 .. Index loop
            Path_Items.all(I) := New_Item(Slice(Tokens, Slice_Number(I)));
         end loop;
         Path_Items.all(Index + 1) := Null_Item;
         Path := New_Menu(Path_Items);
         Set_Format(Path, 1, Column_Position(Index));
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
         Set_Format(DirectoryList, Lines - 6, 1);
         Set_Mark(DirectoryList, "");
         Set_Window(DirectoryList, ListWindow);
         Set_Sub_Window
           (DirectoryList,
            Derived_Window(ListWindow, Lines - 6, (Columns / 2) - 2, 2, 1));
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

end MainWindow;
