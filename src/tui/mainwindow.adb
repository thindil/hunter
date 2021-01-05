-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with RefreshData; use RefreshData;

package body MainWindow is

   ListWindow: Window;

   procedure CreateMainWindow(Directory: String) is
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 7);
      ProgramMenu: Menu;
      MenuWindow: Window;
   begin
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
      ListWindow := Create(Lines - 3, Columns / 2, 3, 0);
      Box(ListWindow, Default_Character, Default_Character);
      Refresh;
      Refresh(MenuWindow);
      Refresh(ListWindow);
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
      DirectoryList: Menu;
   begin
      if Clear then
         Terminal_Interface.Curses.Clear(ListWindow);
         Box(ListWindow, Default_Character, Default_Character);
         for I in ItemsList.First_Index .. ItemsList.Last_Index loop
            Menu_Items.all(I) := New_Item(To_String(ItemsList(I).Name));
         end loop;
         Menu_Items.all(Menu_Items'Last) := Null_Item;
         DirectoryList := New_Menu(Menu_Items);
         Set_Format(DirectoryList, Line_Position(ItemsList.Length), 1);
         Set_Mark(DirectoryList, "");
         Set_Window(DirectoryList, ListWindow);
         Set_Sub_Window
           (DirectoryList,
            Derived_Window(ListWindow, Lines - 5, (Columns / 2) - 2, 1, 1));
         Post(DirectoryList);
         Refresh;
         Refresh(ListWindow);
      end if;
   end UpdateDirectoryList;

end MainWindow;
