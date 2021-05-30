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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;

package body Preferences.UI is

   OptionsMenu: Menu;
   MenuWindow: Window;

   procedure Show_Options is
      Main_Menu_Array: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("Preferences"), To_Unbounded_String("Shortcuts"),
         To_Unbounded_String("Commands"), To_Unbounded_String("Modules"),
         To_Unbounded_String("Close"));
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 6);
   begin
      Clear;
      Create_Program_Menu_Loop :
      for I in Main_Menu_Array'Range loop
         Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
      end loop Create_Program_Menu_Loop;
      Menu_Items.all(6) := Null_Item;
      OptionsMenu := New_Menu(Menu_Items);
      Set_Format(OptionsMenu, 1, 5);
      Set_Mark(OptionsMenu, "");
      MenuWindow := Create(1, Columns, 0, 0);
      Set_Window(OptionsMenu, MenuWindow);
      Set_Sub_Window
        (OptionsMenu, Derived_Window(MenuWindow, 1, Columns, 0, 0));
      Post(OptionsMenu);
      Refresh(MenuWindow);
   end Show_Options;

   function Preferences_Keys(Key: Key_Code) return UI_Locations is
      pragma Unreferenced(Key);
   begin
      return OPTIONS_VIEW;
   end Preferences_Keys;

end Preferences.UI;
