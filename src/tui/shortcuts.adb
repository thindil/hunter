-- Copyright (c) 2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Common; use Common;
with DeleteItems.UI; use DeleteItems.UI;
with LoadData; use LoadData;
with Preferences; use Preferences;
with RenameItems.UI; use RenameItems.UI;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;

package body Shortcuts is

   function Shortcuts_Keys
     (Key: Key_Code; AltKey: Boolean; Old_Location: UI_Locations)
      return UI_Locations is
      Key_Value: constant String := Key_Name(Key);
      New_Key: Unbounded_String := Null_Unbounded_String;
      Index: Natural := 0;
   begin
      if AltKey then
         New_Key := To_Unbounded_String("Alt-" & Key_Value);
      else
         if Key_Value(Key_Value'First) /= '^' then
            New_Key := To_Unbounded_String(Key_Value);
         else
            New_Key :=
              To_Unbounded_String
                ("Control-" & To_Lower(Key_Value(Key_Value'Last)));
         end if;
      end if;
      for I in Accelerators'Range loop
         if Accelerators(I) = New_Key then
            Index := I;
            exit;
         end if;
      end loop;
      case Index is
         when 1 =>
            return QUIT_PROGRAM;
         when 2 =>
            Draw_Menu(BOOKMARKS_MENU);
            return BOOKMARKS_MENU;
         when 3 =>
            ShowSearchForm;
            return SEARCH_FORM;
         when 5 =>
            New_Action := DELETE;
            Show_Delete_Form;
            return DELETE_FORM;
         when 6 =>
            Draw_Menu(ABOUT_MENU);
            return ABOUT_MENU;
         when 8 =>
            if Natural(Selected_Items.Length) = Item_Count(DirectoryList) then
               Selected_Items.Clear;
            else
               Update_Selected_Items_Loop :
               for I in 1 .. Item_Count(DirectoryList) loop
                  Selected_Items.Append
                    (To_Unbounded_String
                       (Description(Items(DirectoryList, I))));
               end loop Update_Selected_Items_Loop;
            end if;
            Update_Directory_List;
         when 9 =>
            New_Action := RENAME;
            ShowRenameForm;
            return RENAME_FORM;
         when 10 | 11 =>
            if New_Action not in COPY | MOVE then
               if Index = 10 then
                  New_Action := COPY;
               else
                  New_Action := MOVE;
               end if;
               UILocation := DESTINATION_VIEW;
               Destination_Directory := Common.Current_Directory;
               Second_Items_List := Items_List;
            else
               New_Action := Default_Item_Action;
               UILocation := DIRECTORY_VIEW;
            end if;
            CreateProgramMenu(Update => True);
            if New_Action in COPY | MOVE then
               Update_Directory_List;
               ShowDestination;
            else
               Update_Directory_List(Clear => True);
               Show_Preview;
            end if;
            return UILocation;
         when others =>
            null;
      end case;
      return Old_Location;
   end Shortcuts_Keys;

end Shortcuts;
