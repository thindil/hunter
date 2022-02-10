-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Common; use Common;
with Messages.UI; use Messages.UI;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body ProgramsMenu.UI is

   ProgramsWindow: Window;
   ProgramsMenu: Menu;

   procedure ShowProgramsMenu is
      Menu_Items: constant Item_Array_Access :=
        new Item_Array(1 .. Natural(Applications_List.Length) + 2);
      Index: Positive := 1;
      Visibility: Cursor_Visibility := Invisible;
      MenuHeight: constant Line_Position :=
        (if Menu_Items'Length < 17 then Line_Position(Menu_Items'Length) + 1
         else 17);
      MenuWidth: Column_Position := 25;
   begin
      Set_Cursor_Visibility(Visibility);
      for Application of Applications_List loop
         if (Application'Length < 25 and MenuWidth = 25) or
           (Application'Length > MenuWidth and MenuWidth < 25) then
            MenuWidth := Column_Position(Application'Length);
            if MenuWidth < 5 then
               MenuWidth := 5;
            end if;
            if MenuWidth > 25 then
               MenuWidth := 25;
            end if;
         end if;
         Menu_Items.all(Index) := New_Item(Application);
         Index := Index + 1;
      end loop;
      Menu_Items.all(Index) := New_Item("Close");
      Menu_Items.all(Index + 1) := Null_Item;
      ProgramsMenu := New_Menu(Menu_Items);
      Set_Format(ProgramsMenu, 15, 1);
      Set_Mark(ProgramsMenu, "");
      ProgramsWindow :=
        Create(MenuHeight, MenuWidth + 2, Lines / 3, Columns / 3);
      Set_Window(ProgramsMenu, ProgramsWindow);
      Set_Sub_Window
        (ProgramsMenu,
         Derived_Window(ProgramsWindow, MenuHeight - 2, MenuWidth, 1, 1));
      Box(ProgramsWindow, Default_Character, Default_Character);
      Post(ProgramsMenu);
      Refresh;
      Refresh(ProgramsWindow);
   end ShowProgramsMenu;

   function Programs_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(ProgramsMenu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(ProgramsMenu, M_Down_Item);
         when Key_Home =>
            Result := Driver(ProgramsMenu, M_First_Item);
         when Key_End =>
            Result := Driver(ProgramsMenu, M_Last_Item);
         when KEY_NPAGE =>
            Result := Driver(ProgramsMenu, M_ScrollUp_Page);
         when KEY_PPAGE =>
            Result := Driver(ProgramsMenu, M_ScrollDown_Page);
         when 10 =>
            if Name(Current(ProgramsMenu)) /= "Close" then
               declare
                  Pid: Process_Id;
                  ExecutableName: constant String :=
                    Find_Executable("xdg-mime");
                  ApplicationName: constant Unbounded_String :=
                    To_Unbounded_String(Name(Current(ProgramsMenu)));
               begin
                  Set_New_Application_Loop :
                  for I in Applications_List.Iterate loop
                     if Applications_List(I) = ApplicationName then
                        Pid :=
                          Non_Blocking_Spawn
                            (ExecutableName,
                             Argument_String_To_List
                               ("default " & Bookmarks_Container.Key(I) & " " &
                                Get_Mime_Type
                                  (To_String(Current_Selected))).all);
                        if Pid = GNAT.OS_Lib.Invalid_Pid then
                           Show_Message
                             ("Could not set new associated program.");
                        end if;
                        exit Set_New_Application_Loop;
                     end if;
                  end loop Set_New_Application_Loop;
               end;
            end if;
            UILocation := DIRECTORY_VIEW;
            Update_Directory_List;
            ShowInfo;
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(ProgramsWindow);
      end if;
      return PROGRAMS_MENU;
   end Programs_Keys;

end ProgramsMenu.UI;
