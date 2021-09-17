-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Bookmarks; use Bookmarks;
with Messages; use Messages;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body ProgramsMenu is

   -- ****iv* ProgramsMenuTUI/ProgramsMenuTUI.ApplicationsList
   -- FUNCTION
   -- List of all applications which can be used to execute files or
   -- directories
   -- SOURCE
   ApplicationsList: Bookmarks_Container.Map;
   -- ****

   -- ****iv* ProgramsMenuTUI/ProgramsMenuTUI.NamesList
   -- FUNCTION
   -- List of all applications showed in the menu
   -- SOURCE
   NamesList: UnboundedString_Container.Vector;
   -- ****

   -- ****it* ProgramsMenuTUI/ProgramsMenuTUI.Programs_Sorting
   -- FUNCTION
   -- Used in sorting available programs
   -- SOURCE
   package Programs_Sorting is new UnboundedString_Container.Generic_Sorting;
   -- ****

   procedure CreateProgramsMenu is
      ApplicationsPaths: constant array
        (Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("/usr/share/applications"),
         To_Unbounded_String("/usr/share/applnk"),
         To_Unbounded_String("/usr/local/share/applications"),
         To_Unbounded_String("/usr/local/share/applnk"),
         To_Unbounded_String(Value("HOME") & "/.local/share/applications"),
         To_Unbounded_String(Value("HOME") & "/.local/share/applnk"));
      SubDirectory: Dir_Type;
      SubLast: Natural;
      SubFileName: String(1 .. 1_024);
      File: File_Type;
      FileLine: Unbounded_String;
   begin
      Create_Programs_Menu_Loop :
      for Path of ApplicationsPaths loop
         if not Ada.Directories.Exists(To_String(Path)) then
            goto End_Of_Loop;
         end if;
         Open(SubDirectory, To_String(Path));
         Read_Desktop_File_Loop :
         loop
            Read(SubDirectory, SubFileName, SubLast);
            exit Read_Desktop_File_Loop when SubLast = 0;
            if Extension(SubFileName(1 .. SubLast)) = "desktop" then
               Open
                 (File, In_File,
                  To_String(Path) & "/" &
                  Simple_Name(SubFileName(1 .. SubLast)));
               Find_Application_Name_Loop :
               while not End_Of_File(File) loop
                  FileLine := To_Unbounded_String(Get_Line(File));
                  if Length(FileLine) > 5
                    and then Slice(FileLine, 1, 5) = "Name=" then
                     ApplicationsList.Include
                       (SubFileName(1 .. SubLast),
                        Slice(FileLine, 6, Length(FileLine)));
                     if not NamesList.Contains
                         (Unbounded_Slice(FileLine, 6, Length(FileLine))) then
                        NamesList.Append
                          (Unbounded_Slice(FileLine, 6, Length(FileLine)));
                     end if;
                     exit Find_Application_Name_Loop;
                  end if;
               end loop Find_Application_Name_Loop;
               Close(File);
            end if;
         end loop Read_Desktop_File_Loop;
         Close(SubDirectory);
         <<End_Of_Loop>>
      end loop Create_Programs_Menu_Loop;
      Programs_Sorting.Sort(NamesList);
   end CreateProgramsMenu;

   function GetProgramName(DesktopFile: String) return String is
   begin
      if not ApplicationsList.Contains(DesktopFile) then
         return DesktopFile;
      end if;
      return ApplicationsList(DesktopFile);
   end GetProgramName;

   ProgramsWindow: Window;
   ProgramsMenu: Menu;

   procedure ShowProgramsMenu is
      Menu_Items: constant Item_Array_Access :=
        new Item_Array(1 .. Natural(ApplicationsList.Length) + 2);
      Index: Positive := 1;
      Visibility: Cursor_Visibility := Invisible;
   begin
      Set_Cursor_Visibility(Visibility);
      for Application of ApplicationsList loop
         Menu_Items.all(Index) := New_Item(Application);
         Index := Index + 1;
      end loop;
      Menu_Items.all(Index) := New_Item("Close");
      Menu_Items.all(Index + 1) := Null_Item;
      ProgramsMenu := New_Menu(Menu_Items);
      Set_Format(ProgramsMenu, 15, 1);
      Set_Mark(ProgramsMenu, "");
      ProgramsWindow := Create(17, 27, Lines / 3, Columns / 3);
      Set_Window(ProgramsMenu, ProgramsWindow);
      Set_Sub_Window
        (ProgramsMenu, Derived_Window(ProgramsWindow, 15, 25, 1, 1));
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
                  for I in ApplicationsList.Iterate loop
                     if ApplicationsList(I) = ApplicationName then
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

end ProgramsMenu;
