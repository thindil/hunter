-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Calendar.Formatting;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with AboutDialog; use AboutDialog;
with AboutDialog.UI; use AboutDialog.UI;
with ActivateItems;
with ActivateItems.UI; use ActivateItems.UI;
with Bookmarks; use Bookmarks;
with Bookmarks.UI; use Bookmarks.UI;
with Common; use Common;
with CreateItems.UI; use CreateItems.UI;
with CopyItems; use CopyItems;
with CopyItems.UI; use CopyItems.UI;
with DeleteItems.UI; use DeleteItems.UI;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Modules; use Modules;
with Modules.Commands;
with MoveItems; use MoveItems;
with MoveItems.UI; use MoveItems.UI;
with Preferences; use Preferences;
with Preferences.UI; use Preferences.UI;
with RefreshData; use RefreshData;
with RenameItems.UI; use RenameItems.UI;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;
with Trash; use Trash;
with UserCommands; use UserCommands;
with Utils; use Utils;

package body MainWindow is

   List_Window: Window;
   Path_Buttons_Window: Window;
   Path: Menu;
   Program_Menu: Menu;
   Menu_Window: Window;
   Sub_Menu_Window: Window;
   Sub_Menu: Menu;

   procedure Create_Program_Menu(Update: Boolean := False) is
   begin
      Terminal_Interface.Curses.Clear(Win => Menu_Window);
      case New_Action is
         when CREATELINK =>
            Set_Create_Menu_Block :
            declare
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 4);
            begin
               Menu_Items.all(1) :=
                 New_Item
                   (Name => Mc(Interp => Interpreter, Src_String => "Quit"));
               Menu_Items.all(2) :=
                 New_Item
                   (Name =>
                      Mc
                        (Interp => Interpreter,
                         Src_String => "{Create link}"));
               Menu_Items.all(3) :=
                 New_Item
                   (Name => Mc(Interp => Interpreter, Src_String => "Cancel"));
               Menu_Items.all(4) := Null_Item;
               Program_Menu := New_Menu(Items => Menu_Items);
               Set_Format(Men => Program_Menu, Lines => 1, Columns => 3);
               Set_Mark(Men => Program_Menu, Mark => "");
               Set_Window(Men => Program_Menu, Win => Menu_Window);
               Set_Sub_Window
                 (Men => Program_Menu,
                  Win =>
                    Derived_Window
                      (Win => Menu_Window, Number_Of_Lines => 1,
                       Number_Of_Columns => Columns, First_Line_Position => 0,
                       First_Column_Position => 0));
               Post(Men => Program_Menu);
            end Set_Create_Menu_Block;
         when COPY | MOVE =>
            Set_Copy_Move_Menu_Block :
            declare
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 5);
            begin
               Menu_Items.all(1) :=
                 New_Item
                   (Name => Mc(Interp => Interpreter, Src_String => "Quit"));
               Menu_Items.all(2) :=
                 New_Item
                   (Name =>
                      Mc(Interp => Interpreter, Src_String => "Bookmarks"));
               Menu_Items.all(3) :=
                 (if New_Action = COPY then
                    New_Item
                      (Name =>
                         Mc
                           (Interp => Interpreter,
                            Src_String => "{Copy selected}"))
                  else New_Item
                      (Name =>
                         Mc
                           (Interp => Interpreter,
                            Src_String => "{Move selected}")));
               Menu_Items.all(4) := New_Item(Mc(Interpreter, "Cancel"));
               Menu_Items.all(5) := Null_Item;
               Program_Menu := New_Menu(Menu_Items);
               Set_Format(Program_Menu, 1, 4);
               Set_Mark(Program_Menu, "");
               Set_Window(Program_Menu, Menu_Window);
               Set_Sub_Window
                 (Program_Menu, Derived_Window(Menu_Window, 1, Columns, 0, 0));
               Post(Program_Menu);
            end Set_Copy_Move_Menu_Block;
         when SHOWTRASH | DELETETRASH =>
            declare
               Main_Menu_Array: constant array(1 .. 4) of Unbounded_String :=
                 (To_Unbounded_String(Mc(Interpreter, "Quit")),
                  To_Unbounded_String(Mc(Interpreter, "Bookmarks")),
                  To_Unbounded_String(Mc(Interpreter, "View")),
                  To_Unbounded_String(Mc(Interpreter, "Actions")));
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 5);
            begin
               Create_Trash_Menu_Loop :
               for I in Main_Menu_Array'Range loop
                  Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
               end loop Create_Trash_Menu_Loop;
               Menu_Items.all(Menu_Items'Last) := Null_Item;
               Program_Menu := New_Menu(Menu_Items);
               Set_Format(Program_Menu, 1, 4);
               Set_Mark(Program_Menu, "");
               Menu_Window := Create(1, Columns, 0, 0);
               Set_Window(Program_Menu, Menu_Window);
               Set_Sub_Window
                 (Program_Menu, Derived_Window(Menu_Window, 1, Columns, 0, 0));
               Post(Program_Menu);
            end;
         when others =>
            declare
               Main_Menu_Array: constant array(1 .. 7) of Unbounded_String :=
                 (To_Unbounded_String(Mc(Interpreter, "Quit")),
                  To_Unbounded_String(Mc(Interpreter, "Bookmarks")),
                  To_Unbounded_String(Mc(Interpreter, "View")),
                  To_Unbounded_String(Mc(Interpreter, "Actions")),
                  To_Unbounded_String(Mc(Interpreter, "Selected")),
                  To_Unbounded_String(Mc(Interpreter, "About")),
                  To_Unbounded_String(Mc(Interpreter, "Options")));
               Menu_Items: constant Item_Array_Access :=
                 new Item_Array(1 .. 8);
            begin
               Create_Program_Menu_Loop :
               for I in Main_Menu_Array'Range loop
                  Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
               end loop Create_Program_Menu_Loop;
               Menu_Items.all(8) := Null_Item;
               Program_Menu := New_Menu(Menu_Items);
               Set_Format(Program_Menu, 1, 7);
               Set_Mark(Program_Menu, "");
               Menu_Window := Create(1, Columns, 0, 0);
               Set_Window(Program_Menu, Menu_Window);
               Set_Sub_Window
                 (Program_Menu, Derived_Window(Menu_Window, 1, Columns, 0, 0));
               Post(Program_Menu);
            end;
      end case;
      if Ui_Location = MAIN_MENU then
         Set_Foreground
           (Program_Menu, (Reverse_Video => True, others => False));
      else
         Set_Foreground(Program_Menu, Normal_Video);
      end if;
      if Update then
         Refresh(Menu_Window);
      end if;
   end Create_Program_Menu;

   procedure Create_Main_Window(Directory: String) is
   begin
      -- Load translations
      Mc_Load
        (DirName => "../share/hunter/translations", Interp => Interpreter);
      Set_About_Dialog_Information(Interp => Interpreter);
      ActivateItems.Add_Commands;
      CreateItems.UI.Add_Commands;
      RenameItems.UI.AddCommands;
      UserCommands.Add_Commands;
      Create_Bookmarks_List;
      Modules.Commands.AddCommands;
      Create_Trash;
      if Ada.Directories.Exists(Directory) then
         Common.Current_Directory := To_Unbounded_String(Directory);
      else
         Common.Current_Directory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists
             (To_String(Common.Current_Directory)) then
            Common.Current_Directory := To_Unbounded_String("/");
         end if;
      end if;
      Load_Directory(To_String(Common.Current_Directory));
      Start_Timer(To_String(Common.Current_Directory));
      Show_Main_Window;
   end Create_Main_Window;

   procedure Update_Directory_List
     (Clear: Boolean := False; Search_For: String := "") is
      Menu_Items: constant Item_Array_Access :=
        new Item_Array(Items_List.First_Index .. Items_List.Last_Index + 1);
      Index: Positive;
      Path_Items: Item_Array_Access;
      Tokens: Slice_Set;
      CurrentIndex: Positive := 1;
      TimeString: Unbounded_String;
      Width: Column_Position;
      Height: Line_Position;
   begin
      Terminal_Interface.Curses.Clear(Path_Buttons_Window);
      Common.Current_Directory :=
        To_Unbounded_String
          (Normalize_Pathname(To_String(Common.Current_Directory)));
      Index := Ada.Strings.Unbounded.Count(Common.Current_Directory, "/") + 1;
      if Common.Current_Directory /= To_Unbounded_String("/") then
         if New_Action in SHOWTRASH | DELETETRASH then
            Index :=
              Ada.Strings.Unbounded.Count(Destination_Directory, "/") + 1;
            Create
              (S => Tokens, From => To_String(Source => Destination_Directory),
               Separators => "/");
            Path_Items := new Item_Array(1 .. Index + 1);
            Path_Items.all(1) := New_Item("Trash");
         else
            Create(Tokens, To_String(Common.Current_Directory), "/");
            Path_Items := new Item_Array(1 .. Index + 1);
            Path_Items.all(1) := New_Item("/");
         end if;
         for I in 2 .. Slice_Count(Tokens) loop
            if New_Action = SHOWTRASH and I = 2 then
               declare
                  File_Info: File_Type;
                  File_Line: Unbounded_String;
               begin
                  Open
                    (File => File_Info, Mode => In_File,
                     Name =>
                       Value(Name => "HOME") & "/.local/share/Trash/info/" &
                       Slice(S => Tokens, Index => I) & ".trashinfo");
                  Skip_Line(File => File_Info);
                  Read_File_Path_Loop :
                  for J in 1 .. 2 loop
                     File_Line :=
                       To_Unbounded_String
                         (Source => Get_Line(File => File_Info));
                     if Slice(Source => File_Line, Low => 1, High => 4) =
                       "Path" then
                        Path_Items.all(Positive(I)) :=
                          New_Item
                            (Ada.Directories.Simple_Name
                               (Name =>
                                  Slice
                                    (Source => File_Line, Low => 6,
                                     High => Length(Source => File_Line))),
                             Slice(S => Tokens, Index => I));
                     end if;
                  end loop Read_File_Path_Loop;
                  Close(File => File_Info);
               end;
            else
               Path_Items.all(Positive(I)) := New_Item(Slice(Tokens, I));
            end if;
         end loop;
         Path_Items.all(Index + 1) := Null_Item;
      else
         Path_Items := new Item_Array(1 .. 2);
         Path_Items.all(1) := New_Item("/");
         Path_Items.all(2) := Null_Item;
         Index := 1;
      end if;
      Path := New_Menu(Path_Items);
      Set_Options(Path, (Show_Descriptions => False, others => <>));
      Set_Format(Path, 1, 5);
      Set_Mark(Path, "");
      Set_Window(Path, Path_Buttons_Window);
      Get_Size(List_Window, Height, Width);
      Set_Sub_Window
        (Path, Derived_Window(Path_Buttons_Window, 1, Width - 2, 0, 1));
      Post(Path);
      Set_Current(Path, Path_Items.all(Index));
      Terminal_Interface.Curses.Clear(List_Window);
      if Ui_Location = DIRECTORY_VIEW then
         Box(List_Window, Default_Character, Default_Character);
      end if;
      Add(List_Window, 1, 10, Mc(Interpreter, "Name"));
      if Settings.Show_Last_Modified then
         Add(List_Window, 1, Width - 27, Mc(Interpreter, "Modified"));
      end if;
      Add(List_Window, 1, Width - 10, Mc(Interpreter, "Size"));
      Index := 1;
      declare
         Item_Entry: String(1 .. Positive(Width - 2));
      begin
         Load_Directory_View_Loop :
         for I in Items_List.First_Index .. Items_List.Last_Index loop
            if not Settings.Show_Hidden and then Items_List(I).Is_Hidden then
               goto End_Of_Loop;
            end if;
            if Search_For'Length > 0
              and then
                Ada.Strings.Unbounded.Index(Items_List(I).Name, Search_For) =
                0 then
               goto End_Of_Loop;
            end if;
            Move
              (Source => To_String(Items_List(I).Name), Target => Item_Entry,
               Drop => Right);
            if Settings.Show_Last_Modified then
               begin
                  TimeString :=
                    To_Unbounded_String
                      (Ada.Calendar.Formatting.Image(Items_List(I).Modified));
               exception
                  when Ada.Calendar.Time_Error =>
                     TimeString :=
                       To_Unbounded_String(Mc(Interpreter, "unknown"));
               end;
               Overwrite
                 (Item_Entry, Item_Entry'Last - 27, To_String(TimeString));
            end if;
            case Items_List(I).Size is
               when -2 =>
                  Overwrite(Item_Entry, Item_Entry'Last - 8, "->");
               when -1 =>
                  Overwrite
                    (Item_Entry, Item_Entry'Last - 8,
                     Mc(Interpreter, "unknown"));
               when others =>
                  if not Items_List(I).Is_Directory then
                     Overwrite
                       (Item_Entry, Item_Entry'Last - 8,
                        Count_File_Size
                          (Ada.Directories.File_Size(Items_List(I).Size)));
                  else
                     if Settings.Show_Hidden then
                        Overwrite
                          (Item_Entry, Item_Entry'Last - 8,
                           Item_Size'Image
                             (Items_List(I).Size +
                              Item_Size(Items_List(I).Hidden_Items)));
                     else
                        Overwrite
                          (Item_Entry, Item_Entry'Last - 8,
                           Item_Size'Image(Items_List(I).Size));
                     end if;
                  end if;
            end case;
            Menu_Items.all(Index) :=
              New_Item(Item_Entry, To_String(Items_List(I).Path));
            if Items_List(I).Path = Current_Selected then
               CurrentIndex := Index;
            end if;
            Index := Index + 1;
            <<End_Of_Loop>>
         end loop Load_Directory_View_Loop;
      end;
      Fill_Empty_Entries_Loop :
      for I in Index .. Menu_Items'Last loop
         Menu_Items.all(I) := Null_Item;
      end loop Fill_Empty_Entries_Loop;
      Directory_List := New_Menu(Menu_Items);
      if Index > 1 then
         Set_Options
           (Directory_List,
            (One_Valued => False, Non_Cyclic => True, others => <>));
         Set_Format(Directory_List, Lines - 5, 1);
         Set_Mark(Directory_List, "");
         Set_Window(Directory_List, List_Window);
         Set_Sub_Window
           (Directory_List,
            Derived_Window(List_Window, Lines - 5, (Columns / 2) - 2, 2, 1));
         Post(Directory_List);
         Set_Current(Directory_List, Menu_Items.all(CurrentIndex));
         if not Clear then
            Update_Selected_Loop :
            for I in 1 .. Item_Count(Directory_List) loop
               if Selected_Items.Contains
                   (To_Unbounded_String
                      (Description(Items(Directory_List, I)))) then
                  Set_Value(Items(Directory_List, I), True);
               end if;
            end loop Update_Selected_Loop;
         end if;
         Show_Selected;
      end if;
      Refresh;
      Refresh(Path_Buttons_Window);
      Refresh(List_Window);
   end Update_Directory_List;

   function Directory_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result;
      Visibility: Cursor_Visibility := Invisible;
   begin
      Set_Cursor_Visibility(Visibility);
      if Item_Count(Directory_List) = 0 then
         if Key in KEY_LEFT | KEY_RIGHT then
            Ui_Location := PATH_BUTTONS;
            return Path_Keys(Key);
         end if;
         return DIRECTORY_VIEW;
      end if;
      case Key is
         when KEY_UP =>
            Result := Driver(Directory_List, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Directory_List, M_Down_Item);
         when 32 =>
            Result := Driver(Directory_List, M_Toggle_Item);
            Result := Driver(Directory_List, M_Down_Item);
         when Key_Home =>
            Result := Driver(Directory_List, M_First_Item);
         when Key_End =>
            Result := Driver(Directory_List, M_Last_Item);
         when KEY_NPAGE =>
            Result := Driver(Directory_List, M_ScrollUp_Page);
         when KEY_PPAGE =>
            Result := Driver(Directory_List, M_ScrollDown_Page);
         when KEY_LEFT | KEY_RIGHT =>
            Ui_Location := PATH_BUTTONS;
            return Path_Keys(Key);
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
         Refresh(List_Window);
         if New_Action /= COPY then
            Show_Selected;
         end if;
      end if;
      return DIRECTORY_VIEW;
   end Directory_Keys;

   function Path_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when KEY_LEFT =>
            Result := Driver(Path, M_Previous_Item);
         when KEY_RIGHT =>
            Result := Driver(Path, M_Next_Item);
         when Key_Home =>
            Result := Driver(Path, M_First_Item);
         when Key_End =>
            Result := Driver(Path, M_Last_Item);
         when 10 =>
            Ui_Location := DIRECTORY_VIEW;
            if New_Action in SHOWTRASH | DELETETRASH then
               Common.Current_Directory :=
                 To_Unbounded_String
                   (Value("HOME") & "/.local/share/Trash/files/");
               Update_Trash_Directory_Loop :
               for I in 2 .. Get_Index(Current(Path)) loop
                  if I = 2 then
                     Append
                       (Common.Current_Directory, Description(Items(Path, I)));
                  else
                     Append(Common.Current_Directory, Name(Items(Path, I)));
                  end if;
                  if I < Get_Index(Current(Path)) then
                     Append(Common.Current_Directory, "/");
                  end if;
               end loop Update_Trash_Directory_Loop;
               if Get_Index(Current(Path)) = 1 then
                  Tcl_Eval(Interpreter, "ShowTrash");
                  Update_Directory_List(True);
               else
                  Tcl_Eval
                    (Interpreter,
                     "GoToTrash " & To_String(Common.Current_Directory));
               end if;
            else
               Common.Current_Directory := To_Unbounded_String("/");
               Update_Current_Directory_Loop :
               for I in 2 .. Get_Index(Current(Path)) loop
                  Append(Common.Current_Directory, Name(Items(Path, I)));
                  if I < Get_Index(Current(Path)) then
                     Append(Common.Current_Directory, "/");
                  end if;
               end loop Update_Current_Directory_Loop;
               Load_Directory(To_String(Common.Current_Directory));
               Update_Directory_List(True);
               Update_Watch(To_String(Common.Current_Directory));
               Execute_Modules
                 (Interpreter, On_Enter_Trigger,
                  "{" & To_String(Common.Current_Directory) & "}");
            end if;
            return DIRECTORY_VIEW;
         when others =>
            Ui_Location := DIRECTORY_VIEW;
            return Directory_Keys(Key);
      end case;
      if Result = Menu_Ok then
         Refresh(Path_Buttons_Window);
      end if;
      return PATH_BUTTONS;
   end Path_Keys;

   procedure Draw_Menu(Menu_Type: Ui_Locations) is
      Menu_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      Selected_Amount: Natural := 0;
      Bookmark_Exists: Boolean := False;
   begin
      case Menu_Type is
         when ACTIONS_MENU =>
            if User_Commands_List.Length = 0 then
               Menu_Items := new Item_Array(1 .. 10);
            else
               Menu_Items := new Item_Array(1 .. 11);
               Menu_Items.all(9) := New_Item(Mc(Interpreter, "User commands"));
            end if;
            Menu_Items.all(1) :=
              New_Item(Mc(Interpreter, "{Create new directory}"));
            Menu_Items.all(2) :=
              New_Item(Mc(Interpreter, "{Create new file}"));
            Menu_Items.all(3) :=
              New_Item(Mc(Interpreter, "{Create new link}"));
            Menu_Items.all(4) :=
              New_Item(Mc(Interpreter, "{Rename selected}"));
            Menu_Items.all(5) := New_Item(Mc(Interpreter, "{Start copying}"));
            Menu_Items.all(6) := New_Item(Mc(Interpreter, "{Start moving}"));
            Menu_Items.all(7) :=
              New_Item(Mc(Interpreter, "{Delete selected}"));
            Menu_Items.all(8) :=
              New_Item(Mc(Interpreter, "{Clear the Trash}"));
         when BOOKMARKS_MENU =>
            Menu_Items := Show_Bookmarks_Menu;
         when SELECTED_MENU =>
            if Is_Directory(To_String(Current_Selected)) then
               if Xdg_Bookmarks_List.Contains(Item => Current_Selected) then
                  Menu_Items := new Item_Array(1 .. 5);
               else
                  Menu_Items := new Item_Array(1 .. 6);
               end if;
            elsif Is_Text(Get_Mime_Type(To_String(Current_Selected))) then
               Menu_Items := new Item_Array(1 .. 5);
            else
               Menu_Items := new Item_Array(1 .. 3);
            end if;
            if Menu_Items'Length > 3 then
               Menu_Items.all(1) := New_Item(Mc(Interpreter, "Preview"));
               Menu_Items.all(2) := New_Item(Mc(Interpreter, "Information"));
               Menu_Items.all(3) :=
                 New_Item(Mc(Interpreter, "{Execute with}"));
            else
               Menu_Items.all(1) :=
                 New_Item(Mc(Interpreter, "{Execute with}"));
            end if;
            for Bookmark of Bookmarks_List loop
               if Bookmark = Current_Selected then
                  Bookmark_Exists := True;
                  exit;
               end if;
            end loop;
            if not Xdg_Bookmarks_List.Contains(Item => Current_Selected) then
               if Bookmark_Exists then
                  Menu_Items.all(4) :=
                    New_Item(Mc(Interpreter, "{Remove bookmark}"));
               elsif Is_Directory(To_String(Current_Selected)) then
                  Menu_Items.all(4) :=
                    New_Item(Mc(Interpreter, "{Add bookmark}"));
               end if;
            end if;
         when VIEW_MENU =>
            Menu_Items :=
              new Item_Array(1 .. (if New_Action = SHOWTRASH then 4 else 5));
            Count_Selected_Loop :
            for I in 1 .. Item_Count(Directory_List) loop
               if Selected_Items.Contains
                   (To_Unbounded_String
                      (Description(Items(Directory_List, I)))) then
                  Selected_Amount := Selected_Amount + 1;
               end if;
            end loop Count_Selected_Loop;
            if Selected_Amount < Item_Count(Directory_List) then
               Menu_Items.all(1) := New_Item(Mc(Interpreter, "{Select all}"));
            else
               Menu_Items.all(1) :=
                 New_Item(Mc(Interpreter, "{Deselect all}"));
            end if;
            Menu_Items.all(2) := New_Item(Mc(Interpreter, "{Search for}"));
            if New_Action /= SHOWTRASH then
               Menu_Items.all(3) := New_Item(Mc(Interpreter, "{Show Trash}"));
            end if;
         when ABOUT_MENU =>
            Menu_Items := new Item_Array(1 .. 7);
            Menu_Items.all(1) :=
              New_Item(Mc(Interpreter, "{About the program}"));
            Menu_Items.all(2) := New_Item(Mc(Interpreter, "{Show README}"));
            Menu_Items.all(3) :=
              New_Item(Mc(Interpreter, "{Show list of changes}"));
            Menu_Items.all(4) := New_Item(Mc(Interpreter, "{Get involved}"));
            Menu_Items.all(5) :=
              New_Item(Mc(Interpreter, "{Show modding guide}"));
         when COMMANDS_MENU =>
            Menu_Items :=
              new Item_Array(1 .. Natural(User_Commands_List.Length) + 2);
            declare
               Index: Positive := 1;
            begin
               for I in User_Commands_List.Iterate loop
                  Menu_Items.all(Index) := New_Item(Commands_Container.Key(I));
                  Index := Index + 1;
               end loop;
            end;
         when T_ACTIONS_MENU =>
            Menu_Items := new Item_Array(1 .. 5);
            Menu_Items.all(1) :=
              New_Item(Mc(Interpreter, "{Restore selected}"));
            Menu_Items.all(2) :=
              New_Item(Mc(Interpreter, "{Delete selected}"));
            Menu_Items.all(3) := New_Item(Mc(Interpreter, "{Clear trash}"));
         when others =>
            null;
      end case;
      Menu_Items.all(Menu_Items'Last - 1) :=
        New_Item(Mc(Interpreter, "Close"));
      Menu_Items.all(Menu_Items'Last) := Null_Item;
      Sub_Menu := New_Menu(Menu_Items);
      Set_Options(Sub_Menu, (Non_Cyclic => False, others => <>));
      Set_Format(Sub_Menu, Lines - 5, 1);
      Set_Mark(Sub_Menu, "");
      Scale(Sub_Menu, MenuHeight, MenuLength);
      Sub_Menu_Window :=
        Create(MenuHeight + 2, MenuLength + 2, Lines / 3, Columns / 3);
      Set_Window(Sub_Menu, Sub_Menu_Window);
      Set_Sub_Window
        (Sub_Menu,
         Derived_Window(Sub_Menu_Window, MenuHeight, MenuLength, 1, 1));
      Box(Sub_Menu_Window, Default_Character, Default_Character);
      Post(Sub_Menu);
      Refresh;
      Refresh(Sub_Menu_Window);
   end Draw_Menu;

   function Menu_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(Program_Menu));
      OverwriteItem: Boolean := False;
   begin
      case Key is
         when KEY_LEFT =>
            Result := Driver(Program_Menu, M_Previous_Item);
         when KEY_RIGHT =>
            Result := Driver(Program_Menu, M_Next_Item);
         when Key_Home =>
            Result := Driver(Program_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Program_Menu, M_Last_Item);
         when 10 =>
            Ui_Location := DIRECTORY_VIEW;
            Create_Program_Menu(True);
            case CurrentIndex is
               when 1 =>
                  return PATH_BUTTONS;
               when 2 =>
                  if New_Action = CREATELINK then
                     Show_Create_Link_Form;
                     return CREATELINK_FORM;
                  end if;
                  Draw_Menu(BOOKMARKS_MENU);
                  return BOOKMARKS_MENU;
               when 3 =>
                  if New_Action = COPY then
                     Copy_Items_List.Clear;
                     if Item_Count(Directory_List) > 0 then
                        Update_Copy_Items_Loop :
                        for I in 1 .. Item_Count(Directory_List) loop
                           if Value(Items(Directory_List, I)) or
                             Current(Directory_List) =
                               Items(Directory_List, I) then
                              Copy_Items_List.Append
                                (To_Unbounded_String
                                   (Description(Items(Directory_List, I))));
                           end if;
                        end loop Update_Copy_Items_Loop;
                     end if;
                     if CopySelected(OverwriteItem) = DIRECTORY_VIEW then
                        New_Action := Default_Item_Action;
                        Create_Program_Menu;
                        Refresh(Menu_Window);
                        return DIRECTORY_VIEW;
                     else
                        return MESSAGE_FORM;
                     end if;
                  elsif New_Action = MOVE then
                     Move_Items_List.Clear;
                     if Item_Count(Directory_List) > 0 then
                        Update_Move_Items_Loop :
                        for I in 1 .. Item_Count(Directory_List) loop
                           if Value(Items(Directory_List, I)) or
                             Current(Directory_List) =
                               Items(Directory_List, I) then
                              Move_Items_List.Append
                                (To_Unbounded_String
                                   (Description(Items(Directory_List, I))));
                           end if;
                        end loop Update_Move_Items_Loop;
                     end if;
                     if MoveSelected(OverwriteItem) = DIRECTORY_VIEW then
                        New_Action := Default_Item_Action;
                        Create_Program_Menu;
                        Refresh(Menu_Window);
                        return DIRECTORY_VIEW;
                     else
                        return MESSAGE_FORM;
                     end if;
                  elsif New_Action = CREATELINK then
                     New_Action := Default_Item_Action;
                     Ui_Location := DIRECTORY_VIEW;
                     Update_Directory_List(True);
                     Show_Preview;
                     Create_Program_Menu;
                     Refresh(Menu_Window);
                     return DIRECTORY_VIEW;
                  end if;
                  Draw_Menu(VIEW_MENU);
                  return VIEW_MENU;
               when 4 =>
                  if New_Action in COPY | MOVE then
                     New_Action := Default_Item_Action;
                     Ui_Location := DIRECTORY_VIEW;
                     Update_Directory_List(True);
                     Show_Preview;
                     Create_Program_Menu;
                     Refresh(Menu_Window);
                     return DIRECTORY_VIEW;
                  elsif New_Action = SHOWTRASH then
                     Draw_Menu(T_ACTIONS_MENU);
                     return T_ACTIONS_MENU;
                  end if;
                  Draw_Menu(ACTIONS_MENU);
                  return ACTIONS_MENU;
               when 5 =>
                  if New_Action = SHOWTRASH then
                     New_Action := DELETETRASH;
                     Show_Delete_Form;
                     return DELETE_FORM;
                  end if;
                  Draw_Menu(SELECTED_MENU);
                  return SELECTED_MENU;
               when 6 =>
                  if New_Action = SHOWTRASH then
                     New_Action := CLEARTRASH;
                     Tcl_Eval(Interpreter, "ClearTrash");
                     return MESSAGE_FORM;
                  end if;
                  Draw_Menu(ABOUT_MENU);
                  return ABOUT_MENU;
               when 7 =>
                  Show_Options;
                  return OPTIONS_VIEW;
               when others =>
                  return MAIN_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Menu_Window);
      end if;
      return MAIN_MENU;
   end Menu_Keys;

   function Actions_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(Sub_Menu));
      CurrentName: constant String := Name(Current(Sub_Menu));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 27 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            return DIRECTORY_VIEW;
         when 10 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Update_Directory_List;
            Show_Preview;
            case CurrentIndex is
               when 1 =>
                  New_Action := CREATEDIRECTORY;
                  Show_Create_Form("directory");
                  return CREATE_FORM;
               when 2 =>
                  New_Action := CREATEFILE;
                  Show_Create_Form("file");
                  return CREATE_FORM;
               when 3 =>
                  New_Action := CREATELINK;
                  Create_Program_Menu;
                  Refresh(Menu_Window);
                  Destination_Directory := Common.Current_Directory;
                  Second_Items_List := Items_List;
                  Ui_Location := DESTINATION_VIEW;
                  ShowDestination;
                  return DESTINATION_VIEW;
               when 4 =>
                  New_Action := RENAME;
                  ShowRenameForm;
                  return RENAME_FORM;
               when 5 =>
                  New_Action := COPY;
                  Create_Program_Menu;
                  Refresh(Menu_Window);
                  Destination_Directory := Common.Current_Directory;
                  Second_Items_List := Items_List;
                  Ui_Location := DESTINATION_VIEW;
                  ShowDestination;
                  return DESTINATION_VIEW;
               when 6 =>
                  New_Action := MOVE;
                  Create_Program_Menu;
                  Refresh(Menu_Window);
                  Destination_Directory := Common.Current_Directory;
                  Second_Items_List := Items_List;
                  Ui_Location := DESTINATION_VIEW;
                  ShowDestination;
                  return DESTINATION_VIEW;
               when 7 =>
                  New_Action := DELETE;
                  Show_Delete_Form;
                  return DELETE_FORM;
               when 8 =>
                  Tcl_Eval(Interpreter, "ClearTrash");
                  return MESSAGE_FORM;
               when others =>
                  if CurrentName = Mc(Interpreter, "Close") then
                     Ui_Location := DIRECTORY_VIEW;
                     Update_Directory_List(True);
                     Show_Preview;
                     return DIRECTORY_VIEW;
                  end if;
                  Draw_Menu(COMMANDS_MENU);
                  return COMMANDS_MENU;
            end case;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return ACTIONS_MENU;
   end Actions_Keys;

   function Bookmarks_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Bookmark: constant String := Name(Current(Sub_Menu));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 27 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            return DIRECTORY_VIEW;
         when 10 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            return Go_To_Bookmark(Bookmark);
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return BOOKMARKS_MENU;
   end Bookmarks_Keys;

   function Selected_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Visibility: Cursor_Visibility := Normal;
      New_Location: Ui_Locations := PREVIEW;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 27 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            return DIRECTORY_VIEW;
         when 10 =>
            Ui_Location := New_Location;
            case Get_Index(Current(Sub_Menu)) is
               when 1 =>
                  Show_Preview;
               when 2 =>
                  Set_Cursor_Visibility(Visibility);
                  ShowInfo;
               when 3 =>
                  New_Location := EXECUTE_FORM;
                  Update_Directory_List;
                  Show_Execute_With_Dialog;
               when 4 =>
                  if Name(Current(Sub_Menu)) =
                    Mc(Interpreter, "{Add bookmark}") then
                     Add_Bookmark;
                  elsif Name(Current(Sub_Menu)) =
                    Mc(Interpreter, "{Remove bookmark}") then
                     Remove_Bookmark;
                  end if;
                  New_Location := DIRECTORY_VIEW;
               when others =>
                  New_Location := DIRECTORY_VIEW;
            end case;
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            if New_Location /= EXECUTE_FORM then
               Ui_Location := New_Location;
               Update_Directory_List;
            end if;
            return New_Location;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return SELECTED_MENU;
   end Selected_Keys;

   function View_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Current_Menu: constant String := Name(Current(Sub_Menu));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 27 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            return DIRECTORY_VIEW;
         when 10 =>
            Ui_Location := DIRECTORY_VIEW;
            if Current_Menu in Mc(Interpreter, "{Select all}") |
                  Mc(Interpreter, "{Deselect all}") then
               if Current_Menu = Mc(Interpreter, "{Select all}") then
                  Update_Selected_Items_Loop :
                  for I in 1 .. Item_Count(Directory_List) loop
                     Selected_Items.Append
                       (To_Unbounded_String
                          (Description(Items(Directory_List, I))));
                  end loop Update_Selected_Items_Loop;
               else
                  Selected_Items.Clear;
               end if;
            elsif Current_Menu = Mc(Interpreter, "{Search for}") then
               Update_Directory_List;
               Post(Sub_Menu, False);
               Delete(Sub_Menu);
               ShowSearchForm;
               return SEARCH_FORM;
            elsif Current_Menu = Mc(Interpreter, "{Show Trash}") then
               New_Action := SHOWTRASH;
               Create_Program_Menu(True);
               Post(Sub_Menu, False);
               Delete(Sub_Menu);
               Clear_Preview_Window;
               Tcl_Eval(Interpreter, "ShowTrash");
               Update_Directory_List(True);
               return DIRECTORY_VIEW;
            end if;
            Update_Directory_List;
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return VIEW_MENU;
   end View_Keys;

   function About_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(Sub_Menu));
      FileName: Unbounded_String := Null_Unbounded_String;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 27 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            return DIRECTORY_VIEW;
         when 10 =>
            case CurrentIndex is
               when 1 =>
                  Update_Directory_List;
                  Show_Preview;
                  Post(Sub_Menu, False);
                  Delete(Sub_Menu);
                  Show_About_Dialog;
                  return ABOUT_FORM;
               when 2 =>
                  FileName := To_Unbounded_String("README.md");
               when 3 =>
                  FileName := To_Unbounded_String("CHANGELOG.md");
               when 4 =>
                  FileName := To_Unbounded_String("CONTRIBUTING.md");
               when 5 =>
                  FileName := To_Unbounded_String("MODDING.md");
               when others =>
                  null;
            end case;
            if FileName /= Null_Unbounded_String then
               if Ada.Directories.Exists
                   (Value("APPDIR", "") & "/usr/share/doc/hunter") then
                  Common.Current_Directory :=
                    To_Unbounded_String
                      (Value("APPDIR", "") & "/usr/share/doc/hunter");
               else
                  Common.Current_Directory :=
                    To_Unbounded_String
                      (Normalize_Pathname
                         (Ada.Directories.Containing_Directory
                            (Ada.Directories.Containing_Directory
                               (Command_Name))));
               end if;
               Load_Directory(To_String(Common.Current_Directory));
               Set_Current_Selected_Loop :
               for I in Items_List.Iterate loop
                  if Items_List(I).Name = FileName then
                     Current_Selected := Items_List(I).Path;
                     exit Set_Current_Selected_Loop;
                  end if;
               end loop Set_Current_Selected_Loop;
            end if;
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List(True);
            Show_Preview;
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return ABOUT_MENU;
   end About_Keys;

   procedure Show_Main_Window is
   begin
      Menu_Window := Create(1, Columns, 0, 0);
      Create_Program_Menu;
      Path_Buttons_Window := Create(1, Columns / 2, 1, 0);
      List_Window :=
        (if Settings.Show_Preview then Create(Lines - 2, Columns / 2, 2, 0)
         else Create(Lines - 2, Columns, 2, 0));
      Box(List_Window, Default_Character, Default_Character);
      Refresh;
      Refresh(Menu_Window);
      Refresh(List_Window);
      CreateShowItemsUI;
      Update_Directory_List(True);
   end Show_Main_Window;

   function User_Commands_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Current_Option: constant String := Name(Current(Sub_Menu));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 10 =>
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            if Current_Option /= "Close" then
               Tcl_Eval(Interpreter, "ExecuteCommand " & Current_Option);
            end if;
            return DIRECTORY_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return COMMANDS_MENU;
   end User_Commands_Keys;

   function Trash_Actions_Keys(Key: Key_Code) return Ui_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(Sub_Menu));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(Sub_Menu, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(Sub_Menu, M_Down_Item);
         when Key_Home =>
            Result := Driver(Sub_Menu, M_First_Item);
         when Key_End =>
            Result := Driver(Sub_Menu, M_Last_Item);
         when 10 =>
            Post(Sub_Menu, False);
            Delete(Sub_Menu);
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List;
            Show_Preview;
            case CurrentIndex is
               when 1 =>
                  Tcl_Eval(Interpreter, "RestoreItems");
                  Clear_Preview_Window;
                  Update_Directory_List(True);
                  return DIRECTORY_VIEW;
               when 2 =>
                  New_Action := DELETETRASH;
                  Show_Delete_Form;
                  return DELETE_FORM;
               when 3 =>
                  New_Action := CLEARTRASH;
                  Tcl_Eval(Interpreter, "ClearTrash");
                  return MESSAGE_FORM;
               when others =>
                  Clear_Preview_Window;
                  Update_Directory_List(True);
                  return DIRECTORY_VIEW;
            end case;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(Sub_Menu_Window);
      end if;
      return T_ACTIONS_MENU;
   end Trash_Actions_Keys;

end MainWindow;
