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

with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Inotify; use Inotify;

package body Preferences.UI is

   OptionsMenu: Menu;
   MenuWindow: Window;
   OptionsWindow: Window;
   DialogForm: Forms.Form;

   procedure Show_Options_Tab(Tab: Positive) is
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
   begin
      Clear(OptionsWindow);
      Set_Cursor_Visibility(Visibility);
      case Tab is
         when 1 =>
            declare
               Options_Fields: constant Field_Array_Access :=
                 new Field_Array(1 .. 17);
            begin
               Options_Fields.all(1) := New_Field(1, 18, 0, 0, 0, 0);
               Set_Buffer(Options_Fields.all(1), 0, "Directory Listing");
               FieldOptions := Get_Options(Options_Fields.all(1));
               Set_Options(Options_Fields.all(1), FieldOptions);
               Options_Fields.all(2) := New_Field(1, 36, 1, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(2), 0,
                  (if Settings.Show_Hidden then "Show " else "Don't show ") &
                  "hidden files");
               FieldOptions := Get_Options(Options_Fields.all(2));
               Set_Options(Options_Fields.all(2), FieldOptions);
               Options_Fields.all(3) := New_Field(1, 36, 2, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(3), 0,
                  (if Settings.Show_Last_Modified then "Show "
                   else "Don't show ") &
                  "modification time");
               FieldOptions := Get_Options(Options_Fields.all(3));
               Set_Options(Options_Fields.all(3), FieldOptions);
               Options_Fields.all(4) := New_Field(1, 36, 3, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(4), 0,
                  "Auto refresh every" &
                  Natural'Image(Settings.Auto_Refresh_Interval) & " seconds");
               FieldOptions := Get_Options(Options_Fields.all(4));
               Set_Options(Options_Fields.all(4), FieldOptions);
               Options_Fields.all(5) := New_Field(1, 18, 4, 0, 0, 0);
               Set_Buffer(Options_Fields.all(5), 0, "Preview");
               FieldOptions := Get_Options(Options_Fields.all(5));
               Set_Options(Options_Fields.all(5), FieldOptions);
               Options_Fields.all(6) := New_Field(1, 36, 5, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(6), 0,
                  (if Settings.Show_Preview then "Show " else "Don't show ") &
                  "preview");
               FieldOptions := Get_Options(Options_Fields.all(6));
               Set_Options(Options_Fields.all(6), FieldOptions);
               Options_Fields.all(7) := New_Field(1, 36, 6, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(7), 0,
                  (if Settings.Color_Text then "Enable " else "Disable ") &
                  "syntax highlightning");
               FieldOptions := Get_Options(Options_Fields.all(7));
               Set_Options(Options_Fields.all(7), FieldOptions);
               Options_Fields.all(8) := New_Field(1, 36, 7, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(8), 0, To_String(Settings.Color_Theme));
               FieldOptions := Get_Options(Options_Fields.all(8));
               Set_Options(Options_Fields.all(8), FieldOptions);
               Options_Fields.all(9) := New_Field(1, 18, 8, 0, 0, 0);
               Set_Buffer(Options_Fields.all(9), 0, "Interface");
               FieldOptions := Get_Options(Options_Fields.all(9));
               Set_Options(Options_Fields.all(9), FieldOptions);
               Options_Fields.all(10) := New_Field(1, 36, 9, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(10), 0,
                  (if Settings.Stay_In_Old then "Stay in source directory"
                   else "Go to destination"));
               FieldOptions := Get_Options(Options_Fields.all(10));
               Set_Options(Options_Fields.all(10), FieldOptions);
               Options_Fields.all(11) := New_Field(1, 38, 10, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(11), 0,
                  (if Settings.Show_Preview then "Show " else "Don't show ") &
                  "information about finished action");
               FieldOptions := Get_Options(Options_Fields.all(11));
               Set_Options(Options_Fields.all(11), FieldOptions);
               Options_Fields.all(12) := New_Field(1, 18, 11, 0, 0, 0);
               Set_Buffer(Options_Fields.all(12), 0, "Deleting");
               FieldOptions := Get_Options(Options_Fields.all(12));
               Set_Options(Options_Fields.all(12), FieldOptions);
               Options_Fields.all(13) := New_Field(1, 38, 12, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(13), 0,
                  (if Settings.Delete_Files then "Delete files"
                   else "Move files to Trash"));
               FieldOptions := Get_Options(Options_Fields.all(13));
               Set_Options(Options_Fields.all(13), FieldOptions);
               Options_Fields.all(14) := New_Field(1, 38, 13, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(14), 0,
                  (if Settings.Clear_Trash_On_Exit then "Clear "
                   else "Don't clear ") &
                  "Trash on exit");
               FieldOptions := Get_Options(Options_Fields.all(14));
               Set_Options(Options_Fields.all(14), FieldOptions);
               Options_Fields.all(15) := New_Field(1, 18, 14, 0, 0, 0);
               Set_Buffer(Options_Fields.all(15), 0, "Copying or moving");
               FieldOptions := Get_Options(Options_Fields.all(15));
               Set_Options(Options_Fields.all(15), FieldOptions);
               Options_Fields.all(16) := New_Field(1, 38, 15, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(16), 0,
                  (if Settings.Overwrite_On_Exist then "Overwrite "
                   else "Don't overwrite ") &
                  "existing");
               FieldOptions := Get_Options(Options_Fields.all(16));
               Set_Options(Options_Fields.all(16), FieldOptions);
               Options_Fields.all(17) := Null_Field;
               DialogForm := New_Form(Options_Fields);
               Set_Current(DialogForm, Options_Fields(2));
            end;
         when others =>
            null;
      end case;
      Set_Options(DialogForm, (others => False));
      Scale(DialogForm, FormHeight, FormLength);
      Set_Window(DialogForm, OptionsWindow);
      Set_Sub_Window
        (DialogForm,
         Derived_Window(OptionsWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      Refresh(OptionsWindow);
   end Show_Options_Tab;

   procedure Show_Options is
      Main_Menu_Array: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("Preferences"), To_Unbounded_String("Shortcuts"),
         To_Unbounded_String("Commands"), To_Unbounded_String("Modules"),
         To_Unbounded_String("Close"));
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 6);
   begin
      Temporary_Stop := True;
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
      Refresh;
      Refresh(MenuWindow);
      OptionsWindow := Create(Lines - 1, Columns, 1, 0);
      Show_Options_Tab(1);
   end Show_Options;

   function Select_Preferences_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Result2: Forms.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(OptionsMenu));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_LEFT =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_Previous_Item);
         when KEY_RIGHT =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_Next_Item);
         when Key_Home =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_First_Item);
         when Key_End =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_Last_Item);
         when KEY_UP =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_Previous_Field);
         when KEY_DOWN =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_Next_Field);
         when 10 =>
            if CurrentIndex = 5 then
               Set_Cursor_Visibility(Visibility);
               Temporary_Stop := False;
               Clear;
               UILocation := DIRECTORY_VIEW;
               Show_Main_Window;
               return DIRECTORY_VIEW;
            else
               Show_Options_Tab(CurrentIndex);
            end if;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow);
      end if;
      if Result2 = Form_Ok then
         Refresh(OptionsWindow);
      end if;
      return OPTIONS_VIEW;
   end Select_Preferences_Keys;

end Preferences.UI;
