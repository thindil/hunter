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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Time_Stamp; use GNAT.Time_Stamp;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with AboutDialog; use AboutDialog;
with AboutDialog.UI;
with ActivateItems.UI;
with Bookmarks.UI;
with Common; use Common;
with CreateItems.UI;
with DeleteItems;
with DeleteItems.UI;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with MainWindow; use MainWindow;
with Messages.UI;
with Modules;
with Preferences; use Preferences;
with Preferences.UI;
with ProgramsMenu; use ProgramsMenu;
with ProgramsMenu.UI;
with RenameItems.UI;
with SearchItems;
with Shortcuts;
with ShowItems; use ShowItems;

procedure Hunter is
   use type Interfaces.C.int;

   Key: Key_Code := Key_None;
   Visibility: Cursor_Visibility := Invisible;
   Error_File: File_Type;
   Error_File_Path: constant String :=
     Ada.Environment_Variables.Value(Name => "HOME") &
     "/.cache/hunter/error.log";
   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Alt_Key: Boolean := False;
   procedure Exit_From_Program is
      use DeleteItems;
      use Modules;
   begin
      Save_Preferences;
      Execute_Modules(Interpreter => Interpreter, State => ON_QUIT);
      if Settings.Clear_Trash_On_Exit then
         New_Action := CLEARTRASH;
         if Delete_Selected(Interpreter => Interpreter) then
            null;
         end if;
      end if;
      End_Windows;
      Inotify_Close;
      Magic_Close;
      Delete_File
        (Name =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.cache/hunter/highlight.tmp");
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Exit_From_Program;
begin
   -- Create needed directories
   Create_Path
     (New_Directory =>
        Ada.Environment_Variables.Value(Name => "HOME") & "/.cache/hunter");
   Create_Path
     (New_Directory =>
        Ada.Environment_Variables.Value(Name => "HOME") &
        "/.local/share/hunter/modules");
   -- Start libmagic data
   Magic_Open;
   -- Start inotify
   Inotify_Init;
   -- Start Tcl/Tk

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create(Argc => Argc, Argv => Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable(argv0 => Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interpreter := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(interp => Interpreter) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        (Item =>
           "Hunter: Tcl.Tcl_Init failed: " &
           Tcl.Ada.Tcl_GetStringResult(interp => Interpreter));
      return;
   end if;

   -- Load required Tcl packages
   MsgCat_Init(Interp => Interpreter);

   -- Load the program setting
   Load_Settings;

   -- Load the available applications list
   Create_Programs_Menu;

   -- Initialize ncurses
   Ada.Environment_Variables.Set(Name => "ESCDELAY", Value => "10");
   Ada.Environment_Variables.Set(Name => "TERMINFO", Value => "terminfo");
   Init_Screen;
   Start_Color;
   Set_Colors_Loop :
   for I in 1 .. 16 loop
      Init_Color
        (Color => Color_Number(I + 7), Red => 0, Green => 0, Blue => 0);
      Init_Pair
        (Pair => Color_Pair(I), Fore => Color_Number(I + 7), Back => Black);
   end loop Set_Colors_Loop;
   Set_Timeout_Mode(Win => Standard_Window, Mode => Blocking, Amount => 0);
   Set_Echo_Mode(SwitchOn => False);
   Set_Cursor_Visibility(Visibility => Visibility);

   -- Create the program main window
   if Argument_Count < 1 then
      Create_Main_Window
        (Directory => Ada.Environment_Variables.Value(Name => "HOME"));
   else
      Create_Main_Window(Directory => Full_Name(Name => Argument(Number => 1)));
   end if;

   -- Main program loop, exit on alt+q
   Main_Program_Loop :
   loop
      Key := Get_Keystroke;
      Alt_Key := False;
      -- Escape key pressed
      if Key = 27 then
         Set_NoDelay_Mode(Mode => True);
         Key := Get_Keystroke;
         Set_NoDelay_Mode(Mode => False);
         --## rule off SIMPLIFIABLE_STATEMENTS
         -- Check if pressed key was arrow key
         if Key = 91 then
            Key := Get_Keystroke;
            case Key is
               when 53 =>
                  Key := KEY_NPAGE;
               when 54 =>
                  Key := KEY_PPAGE;
               when 65 =>
                  Key := KEY_UP;
               when 66 =>
                  Key := KEY_DOWN;
               when 67 =>
                  Key := KEY_RIGHT;
               when 68 =>
                  Key := KEY_LEFT;
               when 70 =>
                  Key := Key_End;
               when 72 =>
                  Key := Key_Home;
               when others =>
                  null;
            end case;
         elsif Key = 256 then
            Key := 27;
         elsif Key > 1 then
            Alt_Key := True;
         end if;
      end if;
      -- Tab key pressed
      if Key in KEY_STAB | 9 then
         case Ui_Location is
            when DIRECTORY_VIEW | PATH_BUTTONS =>
               Ui_Location := MAIN_MENU;
               Create_Program_Menu(Update => True);
               Update_Directory_List;
               if New_Action in COPY | MOVE | CREATELINK then
                  ShowDestination;
               end if;
            when MAIN_MENU =>
               Clear_Preview_Window;
               if New_Action in COPY | MOVE | CREATELINK then
                  Ui_Location := DESTINATION_VIEW;
                  ShowDestination;
               else
                  if Info_Form /= Null_Form then
                     Visibility := Normal;
                     Set_Cursor_Visibility(Visibility => Visibility);
                  end if;
                  Ui_Location := PREVIEW;
                  Show_Preview;
               end if;
               Create_Program_Menu(Update => True);
            when DESTINATION_VIEW | DESTINATION_PATH =>
               Ui_Location := DIRECTORY_VIEW;
               Clear_Preview_Window;
               Update_Directory_List;
               ShowDestination;
            when PREVIEW =>
               Ui_Location := DIRECTORY_VIEW;
               Clear_Preview_Window;
               Show_Preview;
               Update_Directory_List;
               Visibility := Invisible;
               Set_Cursor_Visibility(Visibility => Visibility);
               Delete_Temporary_File_Block :
               begin
                  Delete_File
                    (Name =>
                       Ada.Environment_Variables.Value(Name => "HOME") &
                       "/.cache/hunter/highlight.tmp");
               exception
                  when Ada.Directories.Name_Error =>
                     null;
               end Delete_Temporary_File_Block;
            when others =>
               null;
         end case;
      -- Another key pressed, depends on current UI location
      else
         Handle_Keys_Block :
         declare
            use AboutDialog.UI;
            use ActivateItems.UI;
            use Bookmarks.UI;
            use CreateItems.UI;
            use DeleteItems.UI;
            use Messages.UI;
            use Preferences.UI;
            use ProgramsMenu.UI;
            use RenameItems.UI;
            use SearchItems;
            use Shortcuts;

            Old_Ui_Location: constant Ui_Locations := Ui_Location;
         begin
            Ui_Location :=
              Shortcuts_Keys
                (Key => Key, AltKey => Alt_Key, Old_Location => Ui_Location);
            if Ui_Location = Old_Ui_Location then
               case Ui_Location is
                  when DIRECTORY_VIEW =>
                     Ui_Location := Directory_Keys(Key => Key);
                  when PATH_BUTTONS =>
                     Ui_Location := Path_Keys(Key => Key);
                  when MAIN_MENU =>
                     Ui_Location := Menu_Keys(Key => Key);
                     exit Main_Program_Loop when Ui_Location = PATH_BUTTONS;
                  when ACTIONS_MENU =>
                     Ui_Location := Actions_Keys(Key => Key);
                  when CREATE_FORM =>
                     Ui_Location := Create_Keys(Key => Key);
                  when DELETE_FORM =>
                     Ui_Location := Delete_Keys(Key => Key);
                  when MESSAGE_FORM =>
                     Ui_Location := Message_Keys(Key => Key);
                  when RENAME_FORM =>
                     Ui_Location := Rename_Keys(Key => Key);
                  when DESTINATION_VIEW =>
                     Destination_Keys(Key => Key);
                  when DESTINATION_PATH =>
                     Ui_Location := Destination_Path_Keys(Key => Key);
                  when BOOKMARKS_MENU =>
                     Ui_Location := Bookmarks_Keys(Key => Key);
                  when BOOKMARKS_FORM =>
                     Ui_Location := Bookmarks_Form_Keys(Key => Key);
                  when CREATELINK_FORM =>
                     Ui_Location := Create_Link_Keys(Key => Key);
                  when SELECTED_MENU =>
                     Ui_Location := Selected_Keys(Key => Key);
                  when PREVIEW =>
                     Preview_Keys(Key => Key);
                  when PROGRAMS_MENU =>
                     Ui_Location := Programs_Keys(Key => Key);
                  when VIEW_MENU =>
                     Ui_Location := View_Keys(Key => Key);
                  when SEARCH_FORM =>
                     Ui_Location := Search_Form_Keys(Key => Key);
                  when EXECUTE_FORM =>
                     Ui_Location := Execute_Form_Keys(Key => Key);
                  when ABOUT_MENU =>
                     Ui_Location := About_Keys(Key => Key);
                  when ABOUT_FORM =>
                     Ui_Location := About_View_Keys(Key => Key);
                  when DEVELOPERS_VIEW =>
                     Ui_Location := Developers_Keys(Key => Key);
                  when OPTIONS_VIEW =>
                     Ui_Location := Select_Preferences_Keys(Key => Key);
                  when SECONDS_MENU =>
                     Ui_Location := Select_Seconds_Keys(Key => Key);
                  when COLORS_MENU =>
                     Ui_Location := Select_Colors_Keys(Key => Key);
                  when SHORTCUT_FORM =>
                     Ui_Location :=
                       Set_Shortcut_Keys(Key => Key, AltKey => Alt_Key);
                  when COMMAND_FORM =>
                     Ui_Location := Add_Command_Keys(Key => Key);
                  when COMMANDS_MENU =>
                     Ui_Location := User_Commands_Keys(Key => Key);
                  when T_ACTIONS_MENU =>
                     Ui_Location := Trash_Actions_Keys(Key => Key);
                  when QUIT_PROGRAM =>
                     exit Main_Program_Loop;
               end case;
            elsif Ui_Location = QUIT_PROGRAM then
               exit Main_Program_Loop;
            end if;
         end Handle_Keys_Block;
      end if;
   end loop Main_Program_Loop;

   Exit_From_Program;
   Tcl.Ada.Tcl_Eval(interp => Interpreter, strng => "exit");
exception
   when An_Exception : others =>
      Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") & "/.cache/hunter");
      if Exists(Name => Error_File_Path) then
         Open
           (File => Error_File, Mode => Append_File, Name => Error_File_Path);
      else
         Create
           (File => Error_File, Mode => Append_File, Name => Error_File_Path);
      end if;
      Put_Line(File => Error_File, Item => Current_Time);
      Put_Line(File => Error_File, Item => Version_Number);
      Put_Line
        (File => Error_File,
         Item => "Exception: " & Exception_Name(X => An_Exception));
      Put_Line
        (File => Error_File,
         Item => "Message: " & Exception_Message(X => An_Exception));
      Put_Line
        (File => Error_File,
         Item => "-------------------------------------------------");
      Put_Line
        (File => Error_File, Item => Symbolic_Traceback(E => An_Exception));
      Put_Line
        (File => Error_File,
         Item => "-------------------------------------------------");
      Close(File => Error_File);
      Erase;
      Refresh;
      Move_Cursor(Line => Lines / 2, Column => 2);
      Add
        (Str =>
           "Oops, something bad happens and progam crashed. Please, remember what have you done before crash and report this problem at https://www.laeran.pl/repositories/hunter/ticket and attach (if possible) file 'error.log' from '" &
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.cache/hunter' directory.");
      Key := Get_Keystroke;
      Exit_From_Program;
      Tcl.Ada.Tcl_Eval(interp => Interpreter, strng => "exit 1");
end Hunter;
