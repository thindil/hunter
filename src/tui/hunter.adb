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
with AboutDialog.UI; use AboutDialog.UI;
with ActivateItems.UI; use ActivateItems.UI;
with Bookmarks.UI; use Bookmarks.UI;
with Common; use Common;
with CreateItems.UI; use CreateItems.UI;
with DeleteItems; use DeleteItems;
with DeleteItems.UI; use DeleteItems.UI;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with Modules; use Modules;
with Preferences; use Preferences;
with Preferences.UI; use Preferences.UI;
with ProgramsMenu; use ProgramsMenu;
with ProgramsMenu.UI; use ProgramsMenu.UI;
with RenameItems.UI; use RenameItems.UI;
with SearchItems; use SearchItems;
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
      CreateMainWindow
        (Directory => Ada.Environment_Variables.Value(Name => "HOME"));
   else
      CreateMainWindow(Directory => Full_Name(Name => Argument(Number => 1)));
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
         --## rule on SIMPLIFIABLE_STATEMENTS
         exit Main_Program_Loop when Key = 113;
      end if;
      if Key in KEY_STAB | 9 then
         case UILocation is
            when DIRECTORY_VIEW | PATH_BUTTONS =>
               UILocation := MAIN_MENU;
               CreateProgramMenu(Update => True);
               Update_Directory_List;
               if New_Action in COPY | MOVE | CREATELINK then
                  ShowDestination;
               end if;
            when MAIN_MENU =>
               Clear_Preview_Window;
               if New_Action in COPY | MOVE | CREATELINK then
                  UILocation := DESTINATION_VIEW;
                  ShowDestination;
               else
                  if Info_Form /= Null_Form then
                     Visibility := Normal;
                     Set_Cursor_Visibility(Visibility => Visibility);
                  end if;
                  UILocation := PREVIEW;
                  Show_Preview;
               end if;
               CreateProgramMenu(Update => True);
            when DESTINATION_VIEW | DESTINATION_PATH =>
               UILocation := DIRECTORY_VIEW;
               Clear_Preview_Window;
               Update_Directory_List;
               ShowDestination;
            when PREVIEW =>
               UILocation := DIRECTORY_VIEW;
               Clear_Preview_Window;
               Show_Preview;
               Update_Directory_List;
               Visibility := Invisible;
               Set_Cursor_Visibility(Visibility => Visibility);
               Delete_Temporary_File_Block:
               begin
                  Delete_File
                    (Name => Ada.Environment_Variables.Value(Name => "HOME") &
                      "/.cache/hunter/highlight.tmp");
               exception
                  when Ada.Directories.Name_Error =>
                     null;
               end Delete_Temporary_File_Block;
            when others =>
               null;
         end case;
      else
         case UILocation is
            when DIRECTORY_VIEW =>
               UILocation := Directory_Keys(Key => Key);
            when PATH_BUTTONS =>
               UILocation := Path_Keys(Key => Key);
            when MAIN_MENU =>
               UILocation := Menu_Keys(Key => Key);
               exit Main_Program_Loop when UILocation = PATH_BUTTONS;
            when ACTIONS_MENU =>
               UILocation := Actions_Keys(Key => Key);
            when CREATE_FORM =>
               UILocation := Create_Keys(Key => Key);
            when DELETE_FORM =>
               UILocation := Delete_Keys(Key => Key);
            when MESSAGE_FORM =>
               UILocation := Message_Keys(Key => Key);
            when RENAME_FORM =>
               UILocation := Rename_Keys(Key => Key);
            when DESTINATION_VIEW =>
               Destination_Keys(Key => Key);
            when DESTINATION_PATH =>
               UILocation := Destination_Path_Keys(Key => Key);
            when BOOKMARKS_MENU =>
               UILocation := Bookmarks_Keys(Key => Key);
            when BOOKMARKS_FORM =>
               UILocation := Bookmarks_Form_Keys(Key);
            when CREATELINK_FORM =>
               UILocation := Create_Link_Keys(Key);
            when SELECTED_MENU =>
               UILocation := Selected_Keys(Key);
            when PREVIEW =>
               Preview_Keys(Key);
            when PROGRAMS_MENU =>
               UILocation := Programs_Keys(Key);
            when VIEW_MENU =>
               UILocation := View_Keys(Key);
            when SEARCH_FORM =>
               UILocation := Search_Form_Keys(Key);
            when EXECUTE_FORM =>
               UILocation := Execute_Form_Keys(Key);
            when ABOUT_MENU =>
               UILocation := About_Keys(Key);
            when ABOUT_FORM =>
               UILocation := About_View_Keys(Key);
            when DEVELOPERS_VIEW =>
               UILocation := Developers_Keys(Key);
            when OPTIONS_VIEW =>
               UILocation := Select_Preferences_Keys(Key);
            when SECONDS_MENU =>
               UILocation := Select_Seconds_Keys(Key);
            when COLORS_MENU =>
               UILocation := Select_Colors_Keys(Key);
            when SHORTCUT_FORM =>
               UILocation := Set_Shortcut_Keys(Key, Alt_Key);
            when COMMAND_FORM =>
               UILocation := Add_Command_Keys(Key);
            when COMMANDS_MENU =>
               UILocation := User_Commands_Keys(Key);
            when T_ACTIONS_MENU =>
               UILocation := Trash_Actions_Keys(Key);
         end case;
      end if;
   end loop Main_Program_Loop;

   Exit_From_Program;
   Tcl.Ada.Tcl_Eval(Interpreter, "exit");
exception
   when An_Exception : others =>
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.cache/hunter");
      if Exists(Error_File_Path) then
         Open(Error_File, Append_File, Error_File_Path);
      else
         Create(Error_File, Append_File, Error_File_Path);
      end if;
      Put_Line(Error_File, Current_Time);
      Put_Line(Error_File, Version_Number);
      Put_Line(Error_File, "Exception: " & Exception_Name(An_Exception));
      Put_Line(Error_File, "Message: " & Exception_Message(An_Exception));
      Put_Line
        (Error_File, "-------------------------------------------------");
      Put_Line(Error_File, Symbolic_Traceback(An_Exception));
      Put_Line
        (Error_File, "-------------------------------------------------");
      Close(Error_File);
      Erase;
      Refresh;
      Move_Cursor(Line => (Lines / 2), Column => 2);
      Add
        (Str =>
           "Oops, something bad happens and progam crashed. Please, remember what have you done before crash and report this problem at https://www.laeran.pl/repositories/hunter/ticket and attach (if possible) file 'error.log' from '" &
           Ada.Environment_Variables.Value("HOME") &
           "/.cache/hunter' directory.");
      Key := Get_Keystroke;
      Exit_From_Program;
      Tcl.Ada.Tcl_Eval(Interpreter, "exit 1");
end Hunter;
