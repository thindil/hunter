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
with CreateItems.UI; use CreateItems.UI;
with DeleteItems; use DeleteItems;
with DeleteItems.UI; use DeleteItems.UI;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Modules; use Modules;
with Preferences; use Preferences;
with Preferences.UI; use Preferences.UI;
with ProgramsMenu; use ProgramsMenu;
with RenameItems; use RenameItems;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;

procedure Hunter is
   use type Interfaces.C.int;

   Key: Key_Code := Key_None;
   Visibility: Cursor_Visibility := Invisible;
   ErrorFile: File_Type;
   ErrorFilePath: constant String :=
     Ada.Environment_Variables.Value("HOME") & "/.cache/hunter/error.log";
   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   AltKey: Boolean;
   procedure ExitFromProgram is
   begin
      Save_Preferences;
      Execute_Modules(Interpreter, On_Quit);
      if Settings.Clear_Trash_On_Exit then
         New_Action := CLEARTRASH;
         if Delete_Selected(Interpreter) then
            null;
         end if;
      end if;
      End_Windows;
      Inotify_Close;
      Magic_Close;
      Delete_File
        (Ada.Environment_Variables.Value("HOME") &
         "/.cache/hunter/highlight.tmp");
   exception
      when Ada.Directories.Name_Error =>
         null;
   end ExitFromProgram;
begin
   -- Create needed directories
   Create_Path(Ada.Environment_Variables.Value("HOME") & "/.cache/hunter");
   Create_Path
     (Ada.Environment_Variables.Value("HOME") &
      "/.local/share/hunter/modules");
   -- Start libmagic data
   Magic_Open;
   -- Start inotify
   Inotify_Init;
   -- Start Tcl/Tk

   --  Get command-line arguments and put them into C-style "argv"
   --------------------------------------------------------------
   CArgv.Create(Argc, Argv);

   --  Tcl needs to know the path name of the executable
   --  otherwise Tcl.Tcl_Init below will fail.
   ----------------------------------------------------
   Tcl.Tcl_FindExecutable(Argv.all);

   --  Create one Tcl interpreter
   -----------------------------
   Interpreter := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(Interpreter) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("Hunter: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interpreter));
      return;
   end if;

   -- Load required Tcl packages
   MsgCat_Init(Interpreter);

   -- Load the program setting
   Load_Settings;

   -- Load the available applications list
   CreateProgramsMenu;

   -- Initialize ncurses
   Ada.Environment_Variables.Set("ESCDELAY", "10");
   Ada.Environment_Variables.Set("TERMINFO", "terminfo");
   Init_Screen;
   Start_Color;
   for I in 1 .. 16 loop
      Init_Color(Color_Number(I + 7), 0, 0, 0);
      Init_Pair(Color_Pair(I), Color_Number(I + 7), Black);
   end loop;
   Set_Timeout_Mode(Standard_Window, Blocking, 0);
   Set_Echo_Mode(False);
   Set_Cursor_Visibility(Visibility);

   -- Create the program main window
   if Argument_Count < 1 then
      CreateMainWindow(Ada.Environment_Variables.Value("HOME"));
   else
      CreateMainWindow(Full_Name(Argument(1)));
   end if;

   -- Main program loop, exit on alt+q
   Main_Program_Loop :
   loop
      Key := Get_Keystroke;
      AltKey := False;
      -- Escape key pressed
      if Key = 27 then
         Key := Get_Keystroke;
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
         elsif Key > 1 then
            AltKey := True;
         end if;
         exit Main_Program_Loop when Key = 113;
      end if;
      if Key in KEY_STAB | 9 then
         case UILocation is
            when DIRECTORY_VIEW =>
               UILocation := PATH_BUTTONS;
            when PATH_BUTTONS =>
               UILocation := MAIN_MENU;
            when MAIN_MENU =>
               if New_Action in COPY | MOVE | CREATELINK then
                  UILocation := DESTINATION_VIEW;
               else
                  if Info_Form /= Null_Form then
                     Visibility := Normal;
                     Set_Cursor_Visibility(Visibility);
                  end if;
                  UILocation := PREVIEW;
               end if;
            when DESTINATION_VIEW =>
               UILocation := DESTINATION_PATH;
            when PREVIEW =>
               UILocation := DIRECTORY_VIEW;
               Visibility := Invisible;
               Set_Cursor_Visibility(Visibility);
               begin
                  Delete_File
                    ((Ada.Environment_Variables.Value("HOME") &
                      "/.cache/hunter/highlight.tmp"));
               exception
                  when Ada.Directories.Name_Error =>
                     null;
               end;
            when DESTINATION_PATH =>
               UILocation := DIRECTORY_VIEW;
            when others =>
               null;
         end case;
      else
         case UILocation is
            when DIRECTORY_VIEW =>
               UILocation := Directory_Keys(Key);
            when PATH_BUTTONS =>
               UILocation := Path_Keys(Key);
            when MAIN_MENU =>
               UILocation := Menu_Keys(Key);
               exit Main_Program_Loop when UILocation = PATH_BUTTONS;
            when ACTIONS_MENU =>
               UILocation := Actions_Keys(Key);
            when CREATE_FORM =>
               UILocation := Create_Keys(Key);
            when DELETE_FORM =>
               UILocation := Delete_Keys(Key);
            when MESSAGE_FORM =>
               UILocation := Message_Keys(Key);
            when RENAME_FORM =>
               UILocation := Rename_Keys(Key);
            when DESTINATION_VIEW =>
               Destination_Keys(Key);
            when DESTINATION_PATH =>
               UILocation := Destination_Path_Keys(Key);
            when BOOKMARKS_MENU =>
               UILocation := Bookmarks_Keys(Key);
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
               UILocation := Set_Shortcut_Keys(Key, AltKey);
            when COMMAND_FORM =>
               UILocation := Add_Command_Keys(Key);
            when COMMANDS_MENU =>
               UILocation := User_Commands_Keys(Key);
         end case;
      end if;
   end loop Main_Program_Loop;

   ExitFromProgram;
   Tcl.Ada.Tcl_Eval(Interpreter, "exit");
exception
   when An_Exception : others =>
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.cache/hunter");
      if Exists(ErrorFilePath) then
         Open(ErrorFile, Append_File, ErrorFilePath);
      else
         Create(ErrorFile, Append_File, ErrorFilePath);
      end if;
      Put_Line(ErrorFile, Current_Time);
      Put_Line(ErrorFile, Version_Number);
      Put_Line(ErrorFile, "Exception: " & Exception_Name(An_Exception));
      Put_Line(ErrorFile, "Message: " & Exception_Message(An_Exception));
      Put_Line(ErrorFile, "-------------------------------------------------");
      Put_Line(ErrorFile, Symbolic_Traceback(An_Exception));
      Put_Line(ErrorFile, "-------------------------------------------------");
      Close(ErrorFile);
      Erase;
      Refresh;
      Move_Cursor(Line => (Lines / 2), Column => 2);
      Add
        (Str =>
           "Oops, something bad happens and progam crashed. Please, remember what have you done before crash and report this problem at https://www.laeran.pl/repositories/hunter/ticket and attach (if possible) file 'error.log' from '" &
           Ada.Environment_Variables.Value("HOME") &
           "/.cache/hunter' directory.");
      Key := Get_Keystroke;
      ExitFromProgram;
      Tcl.Ada.Tcl_Eval(Interpreter, "exit 1");
end Hunter;
