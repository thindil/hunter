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
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with DeleteItems; use DeleteItems;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with MainWindow; use MainWindow;
with Modules; use Modules;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;

procedure Hunter is
   use type Interfaces.C.int;

   Key: Key_Code := Key_None;
   Visibility: Cursor_Visibility := Invisible;
   ErrorFile: File_Type;
   ErrorFilePath: constant String :=
     Ada.Environment_Variables.Value("HOME") & "/.cache/hunter/error.log";
   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;
   UILocation: UI_Locations := DIRECTORY_VIEW;
   procedure ExitFromProgram is
   begin
      SavePreferences;
      Execute_Modules(On_Quit);
      if Settings.ClearTrashOnExit then
         NewAction := CLEARTRASH;
         if DeleteSelected(Interp) then
            null;
         end if;
      end if;
      End_Windows;
      InotifyClose;
      MagicClose;
   end ExitFromProgram;
begin
   -- Create needed directories
   Create_Path(Ada.Environment_Variables.Value("HOME") & "/.cache/hunter");
   Create_Path
     (Ada.Environment_Variables.Value("HOME") &
      "/.local/share/hunter/modules");
   -- Start libmagic data
   MagicOpen;
   -- Start inotify
   InotifyInit;
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
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("Hunter: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   -- Load required Tcl packages
   MsgCat_Init(Interp);

   -- Load the program setting
   LoadSettings;

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
      CreateMainWindow(Ada.Environment_Variables.Value("HOME"), Interp);
   else
      CreateMainWindow(Full_Name(Argument(1)), Interp);
   end if;

   -- Main program loop, exit on alt+q
   Main_Program_Loop:
   loop
      Key := Get_Keystroke;
      if Key = 27 then
         Key := Get_Keystroke;
         exit when Key = 113;
      end if;
      if Key in KEY_STAB | 9 then
         case UILocation is
            when DIRECTORY_VIEW =>
               UILocation := PATH_BUTTONS;
            when PATH_BUTTONS =>
               UILocation := MAIN_MENU;
            when MAIN_MENU =>
               UILocation := PREVIEW;
            when PREVIEW =>
               UILocation := DIRECTORY_VIEW;
            when others =>
               null;
         end case;
      else
         case UILocation is
            when DIRECTORY_VIEW =>
               Directory_Keys(Key);
            when PATH_BUTTONS =>
               UILocation := Path_Keys(Key);
            when MAIN_MENU =>
               UILocation := Menu_Keys(Key);
               exit when UILocation = PATH_BUTTONS;
            when ACTIONS_MENU =>
               UILocation := Actions_Keys(Key);
            when others =>
               null;
         end case;
      end if;
   end loop Main_Program_Loop;

   ExitFromProgram;
   Tcl.Ada.Tcl_Eval(Interp, "exit");
exception
   when An_Exception : others =>
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.cache/hunter");
      if Exists(ErrorFilePath) then
         Open(ErrorFile, Append_File, ErrorFilePath);
      else
         Create(ErrorFile, Append_File, ErrorFilePath);
      end if;
      Put_Line(ErrorFile, Current_Time);
      Put_Line(ErrorFile, "1.6");
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
      Tcl.Ada.Tcl_Eval(Interp, "exit 1");
end Hunter;
