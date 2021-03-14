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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with ErrorDialog; use ErrorDialog;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with MainWindow; use MainWindow;
with Preferences; use Preferences;

procedure Hunter is
   use type Interfaces.C.int;

   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;
begin
   -- Create needed directories
   Create_Path(New_Directory => Value(Name => "HOME") & "/.cache/hunter");
   Create_Path
     (New_Directory => Value(Name => "HOME") & "/.local/share/hunter/modules");
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
   Interp := Tcl.Tcl_CreateInterp;

   --  Initialize Tcl
   -----------------
   if Tcl.Tcl_Init(interp => Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        (Item =>
           "Hunter: Tcl.Tcl_Init failed: " &
           Tcl.Ada.Tcl_GetStringResult(interp => Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init(interp => Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        (Item =>
           "Hunter: Tcl.Tk.Tk_Init failed: " &
           Tcl.Ada.Tcl_GetStringResult(interp => Interp));
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Set_Context(Interp => Interp);

   -- Load required Tcl packages
   Tooltip_Init(Interp => Interp);
   Tcl.Ada.Tcl_Eval(interp => Interp, strng => "package require Img");
   Tcl.Ada.Tcl_Eval(interp => Interp, strng => "package require tksvg");
   MsgCat_Init(Interp => Interp);
   Autoscroll_Init(Interp => Interp);

   -- Load the program setting
   LoadSettings;

   -- Create the program main window
   if Argument_Count < 1 then
      Create_Main_Window(Directory => Value(Name => "HOME"));
   else
      Create_Main_Window(Directory => Full_Name(Name => Argument(Number => 1)));
   end if;

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

exception
   when An_Exception : others =>
      Save_Exception(An_Exception => An_Exception);
end Hunter;
