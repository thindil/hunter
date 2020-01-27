-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Text_IO;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with ErrorDialog; use ErrorDialog;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with RefreshData; use RefreshData;

procedure Hunter is
   use type Interfaces.C.int;

   package GetPackages is new Tcl.Ada.Generic_PkgRequire(Integer);

   Argc: CArgv.CNatural;
   Argv: CArgv.Chars_Ptr_Ptr;
   Interp: Tcl.Tcl_Interp;
   MainWindow: Tk_Toplevel;
begin
   if not Ada.Directories.Exists(Value("HOME") & "/.cache/hunter") then
      Create_Path(Value("HOME") & "/.cache/hunter");
   end if;
   if not Ada.Directories.Exists
       (Value("HOME") & "/.local/share/Trash/files") then
      Create_Path(Value("HOME") & "/.local/share/Trash/files");
   end if;
   if not Ada.Directories.Exists
       (Value("HOME") & "/.local/share/Trash/info") then
      Create_Path(Value("HOME") & "/.local/share/Trash/info");
   end if;
   -- Start libmagic data
   MagicOpen;
   -- Start inotify
   InotifyInit;
   -- Start Tk

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
        ("AzipTk: Tcl.Tcl_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Initialize Tk
   ----------------
   if Tcl.Tk.Tk_Init(Interp) = Tcl.TCL_ERROR then
      Ada.Text_IO.Put_Line
        ("AZipTk: Tcl.Tk.Tk_Init failed: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   --  Set the Tk context so that we may use shortcut Tk
   --  calls that require reference to the interpreter.
   ----------------------------------------------------
   Set_Context(Interp);

   -- Load required Tcl packages
   if GetPackages.Tcl_PkgRequireEx(Interp, "tooltip", "1.4.6", 0, null)'
       Length =
     0 then
      Ada.Text_IO.Put_Line
        ("Failed to load tooltip package: " &
         Tcl.Ada.Tcl_GetStringResult(Interp));
      return;
   end if;

   -- Create UI
   MainWindow := Get_Main_Window(Interp);
   Wm_Set(MainWindow, "title", "Hunter");
   Bind_To_Main_Window(Interp, "<Control-q>", "{exit}");

   --  Loop inside Tk, waiting for commands to execute.
   --  When there are no windows left, Tcl.Tk.Tk_MainLoop returns and we exit.
   --------------------------------------------------------------------------
   Tcl.Tk.Tk_MainLoop;

   -- Close everything and quit
   abort InotifyTask;
   InotifyClose;
   MagicClose;

exception
   when An_Exception : others =>
      SaveException(An_Exception);
end Hunter;
