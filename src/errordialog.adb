-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Interfaces.C;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Utils; use Utils;

package body ErrorDialog is

   procedure SaveException(An_Exception: Exception_Occurrence) is
      use type Interfaces.C.int;

      ErrorFile: File_Type;
      ErrorText: Unbounded_String;
      ErrorFilePath: constant String :=
        Value("HOME") & "/.cache/hunter/error.log";
      Interp: Tcl.Tcl_Interp := Get_Context;
      MainWindow: Tk_Toplevel := Get_Main_Window(Interp);
   begin
      if Ada.Directories.Exists(ErrorFilePath) then
         Open(ErrorFile, Append_File, ErrorFilePath);
      else
         Create(ErrorFile, Append_File, ErrorFilePath);
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, LF);
      Append(ErrorText, "1.3");
      Append(ErrorText, LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Append(ErrorText, LF);
      Append(ErrorText, Symbolic_Traceback(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      Destroy(MainWindow);
      Interp := Tcl.Tcl_CreateInterp;
      if Tcl.Tcl_Init(Interp) = Tcl.TCL_ERROR then
         Ada.Text_IO.Put_Line
           ("Hunter: Tcl.Tcl_Init failed: " &
            Tcl.Ada.Tcl_GetStringResult(Interp));
         return;
      end if;
      if Tcl.Tk.Tk_Init(Interp) = Tcl.TCL_ERROR then
         Ada.Text_IO.Put_Line
           ("Hunter: Tcl.Tk.Tk_Init failed: " &
            Tcl.Ada.Tcl_GetStringResult(Interp));
         return;
      end if;
      Set_Context(Interp);
      declare
         ErrorLabel: constant Ttk_Label :=
           Create
             (".errorlabel",
              "-text ""Oops, something bad happens and progam crashed. Please, remember what you done before crash and report this problem at:"" -wraplength 800");
         ErrorButton: constant Ttk_Button :=
           Create
             (".errorbutton",
              "-text ""https://github.com/thindil/hunter/issues"" -command {exec " &
              FindExecutable("xdg-open") &
              " ""https://github.com/thindil/hunter/issues""}");
         ErrorLabel2: constant Ttk_Label :=
           Create
             (".errorlabel2",
              "-text {and attach (if possible) file 'error.log' from" &
              Value("HOME") & "/.cache/hunter' directory.} -wraplength 800");
         CloseButton: constant Ttk_Button :=
           Create(".closebutton", "-text Close -command exit");
         ErrorFrame: constant Ttk_LabelFrame :=
           Create(".errorframe", "-text {Technical information}");
         ErrorInfo: constant Tk_Text :=
           Create
             (".errorframe.errorinfo",
              "-wrap word -yscrollcommand [list .errorframe.scroll set]");
         ErrorScroll: constant Ttk_Scrollbar :=
           Create
             (".errorframe.scroll",
              "-orient vertical -command [list .errorframe.errorinfo yview]");
      begin
         MainWindow := Get_Main_Window(Interp);
         Wm_Set(MainWindow, "title", "{Hunter - error}");
         Wm_Set
           (MainWindow, "geometry",
            "800x600+[expr ([winfo vrootwidth .] - 800) / 2]+[expr ([winfo vrootheight .] - 600) / 2]");
         Tcl.Tk.Ada.Pack.Pack(ErrorLabel);
         Tcl.Tk.Ada.Pack.Pack(ErrorButton);
         Tcl.Tk.Ada.Pack.Pack(ErrorLabel2);
         Tcl.Tk.Ada.Pack.Pack(CloseButton);
         Tcl.Tk.Ada.Pack.Pack(ErrorFrame, "-fill both -expand true");
         Tcl.Tk.Ada.Pack.Pack(ErrorScroll, "-fill y -side right");
         Tcl.Tk.Ada.Pack.Pack(ErrorInfo, "-side top -fill both -expand true");
         Insert(ErrorInfo, "end", "{" & To_String(ErrorText) & "}");
         configure(ErrorInfo, "-state disabled");
         Tcl.Tk.Tk_MainLoop;
      end;
   end SaveException;

end ErrorDialog;
