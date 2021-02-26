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

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with GNAT.Time_Stamp;
with GNAT.Traceback.Symbolic;
with Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Wm;
with Utils.UI;

package body ErrorDialog is

   procedure Save_Exception(An_Exception: Exception_Occurrence) is
      use type Interfaces.C.int;
      use Ada.Characters.Latin_1;
      use Ada.Command_Line;
      use Ada.Environment_Variables;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use GNAT.Time_Stamp;
      use GNAT.Traceback.Symbolic;
      use Tcl.MsgCat.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Toplevel;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;

      Error_File: File_Type;
      Error_Text: Unbounded_String := Null_Unbounded_String;
      Error_File_Path: constant String :=
        Value(Name => "HOME") & "/.cache/hunter/error.log";
      Interp: Tcl.Tcl_Interp := Get_Context;
      Program_Main_Window: Tk_Toplevel := Get_Main_Window(Interp => Interp);
   begin
      Ada.Directories.Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") & "/.cache/hunter");
      if Ada.Directories.Exists(Name => Error_File_Path) then
         Open
           (File => Error_File, Mode => Append_File, Name => Error_File_Path);
      else
         Create
           (File => Error_File, Mode => Append_File, Name => Error_File_Path);
      end if;
      Append(Source => Error_Text, New_Item => Current_Time & LF);
      Append(Source => Error_Text, New_Item => "1.6" & LF);
      Append
        (Source => Error_Text,
         New_Item => "Exception: " & Exception_Name(X => An_Exception) & LF);
      Append
        (Source => Error_Text,
         New_Item => "Message: " & Exception_Message(X => An_Exception) & LF);
      Append
        (Source => Error_Text,
         New_Item => "-------------------------------------------------" & LF);
      Append
        (Source => Error_Text,
         New_Item => Symbolic_Traceback(E => An_Exception) & LF);
      Append
        (Source => Error_Text,
         New_Item => "-------------------------------------------------");
      Put_Line(File => Error_File, Item => To_String(Source => Error_Text));
      Close(File => Error_File);
      Destroy(Widgt => Program_Main_Window);
      Interp := Tcl.Tcl_CreateInterp;
      if Tcl.Tcl_Init(interp => Interp) = Tcl.TCL_ERROR then
         Ada.Text_IO.Put_Line
           (Item =>
              "Hunter: Tcl.Tcl_Init failed: " &
              Tcl.Ada.Tcl_GetStringResult(interp => Interp));
         return;
      end if;
      if Tcl.Tk.Tk_Init(interp => Interp) = Tcl.TCL_ERROR then
         Ada.Text_IO.Put_Line
           (Item =>
              "Hunter: Tcl.Tk.Tk_Init failed: " &
              Tcl.Ada.Tcl_GetStringResult(interp => Interp));
         return;
      end if;
      Set_Context(Interp => Interp);
      MsgCat_Init(Interp => Interp);
      Ada.Directories.Set_Directory
        (Directory =>
           Ada.Directories.Containing_Directory(Name => Command_Name));
      Mc_Load(DirName => "../share/hunter/translations", Interp => Interp);
      Show_Error_Window_Block :
      declare
         use Tcl.Tk.Ada.Widgets.Text;
         use Tcl.Tk.Ada.Widgets.TtkButton;
         use Tcl.Tk.Ada.Widgets.TtkLabel;
         use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
         use Tcl.Tk.Ada.Widgets.TtkScrollbar;
         use Tcl.Tk.Ada.Wm;
         use Utils.UI;

         Error_Label: constant Ttk_Label :=
           Create
             (pathName => ".errorlabel",
              options =>
                "-text {" &
                Mc(Interp => Interp,
                   Src_String =>
                     "{Oops, something bad happens and progam crashed. Please, remember what have you done before crash and report this problem at:}") &
                "} -wraplength 800");
         Error_Button: constant Ttk_Button :=
           Create
             (pathName => ".errorbutton",
              options =>
                "-text ""https://www.laeran.pl/repositories/hunter/ticket"" -command {exec " &
                FindExecutable(Name => "xdg-open") &
                " ""https://www.laeran.pl/repositories/hunter/ticket""}");
         Error_Label2: constant Ttk_Label :=
           Create
             (pathName => ".errorlabel2",
              options =>
                "-text {" &
                Mc(Interp => Interp,
                   Src_String =>
                     "{and attach (if possible) file 'error.log' from '}") &
                Value(Name => "HOME") &
                "/.cache/hunter' directory.} -wraplength 800");
         Close_Button: constant Ttk_Button :=
           Create
             (pathName => ".closebutton",
              options =>
                "-text " & Mc(Interp => Interp, Src_String => "{Close}") &
                " -command exit");
         Error_Frame: constant Ttk_LabelFrame :=
           Create
             (pathName => ".errorframe",
              options =>
                "-text {" &
                Mc(Interp => Interp, Src_String => "{Technical information}") &
                "}");
         Error_Info: constant Tk_Text :=
           Create
             (pathName => ".errorframe.errorinfo",
              options =>
                "-wrap word -yscrollcommand [list .errorframe.scroll set]");
         Error_Scroll: constant Ttk_Scrollbar :=
           Create
             (pathName => ".errorframe.scroll",
              options =>
                "-orient vertical -command [list .errorframe.errorinfo yview]");
      begin
         Program_Main_Window := Get_Main_Window(Interp => Interp);
         Wm_Set
           (Widgt => Program_Main_Window, Action => "title",
            Options => "{Hunter - error}");
         Wm_Set
           (Widgt => Program_Main_Window, Action => "geometry",
            Options =>
              "800x600+[expr ([winfo vrootwidth .] - 800) / 2]+[expr ([winfo vrootheight .] - 600) / 2]");
         Tcl.Tk.Ada.Pack.Pack(Slave => Error_Label);
         Tcl.Tk.Ada.Pack.Pack(Slave => Error_Button);
         Tcl.Tk.Ada.Pack.Pack(Slave => Error_Label2);
         Tcl.Tk.Ada.Pack.Pack(Slave => Close_Button);
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Error_Frame, Options => "-fill both -expand true");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Error_Scroll, Options => "-fill y -side right");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Error_Info,
            Options => "-side top -fill both -expand true");
         Insert
           (TextWidget => Error_Info, Index => "end",
            Text => "{" & To_String(Source => Error_Text) & "}");
         configure(Widgt => Error_Info, options => "-state disabled");
         Tcl.Tk.Tk_MainLoop;
      end Show_Error_Window_Block;
   end Save_Exception;

end ErrorDialog;
