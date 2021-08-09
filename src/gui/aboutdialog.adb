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

with Interfaces.C;
with GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkNotebook;
with Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo;
with Utils;
with Utils.UI; use Utils.UI;

package body AboutDialog is

   -- ****o* AboutDialog/AboutDialog.Show_About_Command
   -- FUNCTION
   -- Show information about the program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAbout
   -- SOURCE
   function Show_About_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_About_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.MsgCat.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Toplevel;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkNotebook;
      use Tcl.Tk.Ada.Widgets.TtkTreeView;
      use Tcl.Tk.Ada.Winfo;
      use Utils;

      About_Dialog: constant Tk_Toplevel :=
        Create(pathName => ".aboutdialog", options => "-class Dialog");
      Close_Button: constant Ttk_Button :=
        Create
          (pathName => About_Dialog & ".closebutton",
           options =>
             "-text {" & Mc(Interp => Interp, Src_String => "Close") &
             "} -command {CloseDialog " & About_Dialog & "} -underline 0");
      Main_Window: constant Tk_Toplevel :=
        Get_Main_Window(Interp => Get_Context);
      Label: Ttk_Label;
      Frame: Ttk_Frame;
      Width: Width_Range;
      Height: Height_Range;
      Website_Button: Ttk_Button;
      Creditsbook: constant Ttk_Notebook :=
        Create(pathName => ".aboutdialog.credits");
      View: Ttk_Tree_View;
   begin
      if Tcl.Tk.Ada.Busy.Status(Window => Main_Window) = "0" then
         Tcl.Tk.Ada.Busy.Busy(Window => Main_Window);
      end if;
      Label :=
        Create(pathName => ".aboutdialog.logo", options => "-image logo");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Width :=
        Width_Range'Value(Winfo_Get(Widgt => Label, Info => "reqwidth"));
      Frame := Create(pathName => ".aboutdialog.general");
      Label :=
        Create
          (pathName => Frame & ".info",
           options =>
             "-text {" &
             Mc(Interp => Interp,
                Src_String => "{Hunter - Graphical file manager for Linux}") &
             "}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Width :=
        Width +
        Width_Range'Value(Winfo_Get(Widgt => Label, Info => "reqwidth"));
      Height :=
        Height_Range'Value(Winfo_Get(Widgt => Label, Info => "reqheight")) *
        17;
      Label :=
        Create
          (pathName => Frame & ".copyright",
           options => "-text {© Bartek Jasicki 2019-2021}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Label :=
        Create
          (pathName => Frame & ".license",
           options =>
             "-text {" & Mc(Interp => Interp, Src_String => "{License:}") &
             " GNU GPLv3}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Label :=
        Create
          (pathName => Frame & ".version",
           options =>
             "-text {" & Mc(Interp => Interp, Src_String => "{Version:}") &
             " 1.7 (" & Mc(Interp => Interp, Src_String => "{development}") &
             ")}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Website_Button :=
        Create
          (pathName => Frame & ".website",
           options =>
             "-text {" & Mc(Interp => Interp, Src_String => "Website") &
             "} -command {OpenLink https://www.laeran.pl/repositories/hunter/} -style Toolbutton");
      Tcl.Tk.Ada.Grid.Grid(Slave => Website_Button);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Frame, Options => "-row 0 -column 1 -sticky nwe");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Creditsbook, Options => "-columnspan 2 -sticky nwes");
      Frame := Create(pathName => Creditsbook & ".programmers");
      View :=
        Create
          (pathName => Frame & ".view",
           options => "-show tree -selectmode none -height 5");
      Column
        (TreeViewWidget => View, Col => "#0",
         Options => "-stretch true -width" & Width_Range'Image(Width - 50));
      Insert
        (TreeViewWidget => View,
         Options => "{} end -text {Bartek Jasicki <thindil@laeran.pl>}");
      Tcl.Tk.Ada.Grid.Grid(Slave => View, Options => "-sticky nwes");
      Add
        (Notebook => Creditsbook, WindowName => Widget_Image(Win => Frame),
         Options =>
           "-text {" & Mc(Interp => Interp, Src_String => "Programmers") &
           "}");
      Frame := Create(pathName => Creditsbook & ".translators");
      View :=
        Create
          (pathName => Frame & ".view",
           options => "-show tree -selectmode none -height 5");
      Column
        (TreeViewWidget => View, Col => "#0",
         Options => "-stretch true -width" & Width_Range'Image(Width - 50));
      Insert
        (TreeViewWidget => View,
         Options =>
           "{} end -text {Polski - Bartek Jasicki <thindil@laeran.pl>}");
      Tcl.Tk.Ada.Grid.Grid(Slave => View, Options => "-sticky nwes");
      Add
        (Notebook => Creditsbook, WindowName => Widget_Image(Win => Frame),
         Options =>
           "-text {" & Mc(Interp => Interp, Src_String => "Translators") &
           "}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Close_Button, Options => "-columnspan 2");
      Bind
        (Widgt => About_Dialog, Sequence => "<Alt-c>",
         Script => "{CloseDialog " & About_Dialog & "}");
      Set_Dialog
        (Dialog => About_Dialog,
         Dialog_Title =>
           Mc(Interp => Interp, Src_String => "{Hunter - About}"),
         Width => Width, Height => Height);
      return TCL_OK;
   end Show_About_Command;

   -- ****o* AboutDialog/AboutDialog.Open_Link_Command
   -- FUNCTION
   -- Open the selected link in the default web browser
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- OpenLink URL
   -- URL is the full URL (with http(s)://) to open
   -- SOURCE
   function Open_Link_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Open_Link_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use GNAT.OS_Lib;
      use Tcl.Ada;

      Os_Name: constant String :=
        Tcl_GetVar(interp => Interp, varName => "tcl_platform(os)");
      Command: constant String :=
        (if Os_Name = "Windows" then
           Locate_Exec_On_Path(Exec_Name => "start").all
         elsif Os_Name = "Darwin" then
           Locate_Exec_On_Path(Exec_Name => "open").all
         else Locate_Exec_On_Path(Exec_Name => "xdg-open").all);
      Pid: Process_Id;
   begin
      Pid :=
        Non_Blocking_Spawn
          (Program_Name => Command,
           Args =>
             Argument_String_To_List
               (Arg_String => CArgv.Arg(Argv => Argv, N => 1)).all);
      if Pid = Invalid_Pid then
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Open_Link_Command;

   procedure Create_About_Ui is
   begin
      Add_Command
        (Name => "ShowAbout", Ada_Command => Show_About_Command'Access);
      Add_Command(Name => "OpenLink", Ada_Command => Open_Link_Command'Access);
   end Create_About_Ui;

end AboutDialog;
