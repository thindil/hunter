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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkNotebook; use Tcl.Tk.Ada.Widgets.TtkNotebook;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Utils; use Utils;

package body AboutDialog is

   -- ****if* AboutDialog/Show_About_Command
   -- FUNCTION
   -- Show information about the program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      AboutDialog: constant Tk_Toplevel :=
        Create(".aboutdialog", "-class Dialog");
      CloseButton: constant Ttk_Button :=
        Create
          (Widget_Image(AboutDialog) & ".closebutton",
           "-text {" & Mc(Interp, "Close") & "} -command {CloseDialog " &
           Widget_Image(AboutDialog) & "} -underline 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      Label: Ttk_Label;
      Frame: Ttk_Frame;
      Width, Height: Positive;
      WebsiteButton: Ttk_Button;
      Creditsbook: constant Ttk_Notebook := Create(".aboutdialog.credits");
      View: Ttk_Tree_View;
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      Label := Create(".aboutdialog.logo", "-image logo");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Width := Positive'Value(Winfo_Get(Label, "reqwidth"));
      Frame := Create(".aboutdialog.general");
      Label :=
        Create
          (Widget_Image(Frame) & ".info",
           "-text {" &
           Mc(Interp, "{Hunter - Graphical file manager for Linux}") & "}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Width := Width + Positive'Value(Winfo_Get(Label, "reqwidth"));
      Height := Positive'Value(Winfo_Get(Label, "reqheight")) * 15;
      Label :=
        Create
          (Widget_Image(Frame) & ".copyright",
           "-text {© Bartek Jasicki 2019-2020}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label :=
        Create
          (Widget_Image(Frame) & ".license",
           "-text {" & Mc(Interp, "{License:}") & " GNU GPLv3}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label :=
        Create
          (Widget_Image(Frame) & ".version",
           "-text {" & Mc(Interp, "{Version:}") & " 1.4 (" &
           Mc(Interp, "{development}") & ")}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      WebsiteButton :=
        Create
          (Widget_Image(Frame) & ".website",
           "-text {" & Mc(Interp, "Website") &
           "} -command {OpenLink http://thindil.github.io/hunter/} -style Toolbutton");
      Tcl.Tk.Ada.Grid.Grid(WebsiteButton);
      Tcl.Tk.Ada.Grid.Grid(Frame, "-row 0 -column 1 -sticky nwe");
      Tcl.Tk.Ada.Grid.Grid(Creditsbook, "-columnspan 2 -sticky nwes");
      Frame := Create(Widget_Image(Creditsbook) & ".programmers");
      View :=
        Create
          (Widget_Image(Frame) & ".view",
           "-show tree -selectmode none -height 5");
      Column(View, "#0", "-stretch true -width" & Positive'Image(Width - 50));
      Insert(View, "{} end -text {Bartek Jasicki <thindil@laeran.pl>}");
      Tcl.Tk.Ada.Grid.Grid(View, "-sticky nwes");
      Add
        (Creditsbook, Widget_Image(Frame),
         "-text {" & Mc(Interp, "Programmers") & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 2");
      Bind
        (AboutDialog, "<Alt-c>",
         "{CloseDialog " & Widget_Image(AboutDialog) & "}");
      SetDialog(AboutDialog, Mc(Interp, "{Hunter - About}"), Width, Height);
      return TCL_OK;
   end Show_About_Command;

   -- ****if* AboutDialog/Open_Link_Command
   -- FUNCTION
   -- Open the selected link in the default web browser
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Open_Link_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      OsName: constant String := Tcl_GetVar(Get_Context, "tcl_platform(os)");
      Command: Unbounded_String;
      ProcessId: Process_Id;
      Azip_Execute_Error: exception;
   begin
      if OsName = "Windows" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("start").all);
      elsif OsName = "Linux" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("xdg-open").all);
      elsif OsName = "Darwin" then
         Command := To_Unbounded_String(Locate_Exec_On_Path("open").all);
      end if;
      ProcessId :=
        Non_Blocking_Spawn
          (To_String(Command),
           Argument_String_To_List(CArgv.Arg(Argv, 1)).all);
      if ProcessId = Invalid_Pid then
         raise Azip_Execute_Error with Mc(Interp, "{Can't open link}");
      end if;
      return TCL_OK;
   end Open_Link_Command;

   procedure CreateAboutUI is
   begin
      AddCommand("ShowAbout", Show_About_Command'Access);
      AddCommand("OpenLink", Open_Link_Command'Access);
   end CreateAboutUI;

end AboutDialog;
