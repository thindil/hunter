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

--with Ada.Directories; use Ada.Directories;
--with Ada.Environment_Variables; use Ada.Environment_Variables;
--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Utils; use Utils;

package body AboutDialog is

--   procedure ShowAboutDialog(Parent: Gtk_Window) is
--      AboutDialog: constant Gtk_About_Dialog := Gtk_About_Dialog_New;
--      Error: GError;
--      LogoBuf: Gdk_Pixbuf;
--      IconName: Unbounded_String;
--   begin
--      Set_Transient_For(Gtk_Window(AboutDialog), Parent);
--      Set_Program_Name(AboutDialog, "Hunter");
--      Set_License_Type(AboutDialog, License_Gpl_3_0);
--      Set_Comments(AboutDialog, "Graphical File Manager for Linux");
--      Set_Copyright(AboutDialog, "(c) 2019 Bartek thindil Jasicki");
--      Set_Authors
--        (AboutDialog,
--         (new String'("Bartek thindil Jasicki <thindil@laeran.pl>"),
--          new String'("")));
--      Set_Translator_Credits(AboutDialog, Gettext("translator-credits"));
--      if Ada.Directories.Exists
--          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
--         IconName :=
--           To_Unbounded_String(Value("APPDIR", "") & "/hunter-icon.png");
--      else
--         IconName :=
--           To_Unbounded_String
--             (Containing_Directory(Current_Directory) &
--              "/others/hunter-icon.png");
--      end if;
--      Gdk_New_From_File(LogoBuf, To_String(IconName), Error);
--      Set_Logo(AboutDialog, LogoBuf);
--      Set_Version(AboutDialog, "1.2.1");
--      Set_Website(AboutDialog, "https://thindil.github.io/hunter/");
--      Set_Website_Label(AboutDialog, "Website");
--      if Run(Gtk_Dialog(AboutDialog)) = Gtk_Response_Delete_Event then
--         Destroy(Gtk_Widget(AboutDialog));
--      end if;
--   end ShowAboutDialog;

   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* AboutDialog/Show_About_Command
      -- FUNCTION
      -- Show information about the program
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Show_About_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      AboutDialog: constant Tk_Toplevel :=
        Create(".aboutdialog", "-class Dialog");
      CloseButton: constant Ttk_Button :=
        Create
          (Widget_Image(AboutDialog) & ".closebutton",
           "-text {Close} -command {CloseDialog " &
           Widget_Image(AboutDialog) & "} -underline 0");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      Tcl.Tk.Ada.Grid.Grid(CloseButton);
      Bind
        (AboutDialog, "<Alt-c>",
         "{CloseDialog " & Widget_Image(AboutDialog) & "}");
      SetDialog(AboutDialog, "Hunter - About", 400, 300);
      return TCL_OK;
   end Show_About_Command;

   procedure CreateAboutUI is
   begin
      AddCommand("ShowAbout", Show_About_Command'Access);
   end CreateAboutUI;

end AboutDialog;
