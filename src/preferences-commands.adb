-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Utils; use Utils;

package body Preferences.Commands is

   -- ****it* Commands/CreateCommands
   -- FUNCTION
   -- Used to create Tcl commands
   -- SOURCE
   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   -- ****

   function Set_Label_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* Commands/Set_Label_Command
      -- FUNCTION
      -- Update text of the selected label
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Label_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      -- ****
      Label: Ttk_Label;
   begin
      Label.Interp := Interp;
      Label.Name :=
        New_String(".preferencesdialog." & CArgv.Arg(Argv, 1) & "label");
      if CArgv.Arg(Argv, 1) = "directory.interval" then
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {Auto refresh every " &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "updateinterval")))) &
            " seconds}");
      end if;
      return TCL_OK;
   end Set_Label_Command;

   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* Commands/Show_Preferences_Command
      -- FUNCTION
      -- Go to the selected bookmarked directory
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      PreferencesDialog: constant Tk_Toplevel :=
        Create(".preferencesdialog", "-class Dialog");
      LabelFrame: Ttk_LabelFrame;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      CloseButton: constant Ttk_Button :=
        Create
          (Widget_Image(PreferencesDialog) & ".closebutton",
           "-text {Close} -command {CloseDialog " &
           Widget_Image(PreferencesDialog) & "} -underline 0");
      CheckButton: Ttk_CheckButton;
      Label: Ttk_Label;
      Scale: Ttk_Scale;
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      LabelFrame :=
        Create
          (Widget_Image(PreferencesDialog) & ".directory",
           "-text {Directory Listing}");
      CheckButton :=
        Create
          (Widget_Image(LabelFrame) & ".showhidden",
           "-text {Show hidden files}");
      if Settings.ShowHidden then
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
      else
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
      end if;
      Add
        (CheckButton,
         "Show hidden files and directories in directory\nlisting and in directories preview.");
      Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      CheckButton :=
        Create
          (Widget_Image(LabelFrame) & ".showmodificationtime",
           "-text {Show modification time}");
      if Settings.ShowLastModified then
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
      else
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
      end if;
      Add
        (CheckButton,
         "Show the column with last modification\ndate for files and directories.");
      Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      Tcl_SetVar
        (CheckButton.Interp, "updateinterval",
         Natural'Image(Settings.AutoRefreshInterval));
      Label :=
        Create
          (Widget_Image(LabelFrame) & ".intervallabel",
           "-text ""Auto refresh every $updateinterval seconds""");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Widget_Image(LabelFrame) & ".intervalscale",
           "-from 0 -to 30 -variable updateinterval -orient horizontal -command {SetLabel directory.interval}");
      Add
        (Scale,
         "How often (in seconds) the program should check\nfor changes in current directory.\nIf set to zero, autorefresh will be disabled.");
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create(Widget_Image(PreferencesDialog) & ".preview", "-text Preview");
      CheckButton :=
        Create
          (Widget_Image(LabelFrame) & ".showpreview", "-text {Show preview}");
      if Settings.ShowPreview then
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
      else
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
      end if;
      Add
        (CheckButton,
         "Show second panel with preview of files and directories.\nIf you disable this option, second panel will be visible only during\ncopying and moving files or directories and during creating new link.");
      Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Bind
        (PreferencesDialog, "<Alt-c>",
         "{CloseDialog " & Widget_Image(PreferencesDialog) & "}");
      SetDialog(PreferencesDialog, "Hunter - Preferences", 300, 400);
      return TCL_OK;
   end Show_Preferences_Command;

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog: Tk_Toplevel;
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
   begin
      Dialog.Interp := Interp;
      Dialog.Name := New_String(CArgv.Arg(Argv, 1));
      Destroy(Dialog);
      if Winfo_Get(MainWindow, "exists") = "1"
        and then Status(MainWindow) = "1" then
         Forget(MainWindow);
      end if;
      return TCL_OK;
   end Close_Dialog_Command;

   procedure AddCommands is
      procedure AddCommand
        (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
         Command: Tcl.Tcl_Command;
      begin
         Command :=
           CreateCommands.Tcl_CreateCommand
             (Get_Context, Name, AdaCommand, 0, null);
         if Command = null then
            raise Program_Error with "Can't add command " & Name;
         end if;
      end AddCommand;
   begin
      AddCommand("ShowPreferences", Show_Preferences_Command'Access);
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("SetLabel", Set_Label_Command'Access);
   end AddCommands;

end Preferences.Commands;
