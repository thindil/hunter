-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with Modules; use Modules;
with Preferences.UI; use Preferences.UI;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Toolbars; use Toolbars;
with UserCommands; use UserCommands;
with UserCommands.UI; use UserCommands.UI;
with Utils; use Utils;

package body Preferences.Commands is

   -- ****o* PCommands/PCommands.Set_Label_Command
   -- FUNCTION
   -- Update text of the selected label
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetLabel labelname
   -- Labelname is the partial pathname of the label which text will be
   -- updated. Possible values are directory.interval and
   -- inteface.messagesscale
   -- SOURCE
   function Set_Label_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Label_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Label: constant Ttk_Label :=
        Get_Widget
          (".preferencesframe.canvas.notebook.preferences." &
           CArgv.Arg(Argv, 1) & "label",
           Interp);
   begin
      if CArgv.Arg(Argv, 1) = "directory.interval" then
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {" & Mc(Interp, "{Auto refresh every}") &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "updateinterval")))) &
            " " & Mc(Interp, "{seconds}") & "}");
      else
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {" & Mc(Interp, "{Hide messages after}") &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "messagesinterval")))) &
            " " & Mc(Interp, "{seconds}") & "}");
      end if;
      return TCL_OK;
   end Set_Label_Command;

   -- ****o* PCommands/PCommands.Set_Show_Hidden_Files_Command
   -- FUNCTION
   -- Update show hidden files setting and reload the current directory
   -- listing
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShowHiddenFiles
   -- SOURCE
   function Set_Show_Hidden_Files_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Hidden_Files_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      Settings.Show_Hidden :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.directory.showhidden") =
           "0"
         then False
         else True);
      Update_Directory_List(True);
      return TCL_OK;
   end Set_Show_Hidden_Files_Command;

   -- ****o* PCommands/PCommands.Set_Show_Modification_Time_Command
   -- FUNCTION
   -- Update show modification time column and reload the current directory
   -- listing
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShowModificationTime
   -- SOURCE
   function Set_Show_Modification_Time_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Modification_Time_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DirectoryTree: constant Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.directoryframe.directorytree", Interp);
   begin
      if Tcl_GetVar
          (Interp,
           ".preferencesframe.canvas.notebook.preferences.directory.showmodificationtime") =
        "0" then
         Settings.Show_Last_Modified := False;
         Tcl.Tk.Ada.Widgets.configure
           (DirectoryTree, "-displaycolumns [list name size]");
      else
         Settings.Show_Last_Modified := True;
         Tcl.Tk.Ada.Widgets.configure(DirectoryTree, "-displaycolumns #all");
      end if;
      return TCL_OK;
   end Set_Show_Modification_Time_Command;

   -- ****o* PCommands/PCommands.Set_Show_Preview_Command
   -- FUNCTION
   -- Update show preview setting and show or hide it
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShowPreview
   -- SOURCE
   function Set_Show_Preview_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Preview_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".mainframe.paned", Interp);
      PreviewFrame: constant Ttk_Frame :=
        Get_Widget(Paned & ".previewframe", Interp);
   begin
      if Tcl_GetVar
          (Interp,
           ".preferencesframe.canvas.notebook.preferences.preview.showpreview") =
        "0" then
         Settings.Show_Preview := False;
         Forget(Paned, PreviewFrame);
         return TCL_OK;
      end if;
      Settings.Show_Preview := True;
      Add(Paned, PreviewFrame, "-weight 20");
      return TCL_OK;
   end Set_Show_Preview_Command;

   -- ****o* PCommands/PCommands.Set_Scale_Images_Command
   -- FUNCTION
   -- Enable or disable images scaling and rescale currently
   -- previewed image if needed
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetScaleImages
   -- SOURCE
   function Set_Scale_Images_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Scale_Images_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MimeType: constant String := Get_Mime_Type(To_String(Current_Selected));
   begin
      Settings.Scale_Images :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.preview.scaleimages") =
           "0"
         then False
         else True);
      if MimeType(1 .. 5) = "image" then
         Show_Preview;
      end if;
      return TCL_OK;
   end Set_Scale_Images_Command;

   -- ****o* PCommands/PCommands.Set_Color_Text_Command
   -- FUNCTION
   -- Enable or disable syntax highlightning in text files and reload the
   -- currently previewed file if it is text file
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetColorText
   -- SOURCE
   function Set_Color_Text_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Color_Text_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget
          (".preferencesframe.canvas.notebook.preferences.preview.colorframe.highlighttheme",
           Interp);
      MimeType: constant String := Get_Mime_Type(To_String(Current_Selected));
   begin
      if Tcl_GetVar
          (Interp,
           ".preferencesframe.canvas.notebook.preferences.preview.syntaxhighlightning") =
        "0" then
         Settings.Color_Text := False;
         State(ComboBox, "disabled");
      else
         Settings.Color_Text := True;
         State(ComboBox, "!disabled");
      end if;
      if MimeType(1 .. 4) = "text" then
         Show_Preview;
      end if;
      return TCL_OK;
   end Set_Color_Text_Command;

   -- ****o* PCommands/PCommands.Set_Color_Theme_Command
   -- FUNCTION
   -- Set color theme for syntax highligthning in text files and reload
   -- the current previewed text file if needed
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetColorTheme
   -- SOURCE
   function Set_Color_Theme_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Color_Theme_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget
          (".preferencesframe.canvas.notebook.preferences.preview.colorframe.highlighttheme",
           Interp);
      MimeType: constant String := Get_Mime_Type(To_String(Current_Selected));
   begin
      Settings.Color_Theme := To_Unbounded_String(Get(ComboBox));
      if MimeType(1 .. 4) = "text" then
         Show_Preview;
      end if;
      return TCL_OK;
   end Set_Color_Theme_Command;

   -- ****o* PCommands/PCommands.Set_Monospace_Font_Command
   -- FUNCTION
   -- Enable or disable using monospace font in text files and reload the
   -- currently previewed file if it is text file
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMonoSpaceFont
   -- SOURCE
   function Set_Monospace_Font_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Monospace_Font_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      PreviewText: constant Ttk_Frame :=
        Get_Widget(".mainframe.paned.previewframe.previewtext", Interp);
   begin
      if Tcl_GetVar
          (Interp,
           ".preferencesframe.canvas.notebook.preferences.preview.monospacefont") =
        "0" then
         Settings.Monospace_Font := False;
         Widgets.configure(PreviewText, "-font TkDefaultFont");
      else
         Settings.Monospace_Font := True;
         Widgets.configure(PreviewText, "-font TkFixedFont");
      end if;
      return TCL_OK;
   end Set_Monospace_Font_Command;

   -- ****o* PCommands/PCommands.Set_Stay_In_Old_Command
   -- FUNCTION
   -- Set if after copying, moving, etc operations user should
   -- see old directory or new
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetStayInOld
   -- SOURCE
   function Set_Stay_In_Old_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Stay_In_Old_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      Settings.Stay_In_Old :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.interface.stayinold") =
           "0"
         then False
         else True);
      return TCL_OK;
   end Set_Stay_In_Old_Command;

   -- ****o* PCommands/PCommands.Set_Show_Finished_Info_Command
   -- FUNCTION
   -- Set if after finishing action, show info about it to the
   -- user
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShowFinishedInfo
   -- SOURCE
   function Set_Show_Finished_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Finished_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      Settings.Show_Finished_Info :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.interface.showfinished") =
           "0"
         then False
         else True);
      return TCL_OK;
   end Set_Show_Finished_Info_Command;

   -- ****o* PCommands/PCommands.Set_Toolbars_On_Top_Command
   -- FUNCTION
   -- Set if toolbars should be on top of the program's window or on the
   -- left
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetToolbarsOnTop
   -- SOURCE
   function Set_Toolbars_On_Top_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Toolbars_On_Top_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MainFrame: constant Ttk_Frame := Get_Widget(".mainframe", Interp);
      Paned: constant Ttk_Frame := Get_Widget(MainFrame & ".paned", Interp);
   begin
      Settings.Toolbars_On_Top :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.interface.toolbarsontop") =
           "0"
         then False
         else True);
      Set_Toolbars;
      if not Settings.Toolbars_On_Top then
         Tcl.Tk.Ada.Grid.Grid_Configure
           (Paned, "-column 1 -row 3 -sticky nswe");
         Grid.Column_Configure(MainFrame, Paned, "-weight 1");
         Grid.Row_Configure(MainFrame, Paned, "-weight 1");
      else
         Tcl.Tk.Ada.Grid.Grid_Configure
           (Paned, "-column 0 -row 3 -sticky nswe -columnspan 2");
      end if;
      return TCL_OK;
   end Set_Toolbars_On_Top_Command;

   -- ****o* PCommands/PCommands.Set_Delete_Files_Command
   -- FUNCTION
   -- Set if files should be deleted of moved to Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDeleteFiles
   -- SOURCE
   function Set_Delete_Files_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Delete_Files_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ButtonMenu: constant Tk_Menu := Get_Widget(".deletemenu", Interp);
   begin
      if Tcl_GetVar
          (Interp,
           ".preferencesframe.canvas.notebook.preferences.deleting.deletefiles") =
        "0" then
         Settings.Delete_Files := False;
         Entry_Configure
           (ButtonMenu, "0",
            "-label {" & Mc(Interp, "{Move selected to Trash}") & "}");
      else
         Settings.Delete_Files := True;
         Entry_Configure
           (ButtonMenu, "0",
            "-label {" & Mc(Interp, "{Delete selected}") & "}");
      end if;
      return TCL_OK;
   end Set_Delete_Files_Command;

   -- ****o* PCommands/PCommands.Set_Clear_Trash_Command
   -- FUNCTION
   -- Set if Trash should be cleared on the program exit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetClearTrash
   -- SOURCE
   function Set_Clear_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Clear_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      Settings.Clear_Trash_On_Exit :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.deleting.cleartrash") =
           "0"
         then False
         else True);
      return TCL_OK;
   end Set_Clear_Trash_Command;

   -- ****o* PCommands/PCommands.Set_Overwrite_Command
   -- FUNCTION
   -- Set if the program should overwrite files or create new with new
   -- names
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetOverwrite
   -- SOURCE
   function Set_Overwrite_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Overwrite_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      Settings.Overwrite_On_Exist :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.preferences.copying.overwrite") =
           "0"
         then False
         else True);
      return TCL_OK;
   end Set_Overwrite_Command;

   -- ****o* PCommands/PCommands.Show_Preferences_Command
   -- FUNCTION
   -- Go to the selected bookmarked directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowPreferences
   -- SOURCE
   function Show_Preferences_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Preferences_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Frame: Ttk_Frame := Get_Widget(".mainframe", Interp);
      CurrentDir: constant String := Ada.Directories.Current_Directory;
      Row: Positive := 1;
      Label: Ttk_Label;
      ModulesFrame: constant Ttk_Frame :=
        Get_Widget(".preferencesframe.canvas.notebook.modules", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(ModulesFrame & ".closebutton", Interp);
      Rows: Natural;
      Tokens: Slice_Set;
      Item: Ttk_Frame;
      procedure LoadModulesInfo(Path: String) is
         Directory: Dir_Type;
         FileName: String(1 .. 1_024);
         Last: Natural range 0 .. FileName'Last;
         ConfigName: GNAT.OS_Lib.String_Access;
         ConfigFile: File_Type;
         Line, CheckButtonName: Unbounded_String;
         CheckButton: Ttk_CheckButton;
         Button: Ttk_Button;
      begin
         Open(Directory, Path);
         Read_Modules_Directory_Loop :
         loop
            Read(Directory, FileName, Last);
            exit Read_Modules_Directory_Loop when Last = 0;
            if FileName(1 .. Last) in "." | ".." then
               goto End_Of_Read_Loop;
            end if;
            if not Is_Directory(Path & "/" & FileName(1 .. Last)) then
               goto End_Of_Read_Loop;
            end if;
            ConfigName :=
              Locate_Regular_File
                ("module.cfg", Path & "/" & FileName(1 .. Last));
            if ConfigName = null or
              Locate_Regular_File
                  ("module.tcl", Path & "/" & FileName(1 .. Last)) =
                null then
               goto End_Of_Read_Loop;
            end if;
            CheckButtonName :=
              To_Unbounded_String
                (ModulesFrame & ".enabled" & Trim(Positive'Image(Row), Left));
            CheckButton :=
              Create
                (To_String(CheckButtonName),
                 "-command {ToggleModule {" & Path & "/" &
                 FileName(1 .. Last) & "}}");
            Add
              (CheckButton,
               Mc(Get_Context, "{Enable or disable the selected module}"));
            if Enabled_Modules.Contains
                (To_Unbounded_String(Path & "/" & FileName(1 .. Last))) then
               Tcl_SetVar(Interp, To_String(CheckButtonName), "1");
            else
               Tcl_SetVar(Interp, To_String(CheckButtonName), "0");
            end if;
            Tcl.Tk.Ada.Grid.Grid(CheckButton, "-row" & Positive'Image(Row));
            Open(ConfigFile, In_File, ConfigName.all);
            Read_Config_File_Loop :
            while not End_Of_File(ConfigFile) loop
               Line := Get_Line(ConfigFile);
               if Length(Line) > 5 and then Index(Line, "Name=") = 1 then
                  Label :=
                    Create
                      (ModulesFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {" & Slice(Line, 6, Length(Line)) &
                       "} -wraplength 200");
                  Tcl.Tk.Ada.Grid.Grid
                    (Label, "-column 1 -row" & Positive'Image(Row));
               elsif Length(Line) > 8 and then Index(Line, "Version=") = 1 then
                  Label :=
                    Create
                      (ModulesFrame & ".version" &
                       Trim(Positive'Image(Row), Left),
                       "-text {" & Slice(Line, 9, Length(Line)) &
                       "} -wraplength 100");
                  Tcl.Tk.Ada.Grid.Grid
                    (Label, "-column 2 -row" & Positive'Image(Row));
               elsif Length(Line) > 12
                 and then Index(Line, "Description=") = 1 then
                  Label :=
                    Create
                      (ModulesFrame & ".description" &
                       Trim(Positive'Image(Row), Left),
                       "-text {" & Slice(Line, 13, Length(Line)) &
                       "} -wraplength 400");
                  Tcl.Tk.Ada.Grid.Grid
                    (Label, "-column 3 -row" & Positive'Image(Row));
               end if;
            end loop Read_Config_File_Loop;
            Close(ConfigFile);
            Button :=
              Create
                (ModulesFrame & ".show" & Trim(Positive'Image(Row), Left),
                 "-image document-openicon -style Toolbutton -command {ShowModule {" &
                 Path & "/" & FileName(1 .. Last) & "}}");
            Add
              (Button,
               Mc(Get_Context, "{Show directory of the selected module}"));
            Tcl.Tk.Ada.Grid.Grid
              (Button, "-column 4 -row" & Positive'Image(Row));
            Row := Row + 1;
            <<End_Of_Read_Loop>>
         end loop Read_Modules_Directory_Loop;
         Close(Directory);
      exception
         when Directory_Error =>
            null;
      end LoadModulesInfo;
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      Frame.Name := New_String(".preferencesframe");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-sticky nwes");
      Unbind_From_Main_Window(Interp, "<Escape>");
      Bind_To_Main_Window(Interp, "<Escape>", "{ClosePreferences}");
      -- Remove the old list of the program modules
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(ModulesFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Remove_Old_Modules_Info_Loop :
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (ModulesFrame, "-row" & Positive'Image(I)),
            " ");
         Remove_Widgets_Loop :
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J), Interp);
            if Widget_Image(Item) /= ModulesFrame & ".closebutton" then
               Destroy(Item);
            end if;
         end loop Remove_Widgets_Loop;
      end loop Remove_Old_Modules_Info_Loop;
      -- Load the list of the program modules
      Set_Directory(Containing_Directory(Command_Name));
      LoadModulesInfo("../share/hunter/modules");
      LoadModulesInfo(Value("HOME") & "/.local/share/hunter/modules");
      Tcl.Tk.Ada.Grid.Grid_Configure
        (CloseButton, "-row" & Positive'Image(Row));
      Set_Directory(CurrentDir);
      return TCL_OK;
   end Show_Preferences_Command;

   -- ****o* PCommands/PCommands.Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog window
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseDialog dialogpath
   -- Dialogpath is the pathname of the dialog to close
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Dialog_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Dialog: Tk_Toplevel := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
   begin
      Destroy(Dialog);
      if Winfo_Get(MainWindow, "exists") = "1"
        and then Status(MainWindow) = "1" then
         Forget(MainWindow);
      end if;
      return TCL_OK;
   end Close_Dialog_Command;

   -- ****o* PCommands/PCommands.Close_Preferences_Command
   -- FUNCTION
   -- Set the program's settings, restart autorefresh view and close the
   -- preferences dialog
   -- names
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ClosePreferences
   -- SOURCE
   function Close_Preferences_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Preferences_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Frame: Ttk_Frame := Get_Widget(".preferencesframe", Interp);
      Scale: Ttk_Scale :=
        Get_Widget
          (Frame & ".canvas.notebook.preferences.directory.intervalscale",
           Interp);
   begin
      Bind_To_Main_Window(Interp, "<Escape>", "{HideWidget}");
      Settings.Auto_Refresh_Interval := Natural(Float'Value(Get(Scale)));
      Scale.Name :=
        New_String
          (".preferencesframe.canvas.notebook.preferences.interface.messagesscale");
      Settings.Auto_Close_Messages_Time := Natural(Float'Value(Get(Scale)));
      Start_Timer;
      Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      Frame.Name := New_String(".mainframe");
      Tcl.Tk.Ada.Grid.Grid(Frame);
      Execute_Modules
        (Interp, ON_ENTER, "{" & To_String(Common.Current_Directory) & "}");
      return TCL_OK;
   end Close_Preferences_Command;

   -- ****o* PCommands/PCommands.Set_UI_Theme_Command
   -- FUNCTION
   -- Set the UI theme
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUITheme
   -- SOURCE
   function Set_UI_Theme_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_UI_Theme_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget
          (".preferencesframe.canvas.notebook.preferences.interface.colorframe.uitheme",
           Interp);
   begin
      Settings.Ui_Theme := To_Unbounded_String(Get(ComboBox));
      Show_Message
        (Mc(Interp, "{To use the new UI theme, please restart the program.}"),
         "message");
      return Close_Preferences_Command(ClientData, Interp, Argc, Argv);
   end Set_UI_Theme_Command;

   -- ****o* PCommands/PCommands.Set_Toolbars_Size_Command
   -- FUNCTION
   -- Set the toolbars icons size
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetToolbarsSize
   -- SOURCE
   function Set_Toolbars_Size_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Toolbars_Size_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget
          (".preferencesframe.canvas.notebook.preferences.interface.toolbarframe.toolbarsize",
           Interp);
      ToolbarSize: Natural;
      ImageSize: Positive;
      ImagesNames: constant array(1 .. 21) of Unbounded_String :=
        (To_Unbounded_String("quiticon"), To_Unbounded_String("bookmarksicon"),
         To_Unbounded_String("edit-findicon"),
         To_Unbounded_String("edit-select-allicon"),
         To_Unbounded_String("document-newicon"),
         To_Unbounded_String("document-save-asicon"),
         To_Unbounded_String("edit-copyicon"),
         To_Unbounded_String("edit-cuticon"),
         To_Unbounded_String("edit-deleteicon"),
         To_Unbounded_String("dialog-cancelicon"),
         To_Unbounded_String("document-reverticon"),
         To_Unbounded_String("configureicon"),
         To_Unbounded_String("help-abouticon"),
         To_Unbounded_String("media-playback-starticon"),
         To_Unbounded_String("document-openicon"),
         To_Unbounded_String("system-runicon"),
         To_Unbounded_String("document-previewicon"),
         To_Unbounded_String("document-propertiesicon"),
         To_Unbounded_String("list-addicon"),
         To_Unbounded_String("list-removeicon"), To_Unbounded_String("ok"));
      Image: Ttk_Frame;
      CurrentDir: constant String := Ada.Directories.Current_Directory;
   begin
      Set_Directory(Containing_Directory(Command_Name));
      ToolbarSize := Natural'Value(Current(ComboBox));
      ImageSize :=
        (case ToolbarSize is when 0 => 16, when 1 => 24, when 2 => 32,
           when 3 => 64, when others => 64);
      Image.Interp := Interp;
      Set_Toolbars_Icons_Loop :
      for ImageName of ImagesNames loop
         Image.Name := New_String(To_String(ImageName));
         Widgets.configure
           (Image,
            "-format {svg -scaletoheight" & Positive'Image(ImageSize) & "}");
      end loop Set_Toolbars_Icons_Loop;
      Set_Directory(CurrentDir);
      Settings.Toolbars_Size := ImageSize;
      return TCL_OK;
   end Set_Toolbars_Size_Command;

   -- ****o* PCommands/PCommands.Restore_Defaults_Command
   -- FUNCTION
   -- Restore default the program setttings
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RestoreDefaults
   -- SOURCE
   function Restore_Defaults_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Defaults_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
   begin
      Settings := Default_Settings;
      Show_Message
        (Mc
           (Interp,
            "{To bring back all default Hunter settings, please restart the program.}"),
         "message");
      return Close_Preferences_Command(ClientData, Interp, Argc, Argv);
   end Restore_Defaults_Command;

   -- ****o* PCommands/PCommands.Start_Changing_Shortcut_Command
   -- FUNCTION
   -- Start changing the selected keyboard shortcut
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StartChangingShortcut shortcutindex
   -- Shortcutindex is the index of the keyboard shortcut which will be
   -- changed
   -- SOURCE
   function Start_Changing_Shortcut_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Start_Changing_Shortcut_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Label: constant Ttk_Label :=
        Get_Widget
          (".preferencesframe.canvas.notebook.shortcuts.labelshortcut" &
           CArgv.Arg(Argv, 1),
           Interp);
   begin
      Widgets.configure
        (Label,
         "-text {" &
         Mc(Interp, "{Select a new shortcut or press Escape to cancel}") &
         "}");
      Unbind_From_Main_Window(Interp, "<Escape>");
      Bind_To_Main_Window
        (Interp, "<KeyRelease>",
         "{ChangeShortcut " & CArgv.Arg(Argv, 1) & " %K}");
      Tcl_SetVar(Interp, "specialkey", "{}");
      return TCL_OK;
   end Start_Changing_Shortcut_Command;

   -- ****o* PCommands/PCommands.Change_Shortcut_Command
   -- FUNCTION
   -- Change the selected keyboard shortcut
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ChangeShortcut shortcutindex
   -- Shortcutindex is the index of the keyboard shortcut which will be
   -- changed
   -- SOURCE
   function Change_Shortcut_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Change_Shortcut_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Label: constant Ttk_Label :=
        Get_Widget
          (".preferencesframe.canvas.notebook.shortcuts.labelshortcut" &
           CArgv.Arg(Argv, 1),
           Interp);
      Key: constant String := CArgv.Arg(Argv, 2);
      Shortcut: constant String :=
        (if Tcl_GetVar(Interp, "specialkey") = "{}" then Key
         else Tcl_GetVar(Interp, "specialkey") & "-" & Key);
      Index: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Script: Unbounded_String;
   begin
      if Key = "Escape" then
         Widgets.configure
           (Label, "-text {" & To_String(Accelerators(Index)) & "}");
         Unbind_From_Main_Window(Interp, "<KeyRelease>");
         Bind_To_Main_Window(Interp, "<Escape>", "{ClosePreferences}");
         return TCL_OK;
      end if;
      if Key in "Control_L" | "Control_R" | "Alt_L" | "Alt_R" | "Shift_L" |
            "Shift_R" then
         Tcl_SetVar(Interp, "specialkey", Key(Key'First .. Key'Length - 2));
         Widgets.configure(Label, "-text ""$specialkey-""");
         return TCL_OK;
      end if;
      Find_Existing_Accelerator_Loop :
      for Accelerator of Accelerators loop
         if To_Unbounded_String(Shortcut) = Accelerator then
            return TCL_OK;
         end if;
      end loop Find_Existing_Accelerator_Loop;
      Script :=
        To_Unbounded_String
          (Bind_To_Main_Window
             (Interp, "<" & To_String(Accelerators(Index)) & ">"));
      Unbind_From_Main_Window
        (Interp, "<" & To_String(Accelerators(Index)) & ">");
      Bind_To_Main_Window
        (Interp, "<" & Shortcut & ">", "{" & To_String(Script) & "}");
      Accelerators(Index) := To_Unbounded_String(Shortcut);
      Unbind_From_Main_Window(Interp, "<KeyRelease>");
      Bind_To_Main_Window(Interp, "<Escape>", "{ClosePreferences}");
      Widgets.configure(Label, "-text {" & Shortcut & "}");
      return TCL_OK;
   end Change_Shortcut_Command;

   -- ****o* PCommands/PCommands.Restore_Default_Shortcuts_Command
   -- FUNCTION
   -- Restore default keyboard shortcuts
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RestoreDefaultShortcuts
   -- SOURCE
   function Restore_Default_Shortcuts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Default_Shortcuts_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      OldAccelerators: constant Accelerators_Array := Accelerators;
      Script: Unbounded_String;
      Label: Ttk_Label;
   begin
      Label.Interp := Interp;
      Accelerators := Default_Accelerators;
      Restore_Default_Shortcuts_Loop :
      for I in OldAccelerators'Range loop
         Script :=
           To_Unbounded_String
             (Bind_To_Main_Window
                (Interp, "<" & To_String(OldAccelerators(I)) & ">"));
         Unbind_From_Main_Window
           (Interp, "<" & To_String(OldAccelerators(I)) & ">");
         Bind_To_Main_Window
           (Interp, "<" & To_String(Accelerators(I)) & ">",
            "{" & To_String(Script) & "}");
         Label.Name :=
           New_String
             (".preferencesframe.canvas.notebook.shortcuts.labelshortcut" &
              Trim(Positive'Image(I), Left));
         Widgets.configure
           (Label, "-text {" & To_String(Accelerators(I)) & "}");
      end loop Restore_Default_Shortcuts_Loop;
      return TCL_OK;
   end Restore_Default_Shortcuts_Command;

   -- ****o* PCommands/PCommands.Add_Command_Command
   -- FUNCTION
   -- Add user defined command
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Add_Command
   -- SOURCE
   function Add_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Tentry: Ttk_Entry :=
        Get_Widget
          (".preferencesframe.canvas.notebook.actions.addframe.title", Interp);
      MenuEntry, Command: Unbounded_String;
      NeedOutput: Boolean;
   begin
      MenuEntry := To_Unbounded_String(Get(Tentry));
      if MenuEntry = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Tentry.Name :=
        New_String
          (".preferencesframe.canvas.notebook.actions.addframe.command");
      Command := To_Unbounded_String(Get(Tentry));
      if Command = Null_Unbounded_String then
         return TCL_OK;
      end if;
      NeedOutput :=
        (if
           Tcl_GetVar
             (Interp,
              ".preferencesframe.canvas.notebook.actions.addframe.output") =
           "1"
         then True
         else False);
      if User_Commands_List.Contains(To_String(MenuEntry)) then
         User_Commands_List(To_String(MenuEntry)) := (NeedOutput, Command);
      else
         User_Commands_List.Include(To_String(MenuEntry), (NeedOutput, Command));
      end if;
      Clear_Add_Command;
      UpdateUserCommandsList;
      Set_User_Commands_Menu;
      return TCL_OK;
   end Add_Command_Command;

   -- ****o* PCommands/PCommands.Edit_Command_Command
   -- FUNCTION
   -- Edit the selected user defined command
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- EditCommand menuentry
   -- Menuentry is the menu label of the command which will be edited
   -- SOURCE
   function Edit_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Edit_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Tentry: Ttk_Entry :=
        Get_Widget
          (".preferencesframe.canvas.notebook.actions.addframe.title", Interp);
      MenuEntry: constant String := CArgv.Arg(Argv, 1);
      Button: constant Ttk_Button :=
        Get_Widget
          (".preferencesframe.canvas.notebook.actions.addframe.add", Interp);
   begin
      Delete(Tentry, "0", "end");
      Insert(Tentry, "end", MenuEntry);
      Tentry.Name :=
        New_String
          (".preferencesframe.canvas.notebook.actions.addframe.command");
      Delete(Tentry, "0", "end");
      Insert(Tentry, "end", To_String(User_Commands_List(MenuEntry).Command));
      if User_Commands_List(MenuEntry).Need_Output then
         Tcl_SetVar
           (Interp,
            ".preferencesframe.canvas.notebook.actions.addframe.output", "1");
      else
         Tcl_SetVar
           (Interp,
            ".preferencesframe.canvas.notebook.actions.addframe.output", "0");
      end if;
      Widgets.configure
        (Button, "-text {" & Mc(Interp, "{Edit command}") & "}");
      return TCL_OK;
   end Edit_Command_Command;

   -- ****o* PCommands/PCommands.Delete_Command_Command
   -- FUNCTION
   -- Delete the selected user defined command
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- EditCommand menuentry
   -- Menuentry is the menu label of the command which will be deleted
   -- SOURCE
   function Delete_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Delete_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      User_Commands_List.Delete(CArgv.Arg(Argv, 1));
      UpdateUserCommandsList;
      Set_User_Commands_Menu;
      return TCL_OK;
   end Delete_Command_Command;

   -- ****o* PCommands/PCommands.Reset_Command_Command
   -- FUNCTION
   -- Clear the add command form
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetCommand
   -- SOURCE
   function Reset_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      Clear_Add_Command;
      return TCL_OK;
   end Reset_Command_Command;

   -- ****o* PCommands/PCommands.Show_Module_Command
   -- FUNCTION
   -- Show the directory of the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModule modulepath
   -- Modulepath is full or relative path to the selected module's directory
   -- SOURCE
   function Show_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Interfaces.C;
   begin
      if Close_Preferences_Command(ClientData, Interp, Argc, Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      Common.Current_Directory :=
        To_Unbounded_String
          (Normalize_Pathname
             (CArgv.Arg(Argv, 1), Containing_Directory(Command_Name)));
      Load_Directory(To_String(Common.Current_Directory));
      Current_Selected := Common.Current_Directory & "/" & Items_List(1).Name;
      Update_Directory_List(True);
      Update_Watch(To_String(Common.Current_Directory));
      Show_Preview;
      return TCL_OK;
   end Show_Module_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowPreferences", Show_Preferences_Command'Access);
      Add_Command("CloseDialog", Close_Dialog_Command'Access);
      Add_Command("SetLabel", Set_Label_Command'Access);
      Add_Command("SetShowHiddenFiles", Set_Show_Hidden_Files_Command'Access);
      Add_Command
        ("SetShowModificationTime", Set_Show_Modification_Time_Command'Access);
      Add_Command("SetShowPreview", Set_Show_Preview_Command'Access);
      Add_Command("SetScaleImages", Set_Scale_Images_Command'Access);
      Add_Command("SetColorText", Set_Color_Text_Command'Access);
      Add_Command("SetColorTheme", Set_Color_Theme_Command'Access);
      Add_Command("SetMonospaceFont", Set_Monospace_Font_Command'Access);
      Add_Command("SetStayInOld", Set_Stay_In_Old_Command'Access);
      Add_Command
        ("SetShowFinishedInfo", Set_Show_Finished_Info_Command'Access);
      Add_Command("SetToolbarsOnTop", Set_Toolbars_On_Top_Command'Access);
      Add_Command("SetDeleteFiles", Set_Delete_Files_Command'Access);
      Add_Command("SetClearTrash", Set_Clear_Trash_Command'Access);
      Add_Command("SetOverwrite", Set_Overwrite_Command'Access);
      Add_Command("ClosePreferences", Close_Preferences_Command'Access);
      Add_Command("SetUITheme", Set_UI_Theme_Command'Access);
      Add_Command("SetToolbarsSize", Set_Toolbars_Size_Command'Access);
      Add_Command("RestoreDefaults", Restore_Defaults_Command'Access);
      Add_Command
        ("StartChangingShortcut", Start_Changing_Shortcut_Command'Access);
      Add_Command("ChangeShortcut", Change_Shortcut_Command'Access);
      Add_Command
        ("RestoreDefaultShortcuts", Restore_Default_Shortcuts_Command'Access);
      Add_Command("AddCommand", Add_Command_Command'Access);
      Add_Command("EditCommand", Edit_Command_Command'Access);
      Add_Command("DeleteCommand", Delete_Command_Command'Access);
      Add_Command("ResetCommand", Reset_Command_Command'Access);
      Add_Command("ShowModule", Show_Module_Command'Access);
   end AddCommands;

end Preferences.Commands;
