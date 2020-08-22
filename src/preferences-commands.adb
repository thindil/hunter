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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
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
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with MainWindow; use MainWindow;
with Messages; use Messages;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Toolbars; use Toolbars;
with Utils; use Utils;

package body Preferences.Commands is

   -- ****if* PCommands/Set_Label_Command
   -- FUNCTION
   -- Update text of the selected label
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Label_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Label_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Label: Ttk_Label;
   begin
      Label.Interp := Interp;
      Label.Name :=
        New_String(".preferencesframe." & CArgv.Arg(Argv, 1) & "label");
      if CArgv.Arg(Argv, 1) = "directory.interval" then
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {" & Mc(Interp, "{Auto refresh every }") &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "updateinterval")))) &
            Mc(Interp, "{ seconds}") & "}");
      else
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {" & Mc(Interp, "{Hide messages after }") &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "messagesinterval")))) &
            Mc(Interp, "{ seconds}") & "}");
      end if;
      return TCL_OK;
   end Set_Label_Command;

   -- ****if* PCommands/Set_Show_Hidden_Files_Command
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
   -- SOURCE
   function Set_Show_Hidden_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Hidden_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.directory.showhidden") =
        "0" then
         Settings.ShowHidden := False;
      else
         Settings.ShowHidden := True;
      end if;
      UpdateDirectoryList(True);
      return TCL_OK;
   end Set_Show_Hidden_Files_Command;

   -- ****if* PCommands/Set_Show_Modification_Time_Command
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
   -- SOURCE
   function Set_Show_Modification_Time_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Modification_Time_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DirectoryTree: Ttk_Tree_View;
   begin
      DirectoryTree.Interp := Interp;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      if Tcl_GetVar
          (Interp, ".preferencesframe.directory.showmodificationtime") =
        "0" then
         Settings.ShowLastModified := False;
         Tcl.Tk.Ada.Widgets.configure
           (DirectoryTree, "-displaycolumns [list name size]");
      else
         Settings.ShowLastModified := True;
         Tcl.Tk.Ada.Widgets.configure(DirectoryTree, "-displaycolumns #all");
      end if;
      return TCL_OK;
   end Set_Show_Modification_Time_Command;

   -- ****if* PCommands/Set_Show_Preview_Command
   -- FUNCTION
   -- Update show preview setting and show or hide it
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Show_Preview_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Preview_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      PreviewFrame: Ttk_Frame;
      Paned: Ttk_PanedWindow;
   begin
      PreviewFrame.Interp := Interp;
      PreviewFrame.Name := New_String(".mainframe.paned.previewframe");
      Paned.Interp := Interp;
      Paned.Name := New_String(".mainframe.paned");
      if Tcl_GetVar(Interp, ".preferencesframe.preview.showpreview") = "0" then
         Settings.ShowPreview := False;
         Forget(Paned, PreviewFrame);
         return TCL_OK;
      end if;
      Settings.ShowPreview := True;
      Add(Paned, PreviewFrame, "-weight 20");
      return Show_Selected_Command(ClientData, Interp, Argc, Argv);
   end Set_Show_Preview_Command;

   -- ****if* PCommands/Set_Scale_Images_Command
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
   -- SOURCE
   function Set_Scale_Images_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Scale_Images_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MimeType: constant String := GetMimeType(To_String(CurrentSelected));
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.preview.scaleimages") = "0" then
         Settings.ScaleImages := False;
      else
         Settings.ScaleImages := True;
      end if;
      if MimeType(1 .. 5) = "image" then
         ShowPreview;
      end if;
      return TCL_OK;
   end Set_Scale_Images_Command;

   -- ****if* PCommands/Set_Color_Text_Command
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
   -- SOURCE
   function Set_Color_Text_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Color_Text_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
      MimeType: constant String := GetMimeType(To_String(CurrentSelected));
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name :=
        New_String(".preferencesframe.preview.colorframe.highlighttheme");
      if Tcl_GetVar(Interp, ".preferencesframe.preview.syntaxhighlightning") =
        "0" then
         Settings.ColorText := False;
         State(ComboBox, "disabled");
      else
         Settings.ColorText := True;
         State(ComboBox, "!disabled");
      end if;
      if MimeType(1 .. 4) = "text" then
         ShowPreview;
      end if;
      return TCL_OK;
   end Set_Color_Text_Command;

   -- ****if* PCommands/Set_Color_Theme_Command
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
   -- SOURCE
   function Set_Color_Theme_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Color_Theme_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
      MimeType: constant String := GetMimeType(To_String(CurrentSelected));
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name :=
        New_String(".preferencesframe.preview.colorframe.highlighttheme");
      Settings.ColorTheme := To_Unbounded_String(Get(ComboBox));
      if MimeType(1 .. 4) = "text" then
         ShowPreview;
      end if;
      return TCL_OK;
   end Set_Color_Theme_Command;

   -- ****if* PCommands/Set_Monospace_Font_Command
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
   -- SOURCE
   function Set_Monospace_Font_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Monospace_Font_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      PreviewText: Ttk_Frame;
   begin
      PreviewText.Interp := Interp;
      PreviewText.Name :=
        New_String(".mainframe.paned.previewframe.previewtext");
      if Tcl_GetVar(Interp, ".preferencesframe.preview.monospacefont") =
        "0" then
         Settings.MonospaceFont := False;
         Widgets.configure(PreviewText, "-font TkDefaultFont");
      else
         Settings.MonospaceFont := True;
         Widgets.configure(PreviewText, "-font TkFixedFont");
      end if;
      return TCL_OK;
   end Set_Monospace_Font_Command;

   -- ****if* PCommands/Set_Stay_In_Old_Command
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
   -- SOURCE
   function Set_Stay_In_Old_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Stay_In_Old_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.interface.stayinold") = "0" then
         Settings.StayInOld := False;
      else
         Settings.StayInOld := True;
      end if;
      return TCL_OK;
   end Set_Stay_In_Old_Command;

   -- ****if* PCommands/Set_Show_Finished_Info_Command
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
   -- SOURCE
   function Set_Show_Finished_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Show_Finished_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.interface.showfinished") =
        "0" then
         Settings.ShowFinishedInfo := False;
      else
         Settings.ShowFinishedInfo := True;
      end if;
      return TCL_OK;
   end Set_Show_Finished_Info_Command;

   -- ****if* PCommands/Set_Toolbars_On_Top_Command
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
   -- SOURCE
   function Set_Toolbars_On_Top_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Toolbars_On_Top_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MainFrame, Paned: Ttk_Frame;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".mainframe.paned");
      if Tcl_GetVar(Interp, ".preferencesframe.interface.toolbarsontop") =
        "0" then
         Settings.ToolbarsOnTop := False;
      else
         Settings.ToolbarsOnTop := True;
      end if;
      SetToolbars;
      MainFrame.Interp := Interp;
      MainFrame.Name := New_String(".mainframe");
      if not Settings.ToolbarsOnTop then
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

   -- ****if* PCommands/Set_Delete_Files_Command
   -- FUNCTION
   -- Set if files should be deleted of moved to Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Delete_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Delete_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ButtonMenu: Tk_Menu;
   begin
      ButtonMenu.Interp := Interp;
      ButtonMenu.Name := New_String(".deletemenu");
      if Tcl_GetVar(Interp, ".preferencesframe.deleting.deletefiles") =
        "0" then
         Settings.DeleteFiles := False;
         Entry_Configure
           (ButtonMenu, "0",
            "-label {" & Mc(Interp, "{Move selected to Trash}") & "}");
      else
         Settings.DeleteFiles := True;
         Entry_Configure
           (ButtonMenu, "0",
            "-label {" & Mc(Interp, "{Delete selected}") & "}");
      end if;
      return TCL_OK;
   end Set_Delete_Files_Command;

   -- ****if* PCommands/Set_Clear_Trash_Command
   -- FUNCTION
   -- Set if Trash should be cleared on the program exit
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.deleting.cleartrash") = "0" then
         Settings.ClearTrashOnExit := False;
      else
         Settings.ClearTrashOnExit := True;
      end if;
      return TCL_OK;
   end Set_Clear_Trash_Command;

   -- ****if* PCommands/Set_Overwrite_Command
   -- FUNCTION
   -- Set if the program should overwrite files or create new with new
   -- names
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Overwrite_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Overwrite_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl_GetVar(Interp, ".preferencesframe.copying.overwrite") = "0" then
         Settings.OverwriteOnExist := False;
      else
         Settings.OverwriteOnExist := True;
      end if;
      return TCL_OK;
   end Set_Overwrite_Command;

   -- ****if* PCommands/Show_Preferences_Command
   -- FUNCTION
   -- Go to the selected bookmarked directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Frame: Ttk_Frame;
   begin
      Frame.Interp := Interp;
      Frame.Name := New_String(".mainframe");
      Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      Frame.Name := New_String(".preferencesframe");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-sticky nwes");
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

   -- ****if* PCommands/Close_Preferences_Command
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
   -- SOURCE
   function Close_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Scale: Ttk_Scale;
      Frame: Ttk_Frame;
   begin
      Scale.Interp := Interp;
      Scale.Name := New_String(".preferencesframe.directory.intervalscale");
      Settings.AutoRefreshInterval := Natural(Float'Value(Get(Scale)));
      Scale.Name := New_String(".preferencesframe.interface.messagesscale");
      Settings.AutoCloseMessagesTime := Natural(Float'Value(Get(Scale)));
      StartTimer;
      Frame.Interp := Interp;
      Frame.Name := New_String(".preferencesframe");
      Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      Frame.Name := New_String(".mainframe");
      Tcl.Tk.Ada.Grid.Grid(Frame);
      return TCL_OK;
   end Close_Preferences_Command;

   -- ****if* PCommands/Set_UI_Theme_Command
   -- FUNCTION
   -- Set the UI theme
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_UI_Theme_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_UI_Theme_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ComboBox: Ttk_ComboBox;
   begin
      ComboBox.Interp := Interp;
      ComboBox.Name :=
        New_String(".preferencesframe.interface.colorframe.uitheme");
      Settings.UITheme := To_Unbounded_String(Get(ComboBox));
      ShowMessage
        (Mc(Interp, "{To use the new UI theme, please restart the program.}"),
         "message");
      return Close_Preferences_Command(ClientData, Interp, Argc, Argv);
   end Set_UI_Theme_Command;

   -- ****if* PCommands/Set_Toolbars_Size_Command
   -- FUNCTION
   -- Set the toolbars icons size
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Toolbars_Size_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Toolbars_Size_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ComboBox: Ttk_ComboBox;
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
      CurrentDir: constant String := Current_Directory;
   begin
      Set_Directory(Containing_Directory(Command_Name));
      ComboBox.Interp := Interp;
      ComboBox.Name :=
        New_String(".preferencesframe.interface.toolbarframe.toolbarsize");
      ToolbarSize := Natural'Value(Current(ComboBox));
      case ToolbarSize is
         when 0 =>
            ImageSize := 16;
         when 1 =>
            ImageSize := 24;
         when 2 =>
            ImageSize := 32;
         when 3 =>
            ImageSize := 64;
         when others =>
            null;
      end case;
      Image.Interp := Interp;
      for ImageName of ImagesNames loop
         Image.Name := New_String(To_String(ImageName));
         Widgets.configure
           (Image,
            "-format {svg -scaletoheight" & Positive'Image(ImageSize) & "}");
      end loop;
      Set_Directory(CurrentDir);
      Settings.ToolbarsSize := ImageSize;
      return TCL_OK;
   end Set_Toolbars_Size_Command;

   -- ****if* PCommands/Restore_Defaults_Command
   -- FUNCTION
   -- Restore default the program setttings
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Restore_Defaults_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Defaults_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
   begin
      SetDefaultSettings;
      ShowMessage
        (Mc
           (Interp,
            "{To bring back all default Hunter settings, please restart the program.}"),
         "message");
      return Close_Preferences_Command(ClientData, Interp, Argc, Argv);
   end Restore_Defaults_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowPreferences", Show_Preferences_Command'Access);
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("SetLabel", Set_Label_Command'Access);
      AddCommand("SetShowHiddenFiles", Set_Show_Hidden_Files_Command'Access);
      AddCommand
        ("SetShowModificationTime", Set_Show_Modification_Time_Command'Access);
      AddCommand("SetShowPreview", Set_Show_Preview_Command'Access);
      AddCommand("SetScaleImages", Set_Scale_Images_Command'Access);
      AddCommand("SetColorText", Set_Color_Text_Command'Access);
      AddCommand("SetColorTheme", Set_Color_Theme_Command'Access);
      AddCommand("SetMonospaceFont", Set_Monospace_Font_Command'Access);
      AddCommand("SetStayInOld", Set_Stay_In_Old_Command'Access);
      AddCommand("SetShowFinishedInfo", Set_Show_Finished_Info_Command'Access);
      AddCommand("SetToolbarsOnTop", Set_Toolbars_On_Top_Command'Access);
      AddCommand("SetDeleteFiles", Set_Delete_Files_Command'Access);
      AddCommand("SetClearTrash", Set_Clear_Trash_Command'Access);
      AddCommand("SetOverwrite", Set_Overwrite_Command'Access);
      AddCommand("ClosePreferences", Close_Preferences_Command'Access);
      AddCommand("SetUITheme", Set_UI_Theme_Command'Access);
      AddCommand("SetToolbarsSize", Set_Toolbars_Size_Command'Access);
      AddCommand("RestoreDefaults", Restore_Defaults_Command'Access);
   end AddCommands;

end Preferences.Commands;
