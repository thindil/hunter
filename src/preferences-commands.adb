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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with MainWindow; use MainWindow;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body Preferences.Commands is

   -- ****it* PCommands/CreateCommands
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

      -- ****if* PCommands/Set_Label_Command
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
      else
         Tcl.Tk.Ada.Widgets.configure
           (Label,
            "-text {Hide messages after " &
            Natural'Image
              (Natural(Float'Value(Tcl_GetVar(Interp, "messagesinterval")))) &
            " seconds}");
      end if;
      return TCL_OK;
   end Set_Label_Command;

   function Set_Show_Hidden_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* PCommands/Set_Show_Hidden_Files_Command
      -- FUNCTION
      -- Update show hidden files setting and reload the current directory
      -- listing
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Show_Hidden_Files_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
   begin
      if Tcl_GetVar(Interp, ".preferencesdialog.directory.showhidden") =
        "0" then
         Settings.ShowHidden := False;
      else
         Settings.ShowHidden := True;
      end if;
      UpdateDirectoryList(True);
      return TCL_OK;
   end Set_Show_Hidden_Files_Command;

   function Set_Show_Modification_Time_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* PCommands/Set_Show_Modification_Time_Command
      -- FUNCTION
      -- Update show modification time column and reload the current directory
      -- listing
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Show_Modification_Time_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
      DirectoryTree: Ttk_Tree_View;
   begin
      DirectoryTree.Interp := Interp;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      if Tcl_GetVar
          (Interp, ".preferencesdialog.directory.showmodificationtime") =
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

   function Set_Show_Preview_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* PCommands/Set_Show_Preview_Command
      -- FUNCTION
      -- Update show preview setting and show or hide it
      -- PARAMETERS
      -- ClientData - Custom data send to the command.
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command.
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Show_Preview_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      -- ****
      PreviewFrame: Ttk_Frame;
      Paned: Ttk_PanedWindow;
   begin
      PreviewFrame.Interp := Interp;
      PreviewFrame.Name := New_String(".mainframe.paned.previewframe");
      Paned.Interp := Interp;
      Paned.Name := New_String(".mainframe.paned");
      if Tcl_GetVar(Interp, ".preferencesdialog.preview.showpreview") =
        "0" then
         Settings.ShowPreview := False;
         Forget(Paned, PreviewFrame);
         return TCL_OK;
      end if;
      Settings.ShowPreview := True;
      Add(Paned, PreviewFrame, "-weight 20");
      return Show_Selected_Command(ClientData, Interp, Argc, Argv);
   end Set_Show_Preview_Command;

   function Set_Scale_Images_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* PCommands/Set_Scale_Images_Command
      -- FUNCTION
      -- Update show hidden files setting and reload the current directory
      -- listing
      -- PARAMETERS
      -- ClientData - Custom data send to the command.
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command.
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Scale_Images_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
      MimeType: constant String := GetMimeType(To_String(CurrentSelected));
   begin
      if Tcl_GetVar(Interp, ".preferencesdialog.preview.scaleimages") =
        "0" then
         Settings.ScaleImages := False;
      else
         Settings.ScaleImages := True;
      end if;
      if MimeType(1 .. 5) = "image" then
         ShowPreview;
      end if;
      return TCL_OK;
   end Set_Scale_Images_Command;

   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* PCommands/Show_Preferences_Command
      -- FUNCTION
      -- Go to the selected bookmarked directory
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Show_Preferences_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
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
      ColorsEnabled: constant Boolean :=
        (if FindExecutable("highlight")'Length > 0 then True else False);
      procedure AddButton
        (Name, Text: String; Value: Boolean; TooltipText: String;
         Command: String := "") is
         CheckButton: Ttk_CheckButton;
      begin
         CheckButton :=
           (if Command /= "" then
              Create
                (Widget_Image(LabelFrame) & Name,
                 "-text {" & Text & "} -command " & Command)
            else Create
                (Widget_Image(LabelFrame) & Name, "-text {" & Text & "}"));
         if Value then
            Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
         else
            Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
         end if;
         Add(CheckButton, TooltipText);
         Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      end AddButton;
   begin
      if Tcl.Tk.Ada.Busy.Status(MainWindow) = "0" then
         Tcl.Tk.Ada.Busy.Busy(MainWindow);
      end if;
      LabelFrame :=
        Create
          (Widget_Image(PreferencesDialog) & ".directory",
           "-text {Directory Listing}");
      AddButton
        (".showhidden", "Show hidden files", Settings.ShowHidden,
         "Show hidden files and directories in directory\nlisting and in directories preview.",
         "SetShowHiddenFiles");
      AddButton
        (".showmodificationtime", "Show modification time",
         Settings.ShowLastModified,
         "Show the column with last modification\ndate for files and directories.",
         "SetShowModificationTime");
      Tcl_SetVar
        (Interp, "updateinterval",
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
      AddButton
        (".showpreview", "Show preview", Settings.ShowPreview,
         "Show second panel with preview of files and directories.\nIf you disable this option, second panel will be visible only during\ncopying and moving files or directories and during creating new link.",
         "SetShowPreview");
      AddButton
        (".scaleimages", "Scale images", Settings.ScaleImages,
         "Scale images in preview. When disabled, images shows with\nnatural size. When enabled, images are resized to the size of the\npreview window.",
         "SetScaleImages");
      CheckButton :=
        Create
          (Widget_Image(LabelFrame) & ".syntaxhighlightning",
           "-text {Syntax highlightning}");
      if Settings.ColorText then
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "1");
      else
         Tcl_SetVar(CheckButton.Interp, Widget_Image(CheckButton), "0");
      end if;
      if ColorsEnabled then
         State(CheckButton, "!disabled");
      else
         State(CheckButton, "disabled");
      end if;
      Add
        (CheckButton,
         "Color files syntax in files preview. Not all text (especially source code)\nfiles are supported. You may not be able to enable this\noption if you don't have installed the program 'highlight'.");
      Tcl.Tk.Ada.Pack.Pack(CheckButton, "-fill x");
      declare
         Search: Search_Type;
         File: Directory_Entry_Type;
         ThemesName: Unbounded_String;
         ComboBox: Ttk_ComboBox;
         ColorFrame: constant Ttk_Frame :=
           Create(Widget_Image(LabelFrame) & ".colorframe");
      begin
         if not Ada.Environment_Variables.Exists("HIGHLIGHT_DATADIR") then
            Ada.Environment_Variables.Set
              ("HIGHLIGHT_DATADIR",
               Ada.Environment_Variables.Value("APPDIR", "") &
               "/usr/share/highlight");
         end if;
         if Exists
             (Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
              "/themes/base16") then
            Start_Search
              (Search,
               Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
               "/themes/base16",
               "*.theme");
            while More_Entries(Search) loop
               Get_Next_Entry(Search, File);
               Append(ThemesName, " " & Base_Name(Simple_Name(File)));
            end loop;
            End_Search(Search);
         end if;
         ComboBox :=
           Create
             (Widget_Image(ColorFrame) & ".highlighttheme",
              "-state readonly -values [list" & To_String(ThemesName) & "]");
         if ColorsEnabled then
            State(ComboBox, "!disabled");
         else
            State(ComboBox, "disabled");
         end if;
         Set(ComboBox, "{" & To_String(Settings.ColorTheme) & "}");
         Add
           (ComboBox,
            "Select color theme for coloring syntax in text files in preview. You may\nnot be able to enable this option if you don't have installed\nthe program 'highlight'.");
         Label :=
           Create
             (Widget_Image(ColorFrame) & ".themelabel",
              "-text {Color theme:}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(ComboBox, "-column 1 -row 0");
         Tcl.Tk.Ada.Pack.Pack(ColorFrame, "-fill x");
      end;
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesDialog) & ".interface",
           "-text {Interface}");
      Tcl_SetVar
        (CheckButton.Interp, "messagesinterval",
         Natural'Image(Settings.AutoCloseMessagesTime));
      Label :=
        Create
          (Widget_Image(LabelFrame) & ".messageslabel",
           "-text ""Hide messages after $messagesinterval seconds""");
      Tcl.Tk.Ada.Pack.Pack(Label, "-fill x");
      Scale :=
        Create
          (Widget_Image(LabelFrame) & ".messagesscale",
           "-from 0 -to 60 -variable messagesinterval -orient horizontal -command {SetLabel interface.messages}");
      Add
        (Scale,
         "After that amount of seconds, all messages will be automatically closed by the\nprogram. If you set it to 0, this feature will be\ndisabled.");
      Tcl.Tk.Ada.Pack.Pack(Scale, "-fill x");
      AddButton
        (".stayinold", "Stay in source directory", Settings.StayInOld,
         "After copying, moving files and directories or creating new link, stay in old\ndirectory, don't automatically go to destination directory.");
      AddButton
        (".showfinished", "Show info about finished action",
         Settings.ShowFinishedInfo,
         "Show information about finished copying, moving and\ndeleting files or directories.");
      AddButton
        (".toolbarsontop", "Toolbars on top", Settings.ToolbarsOnTop,
         "If enabled, show toolbars for actions and information on top of the window.\nOtherwise, they will be at left side of the window.");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesDialog) & ".deleting", "-text {Deleting}");
      AddButton
        (".deletefiles", "Delete files", Settings.DeleteFiles,
         "Delete selected files and directories instead of moving them to Trash.");
      AddButton
        (".cleartrash", "Clear Trash on exit", Settings.ClearTrashOnExit,
         "Automatically clear Trash on exit from the program.");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      LabelFrame :=
        Create
          (Widget_Image(PreferencesDialog) & ".copying",
           "-text {Copying or moving}");
      AddButton
        (".overwrite", "Overwrite existing", Settings.OverwriteOnExist,
         "If enabled, during copying or moving files and directories,\nif in destination directory exists file or directory with that\nsame name, the program will ask if overwrite it. If disabled, the\nprogram will quietly give add underscore to the name of moved or\ncopied file or directory.");
      Tcl.Tk.Ada.Pack.Pack(LabelFrame, "-fill x");
      Tcl.Tk.Ada.Pack.Pack(CloseButton);
      Bind
        (PreferencesDialog, "<Alt-c>",
         "{CloseDialog " & Widget_Image(PreferencesDialog) & "}");
      SetDialog(PreferencesDialog, "Hunter - Preferences", 350, 600);
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
      AddCommand("SetShowHiddenFiles", Set_Show_Hidden_Files_Command'Access);
      AddCommand
        ("SetShowModificationTime", Set_Show_Modification_Time_Command'Access);
      AddCommand("SetShowPreview", Set_Show_Preview_Command'Access);
      AddCommand("SetScaleImages", Set_Scale_Images_Command'Access);
   end AddCommands;

end Preferences.Commands;
