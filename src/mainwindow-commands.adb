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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with CopyItems; use CopyItems;
with DeleteItems; use DeleteItems;
with Inotify; use Inotify;
with LibMagic; use LibMagic;
with LoadData; use LoadData;
with Messages; use Messages;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body MainWindow.Commands is

   -- ****it* MCommands/MCommands.ExitCommand
   -- FUNCTION
   -- Used in creating exit handler for the program
   -- SOURCE
   package ExitCommand is new Tcl.Ada.Generic_ExitHandler(Integer);
   -- ****

   -- ****o* MCommands/MCommands.Sort_Command
   -- FUNCTION
   -- Sort directory view based on which header was clicked
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Sort columnname
   -- Columnname is a name of column which will be used for sorting. Possible
   -- values are name, modified, size and previewname
   -- SOURCE
   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      DirectoryTree: Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.directoryframe.directorytree", Interp);
   begin
      Heading(DirectoryTree, "name", "-image {}");
      Heading(DirectoryTree, "modified", "-image {}");
      Heading(DirectoryTree, "size", "-image {}");
      if CArgv.Arg(Argv, 1) = "name" then
         if SortOrder = NameAsc then
            SortOrder := NameDesc;
            Heading(DirectoryTree, "name", "-image {arrow-up}");
         else
            SortOrder := NameAsc;
            Heading(DirectoryTree, "name", "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv, 1) = "modified" then
         if SortOrder = ModifiedAsc then
            SortOrder := ModifiedDesc;
            Heading(DirectoryTree, "modified", "-image {arrow-up}");
         else
            SortOrder := ModifiedAsc;
            Heading(DirectoryTree, "modified", "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv, 1) = "size" then
         if SortOrder = SizeAsc then
            SortOrder := SizeDesc;
            Heading(DirectoryTree, "size", "-image {arrow-up}");
         else
            SortOrder := SizeAsc;
            Heading(DirectoryTree, "size", "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv, 1) = "previewname" then
         DirectoryTree.Name :=
           New_String(".mainframe.paned.previewframe.directorytree");
         if SortOrder = NameAsc then
            SortOrder := NameDesc;
            Heading(DirectoryTree, "name", "-image {arrow-up}");
         else
            SortOrder := NameAsc;
            Heading(DirectoryTree, "name", "-image {arrow-down}");
         end if;
         Items_Sorting.Sort(SecondItemsList);
         UpdateDirectoryList(True, "preview");
         return TCL_OK;
      end if;
      Items_Sorting.Sort(ItemsList);
      UpdateDirectoryList(True);
      return TCL_OK;
   end Sort_Command;

   -- ****o* MCommands/MCommands.Quit_Command
   -- FUNCTION
   -- Save preferences and clear trash on exit from the program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- COMMANDS
   -- Quit
   -- SOURCE
   procedure Quit_Command(ClientData: in Integer) with
      Convention => C;
      -- ****

   procedure Quit_Command(ClientData: in Integer) is
      pragma Unreferenced(ClientData);
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      ErrorButton: constant Ttk_Button := Get_Widget(".errorbutton");
   begin
      if Winfo_Get(ErrorButton, "exists") = "0" then
         Settings.WindowWidth :=
           Positive'Value(Winfo_Get(MainWindow, "width"));
         Settings.WindowHeight :=
           Positive'Value(Winfo_Get(MainWindow, "height"));
      end if;
      SavePreferences;
      if Settings.ClearTrashOnExit then
         NewAction := CLEARTRASH;
         if DeleteSelected then
            null;
         end if;
      end if;
      InotifyClose;
      MagicClose;
   end Quit_Command;

   -- ****o* MCommands/MCommands.Hide_Widget_Command
   -- FUNCTION
   -- Hide text entry or message, depends on which is visible
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- HideWidget
   -- SOURCE
   function Hide_Widget_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_Widget_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      Frame: Ttk_Frame := Get_Widget(".mainframe.message", Interp);
      Button: Ttk_Button :=
        Get_Widget(".mainframe.toolbars.actiontoolbar.searchbutton", Interp);
      TextEntry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
   begin
      if Winfo_Get(Frame, "ismapped") = "1" then
         return Close_Command(ClientData, Interp, Argc, Argv);
      end if;
      Frame.Name := New_String(".mainframe.textframe");
      if Winfo_Get(Frame, "ismapped") = "1" then
         State(Button, "!selected");
         Button.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.openwithbutton");
         State(Button, "!selected");
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.renamebutton");
         ToggleToolButtons(NewAction, True);
         if NewAction = CREATELINK then
            NewAction := COPY;
            ShowPreview;
         end if;
         if State(Button) = "selected" then
            State(Button, "!selected");
            NewAction := COPY;
         end if;
         Delete(TextEntry, "0", "end");
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
         return TCL_OK;
      end if;
      Frame.Name :=
        New_String(".mainframe.paned.previewframe.infoframe.applicationsmenu");
      if Winfo_Get(Frame, "ismapped") = "1" then
         return Toggle_Applications_Menu_Command
             (ClientData, Interp, Argc, Argv);
      end if;
      return TCL_OK;
   end Hide_Widget_Command;

   -- ****o* MCommands/MCommands.Toggle_Selection_Command
   -- FUNCTION
   -- Select all or deselect all items in directory view
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleSelection
   -- SOURCE
   function Toggle_Selection_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Selection_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DirectoryTree: constant Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.directoryframe.directorytree", Interp);
   begin
      if Selection(DirectoryTree) = Children(DirectoryTree, "{}") then
         UpdateDirectoryList;
      else
         Selection_Set
           (DirectoryTree, "[list " & Children(DirectoryTree, "{}") & " ]");
      end if;
      return TCL_OK;
   end Toggle_Selection_Command;

   -- ****o* MCommands/MCommands.Arrange_Path_Command
   -- FUNCTION
   -- Arrange path buttons when they window were resized
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ArrangePath buttonsframe width
   -- Buttonsframe is the pathname of the frame which hold buttons which will
   -- be resized. Width is the new width for the buttonsframe
   -- SOURCE
   function Arrange_Path_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Arrange_Path_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      PathButtonsFrame: constant Ttk_Frame :=
        Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Buttons: Unbounded_String;
      Tokens: Slice_Set;
      Row, Column, Width: Natural := 0;
      Button: Ttk_Button;
      PreviewCanvas: constant Ttk_Frame :=
        Get_Widget(".mainframe.paned.previewframe.previewcanvas", Interp);
   begin
      Buttons :=
        To_Unbounded_String(Tcl.Tk.Ada.Grid.Grid_Slaves(PathButtonsFrame));
      if Buttons = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Create(Tokens, To_String(Buttons), " ");
      Button.Interp := Interp;
      for I in reverse 1 .. Slice_Count(Tokens) loop
         Button.Name := New_String(Slice(Tokens, I));
         Width := Width + Positive'Value(Winfo_Get(Button, "width"));
         if Width > Positive'Value(CArgv.Arg(Argv, 2)) then
            Row := Row + 1;
            Width := 0;
            Column := 0;
         end if;
         Tcl.Tk.Ada.Grid.Grid_Configure
           (Button,
            "-row" & Natural'Image(Row) & " -column" & Natural'Image(Column));
         Column := Column + 1;
      end loop;
      if (Settings.ScaleImages and Settings.ShowPreview)
        and then Winfo_Get(PreviewCanvas, "ismapped") = "1" then
         ScaleImage;
      end if;
      return TCL_OK;
   end Arrange_Path_Command;

   -- ****o* MCommands/MCommands.Cancel_Action_Command
   -- FUNCTION
   -- Select all or deselect all items in directory view
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK;
   -- COMMANDS
   -- CancelAction
   -- SOURCE
   function Cancel_Action_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Action_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ActionButton: Ttk_Button;
   begin
      ActionButton.Interp := Interp;
      if NewAction = COPY then
         CopyItemsList.Clear;
         ActionButton.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.copybutton");
         ShowPreview;
      elsif NewAction = MOVE then
         MoveItemsList.Clear;
         ActionButton.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.movebutton");
         ShowPreview;
      end if;
      if State(ActionButton) = "selected" then
         State(ActionButton, "!selected");
      end if;
      Unbind_From_Main_Window(Interp, "<Escape>");
      ToggleToolButtons(NewAction, True);
      return TCL_OK;
   end Cancel_Action_Command;

   -- ****o* MCommands/MCommands.Show_File_Menu_Command
   -- FUNCTION
   -- Show menu for the selected items in current directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowFileMenu x y
   -- X is X coordinate where menu will be show, Y is Y coordinate where menu
   -- will be show
   -- SOURCE
   function Show_File_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      FileMenu: constant Tk_Menu := Get_Widget(".filemenu", Interp);
      Button: Ttk_Button;
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("itemtoolbar.runbutton"),
         To_Unbounded_String("itemtoolbar.openbutton"),
         To_Unbounded_String("itemtoolbar.openwithbutton"),
         To_Unbounded_String("actiontoolbar.renamebutton"),
         To_Unbounded_String("actiontoolbar.copybutton"),
         To_Unbounded_String("actiontoolbar.movebutton"),
         To_Unbounded_String("actiontoolbar.deletebutton"),
         To_Unbounded_String("actiontoolbar.selectbutton"));
      MenuLabels: constant array(ButtonsNames'Range) of Unbounded_String :=
        (To_Unbounded_String(Mc(Interp, "{Execute}")),
         To_Unbounded_String(Mc(Interp, "{Open}")),
         To_Unbounded_String(Mc(Interp, "{Open with...}")),
         To_Unbounded_String(Mc(Interp, "{Rename}")),
         To_Unbounded_String(Mc(Interp, "{Copy}")),
         To_Unbounded_String(Mc(Interp, "{Move}")),
         To_Unbounded_String(Mc(Interp, "{Delete}")),
         To_Unbounded_String(Mc(Interp, "{Select/Deselect all}")));
   begin
      Delete(FileMenu, "0", "end");
      Button.Interp := Interp;
      for I in ButtonsNames'Range loop
         Button.Name :=
           New_String(".mainframe.toolbars." & To_String(ButtonsNames(I)));
         if Winfo_Get(Button, "ismapped") = "1" then
            if I /= 7 then
               Add
                 (FileMenu, "command",
                  "-label {" & To_String(MenuLabels(I)) & "} -command {" &
                  Widget_Image(Button) & " invoke}");
            else
               Add
                 (FileMenu, "command",
                  "-label {" & To_String(MenuLabels(I)) &
                  "} -command {.deletemenu invoke 0}");
            end if;
         end if;
      end loop;
      Tk_Popup(FileMenu, CArgv.Arg(Argv, 1), CArgv.Arg(Argv, 2));
      return TCL_OK;
   end Show_File_Menu_Command;

   -- ****o* MCommands/MCommands.Show_File_Command
   -- FUNCTION
   -- Show content of the selected file. Used in about menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowFile filename
   -- Filename is the name of the file which preview will be show
   -- SOURCE
   function Show_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      CurrentDirectory :=
        To_Unbounded_String
          (Normalize_Pathname
             (Containing_Directory(Containing_Directory(Command_Name))));
      if Ada.Directories.Exists
          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
         CurrentDirectory :=
           To_Unbounded_String(Value("APPDIR", "") & "/usr/share/doc/hunter");
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      for I in ItemsList.Iterate loop
         if ItemsList(I).Name = To_Unbounded_String(CArgv.Arg(Argv, 1)) then
            CurrentSelected := CurrentDirectory & "/" & ItemsList(I).Name;
            exit;
         end if;
      end loop;
      UpdateDirectoryList(True);
      ShowPreview;
      return TCL_OK;
   end Show_File_Command;

   -- ****o* MCommands/MCommands.Invoke_Button_Command
   -- FUNCTION
   -- Invoke the selected button if it is mapped
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- InvokeButton buttonname
   -- Buttonname is pathname of the button which will be invoked
   -- SOURCE
   function Invoke_Button_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Invoke_Button_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Button: constant Ttk_Button := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Hunter_Button_Exception: exception;
      Menu: Tk_Menu;
   begin
      if Winfo_Get(Button, "ismapped") = "0" then
         return TCL_OK;
      end if;
      Menu.Interp := Interp;
      if CArgv.Arg(Argv, 1) =
        ".mainframe.toolbars.actiontoolbar.bookmarksbutton" then
         Menu.Name := New_String(".bookmarksmenu");
         Tk_Popup
           (Menu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv, 1) =
        ".mainframe.toolbars.actiontoolbar.newbutton" then
         Menu.Name := New_String(".newmenu");
         Tk_Popup
           (Menu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv, 1) =
        ".mainframe.toolbars.actiontoolbar.deletebutton" then
         Menu.Name := New_String(".deletemenu");
         Tk_Popup
           (Menu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv, 1) =
        ".mainframe.toolbars.actiontoolbar.aboutbutton" then
         Menu.Name := New_String(".aboutmenu");
         Tk_Popup
           (Menu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv, 1) =
        ".mainframe.toolbars.actiontoolbar.userbutton" then
         Menu.Name := New_String(".actionsmenu");
         Tk_Popup
           (Menu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
         return TCL_OK;
      end if;
      if Invoke(Button) /= "" then
         raise Hunter_Button_Exception
           with Mc(Interp, "{Can't invoke button }") & CArgv.Arg(Argv, 1);
      end if;
      return TCL_OK;
   end Invoke_Button_Command;

   procedure AddCommands is
   begin
      AddCommand("Sort", Sort_Command'Access);
      AddCommand("HideWidget", Hide_Widget_Command'Access);
      AddCommand("ToggleSelection", Toggle_Selection_Command'Access);
      AddCommand("ArrangePath", Arrange_Path_Command'Access);
      AddCommand("CancelAction", Cancel_Action_Command'Access);
      AddCommand("ShowFileMenu", Show_File_Menu_Command'Access);
      AddCommand("ShowFile", Show_File_Command'Access);
      AddCommand("InvokeButton", Invoke_Button_Command'Access);
      ExitCommand.Tcl_CreateExitHandler(Quit_Command'Access, 0);
   end AddCommands;

end MainWindow.Commands;
