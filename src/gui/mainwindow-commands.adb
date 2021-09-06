-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Modules; use Modules;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body MainWindow.Commands is

   -- ****it* MCommands/MCommands.ExitCommand
   -- FUNCTION
   -- Used in creating exit handler for the program
   -- SOURCE
   package ExitCommand is new Tcl.Ada.Generic_ExitHandler
     (ClientData => Integer);
   -- ****

   -- ****o* MCommands/MCommands.Sort_Command
   -- FUNCTION
   -- Sort directory view based on which header was clicked
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Sort columnname
   -- Columnname is a name of column which will be used for sorting. Possible
   -- values are name, modified, size and previewname
   -- SOURCE
   function Sort_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Directory_Tree: Ttk_Tree_View :=
        Get_Widget
          (pathName => ".mainframe.paned.directoryframe.directorytree",
           Interp => Interp);
   begin
      Heading
        (TreeViewWidget => Directory_Tree, Column => "name",
         Options => "-image {}");
      Heading
        (TreeViewWidget => Directory_Tree, Column => "modified",
         Options => "-image {}");
      Heading
        (TreeViewWidget => Directory_Tree, Column => "size",
         Options => "-image {}");
      if CArgv.Arg(Argv => Argv, N => 1) = "name" then
         if Sort_Order = NAMEASC then
            Sort_Order := NAMEDESC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "name",
               Options => "-image {arrow-up}");
         else
            Sort_Order := NAMEASC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "name",
               Options => "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "modified" then
         if Sort_Order = MODIFIEDASC then
            Sort_Order := MODIFIEDDESC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "modified",
               Options => "-image {arrow-up}");
         else
            Sort_Order := MODIFIEDASC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "modified",
               Options => "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "size" then
         if Sort_Order = SIZEASC then
            Sort_Order := SIZEDESC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "size",
               Options => "-image {arrow-up}");
         else
            Sort_Order := SIZEASC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "size",
               Options => "-image {arrow-down}");
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "previewname" then
         Directory_Tree.Name :=
           New_String(Str => ".mainframe.paned.previewframe.directorytree");
         if Sort_Order = NAMEASC then
            Sort_Order := NAMEDESC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "name",
               Options => "-image {arrow-up}");
         else
            Sort_Order := NAMEASC;
            Heading
              (TreeViewWidget => Directory_Tree, Column => "name",
               Options => "-image {arrow-down}");
         end if;
         Items_Sorting.Sort(Container => Second_Items_List);
         Update_Directory_List(Clear => True, Frame_Name => "preview");
         return TCL_OK;
      end if;
      Items_Sorting.Sort(Container => Items_List);
      Update_Directory_List(Clear => True);
      return TCL_OK;
   end Sort_Command;

   -- ****o* MCommands/MCommands.Quit_Command
   -- FUNCTION
   -- Save preferences and clear trash on exit from the program
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- COMMANDS
   -- Quit
   -- SOURCE
   procedure Quit_Command(Client_Data: Integer) with
      Convention => C;
      -- ****

   procedure Quit_Command(Client_Data: Integer) is
      pragma Unreferenced(Client_Data);
      Main_Window: constant Tk_Toplevel :=
        Get_Main_Window(Interp => Get_Context);
      Error_Button: constant Ttk_Button :=
        Get_Widget(pathName => ".errorbutton");
   begin
      if Winfo_Get(Widgt => Error_Button, Info => "exists") = "0" then
         Settings.Window_Width :=
           Positive'Value(Winfo_Get(Widgt => Main_Window, Info => "width"));
         Settings.Window_Height :=
           Positive'Value(Winfo_Get(Widgt => Main_Window, Info => "height"));
      end if;
      Save_Preferences;
      Execute_Modules(State => On_Quit);
      if Settings.Clear_Trash_On_Exit then
         New_Action := CLEARTRASH;
         if Delete_Selected then
            null;
         end if;
      end if;
      Inotify_Close;
      Magic_Close;
   end Quit_Command;

   -- ****o* MCommands/MCommands.Hide_Widget_Command
   -- FUNCTION
   -- Hide text entry or message, depends on which is visible
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- HideWidget
   -- SOURCE
   function Hide_Widget_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_Widget_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Frame: Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.message", Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget
          (pathName => ".mainframe.toolbars.actiontoolbar.searchbutton",
           Interp => Interp);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => ".mainframe.textframe.textentry", Interp => Interp);
   begin
      if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         return
           Close_Command
             (ClientData => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      Frame.Name := New_String(Str => ".mainframe.textframe");
      if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         State(Widget => Button, StateSpec => "!selected");
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.itemtoolbar.openwithbutton");
         State(Widget => Button, StateSpec => "!selected");
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.actiontoolbar.renamebutton");
         Toggle_Tool_Buttons(Action => New_Action, Finished => True);
         if New_Action = CREATELINK then
            New_Action := COPY;
            ShowPreview;
         end if;
         if State(Widget => Button) = "selected" then
            State(Widget => Button, StateSpec => "!selected");
            New_Action := COPY;
         end if;
         Delete
           (TextEntry => Text_Entry, FirstIndex => "0", LastIndex => "end");
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Frame);
         return TCL_OK;
      end if;
      Frame.Name :=
        New_String
          (Str => ".mainframe.paned.previewframe.infoframe.applicationsmenu");
      if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         return
           Toggle_Applications_Menu_Command
             (ClientData => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Selection_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Directory_Tree: constant Ttk_Tree_View :=
        Get_Widget
          (pathName => ".mainframe.paned.directoryframe.directorytree",
           Interp => Interp);
   begin
      if Selection(TreeViewWidget => Directory_Tree) =
        Children(TreeViewWidget => Directory_Tree, Item => "{}") then
         Update_Directory_List;
      else
         Selection_Set
           (TreeViewWidget => Directory_Tree,
            Items =>
              "[list " &
              Children(TreeViewWidget => Directory_Tree, Item => "{}") & " ]");
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Arrange_Path_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Path_Buttons_Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Buttons: Unbounded_String;
      Tokens: Slice_Set;
      Row, Column, Width: Natural := 0;
      Button: Ttk_Button; --## rule line off IMPROPER_INITIALIZATION
      Preview_Canvas: constant Ttk_Frame :=
        Get_Widget
          (pathName => ".mainframe.paned.previewframe.previewcanvas",
           Interp => Interp);
   begin
      Buttons :=
        To_Unbounded_String
          (Source =>
             Tcl.Tk.Ada.Grid.Grid_Slaves(Master => Path_Buttons_Frame));
      if Buttons = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Create
        (S => Tokens, From => To_String(Source => Buttons), Separators => " ");
      Button.Interp := Interp;
      Arrange_Buttons_Loop :
      for I in reverse 1 .. Slice_Count(S => Tokens) loop
         Button.Name := New_String(Str => Slice(S => Tokens, Index => I));
         Width :=
           Width + Positive'Value(Winfo_Get(Widgt => Button, Info => "width"));
         if Width > Positive'Value(CArgv.Arg(Argv => Argv, N => 2)) then
            Row := Row + 1;
            Width := 0;
            Column := 0;
         end if;
         Tcl.Tk.Ada.Grid.Grid_Configure
           (Slave => Button,
            Options =>
              "-row" & Natural'Image(Row) & " -column" &
              Natural'Image(Column));
         Column := Column + 1;
      end loop Arrange_Buttons_Loop;
      if (Settings.Scale_Images and Settings.Show_Preview)
        and then Winfo_Get(Widgt => Preview_Canvas, Info => "ismapped") =
          "1" then
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Cancel_Action_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Action_Button: Ttk_Button :=
        Get_Widget
          (pathName => ".mainframe.toolbars.actiontoolbar.copybutton",
           Interp => Interp);
   begin
      Toggle_Tool_Buttons(Action => New_Action, Finished => True);
      case New_Action is
         when COPY =>
            Copy_Items_List.Clear;
         when MOVE =>
            MoveItemsList.Clear;
            Action_Button.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.movebutton");
         when others =>
            return TCL_OK;
      end case;
      ShowPreview;
      if State(Widget => Action_Button) = "selected" then
         State(Widget => Action_Button, StateSpec => "!selected");
      end if;
      Unbind_From_Main_Window(Interp => Interp, Sequence => "<Escape>");
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      File_Menu: constant Tk_Menu :=
        Get_Widget(pathName => ".filemenu", Interp => Interp);
      Button: Ttk_Button; --## rule line off IMPROPER_INITIALIZATION
      Buttons_Names: constant array(1 .. 8) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "itemtoolbar.runbutton"),
         2 => To_Unbounded_String(Source => "itemtoolbar.openbutton"),
         3 => To_Unbounded_String(Source => "itemtoolbar.openwithbutton"),
         4 => To_Unbounded_String(Source => "actiontoolbar.renamebutton"),
         5 => To_Unbounded_String(Source => "actiontoolbar.copybutton"),
         6 => To_Unbounded_String(Source => "actiontoolbar.movebutton"),
         7 => To_Unbounded_String(Source => "actiontoolbar.deletebutton"),
         8 => To_Unbounded_String(Source => "actiontoolbar.selectbutton"));
      Menu_Labels: constant array(Buttons_Names'Range) of Unbounded_String :=
        (1 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Execute}")),
         2 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Open}")),
         3 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Open with...}")),
         4 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Rename}")),
         5 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Copy}")),
         6 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Move}")),
         7 =>
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Delete}")),
         8 =>
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interp, Src_String => "{Select/Deselect all}")));
   begin
      Delete(File_Menu, "0", "end");
      Button.Interp := Interp;
      Update_File_Menu_Loop :
      for I in Buttons_Names'Range loop
         Button.Name :=
           New_String(".mainframe.toolbars." & To_String(Buttons_Names(I)));
         if Winfo_Get(Button, "ismapped") = "1" then
            if I /= 7 then
               Add
                 (File_Menu, "command",
                  "-label {" & To_String(Menu_Labels(I)) & "} -command {" &
                  Widget_Image(Button) & " invoke}");
            else
               Add
                 (File_Menu, "command",
                  "-label {" & To_String(Menu_Labels(I)) &
                  "} -command {.deletemenu invoke 0}");
            end if;
         end if;
      end loop Update_File_Menu_Loop;
      Tk_Popup(File_Menu, CArgv.Arg(Argv, 1), CArgv.Arg(Argv, 2));
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_File_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      if Ada.Directories.Exists
          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
         Current_Directory :=
           To_Unbounded_String(Value("APPDIR", "") & "/usr/share/doc/hunter");
      else
         Current_Directory :=
           To_Unbounded_String
             (Normalize_Pathname
                (Containing_Directory(Containing_Directory(Command_Name))));
      end if;
      Load_Directory(To_String(Current_Directory));
      Set_Current_Selected_Loop :
      for I in Items_List.Iterate loop
         if Items_List(I).Name = To_Unbounded_String(CArgv.Arg(Argv, 1)) then
            Current_Selected := Items_List(I).Path;
            exit Set_Current_Selected_Loop;
         end if;
      end loop Set_Current_Selected_Loop;
      Update_Directory_List(True);
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Invoke_Button_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Button: constant Ttk_Button := Get_Widget(CArgv.Arg(Argv, 1), Interp);
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
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Invoke_Button_Command;

   procedure Add_Commands is
   begin
      Add_Command("Sort", Sort_Command'Access);
      Add_Command("HideWidget", Hide_Widget_Command'Access);
      Add_Command("ToggleSelection", Toggle_Selection_Command'Access);
      Add_Command("ArrangePath", Arrange_Path_Command'Access);
      Add_Command("CancelAction", Cancel_Action_Command'Access);
      Add_Command("ShowFileMenu", Show_File_Menu_Command'Access);
      Add_Command("ShowFile", Show_File_Command'Access);
      Add_Command("InvokeButton", Invoke_Button_Command'Access);
      ExitCommand.Tcl_CreateExitHandler(Quit_Command'Access, 0);
   end Add_Commands;

end MainWindow.Commands;
