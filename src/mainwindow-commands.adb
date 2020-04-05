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
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body MainWindow.Commands is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   package ExitCommand is new Tcl.Ada.Generic_ExitHandler(Integer);

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* MainWindow-Commands/Sort_Command
      -- FUNCTION
      -- Sort directory view based on which header was clicked
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      -- ****
      DirectoryTree: Ttk_Tree_View;
   begin
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      Heading(DirectoryTree, "name", "-image """"");
      Heading(DirectoryTree, "modified", "-image """"");
      Heading(DirectoryTree, "size", "-image """"");
      if CArgv.Arg(Argv, 1) = "name" then
         if SortOrder = NameAsc then
            SortOrder := NameDesc;
            Heading(DirectoryTree, "name", "-image ""arrow-up""");
         else
            SortOrder := NameAsc;
            Heading(DirectoryTree, "name", "-image ""arrow-down""");
         end if;
      elsif CArgv.Arg(Argv, 1) = "modified" then
         if SortOrder = ModifiedAsc then
            SortOrder := ModifiedDesc;
            Heading(DirectoryTree, "modified", "-image ""arrow-up""");
         else
            SortOrder := ModifiedAsc;
            Heading(DirectoryTree, "modified", "-image ""arrow-down""");
         end if;
      elsif CArgv.Arg(Argv, 1) = "size" then
         if SortOrder = SizeAsc then
            SortOrder := SizeDesc;
            Heading(DirectoryTree, "size", "-image ""arrow-up""");
         else
            SortOrder := SizeAsc;
            Heading(DirectoryTree, "size", "-image ""arrow-down""");
         end if;
      elsif CArgv.Arg(Argv, 1) = "previewname" then
         DirectoryTree.Interp := Get_Context;
         DirectoryTree.Name :=
           New_String(".mainframe.paned.previewframe.directorytree");
         if SortOrder = NameAsc then
            SortOrder := NameDesc;
            Heading(DirectoryTree, "name", "-image ""arrow-up""");
         else
            SortOrder := NameAsc;
            Heading(DirectoryTree, "name", "-image ""arrow-down""");
         end if;
         Items_Sorting.Sort(SecondItemsList);
         UpdateDirectoryList(True, "preview");
         return TCL_OK;
      end if;
      Items_Sorting.Sort(ItemsList);
      UpdateDirectoryList(True);
      return TCL_OK;
   end Sort_Command;

   procedure Quit_Command(ClientData: in Integer) with
      Convention => C;

      -- ****if* MainWindow-Commands/Quit_Command
      -- FUNCTION
      -- Save preferences and clear trash on exit from the program
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- SOURCE
   procedure Quit_Command(ClientData: in Integer) is
      pragma Unreferenced(ClientData);
      -- ****
   begin
      Settings.WindowWidth :=
        Positive'Value(Winfo_Get(Get_Main_Window(Get_Context), "width"));
      Settings.WindowHeight :=
        Positive'Value(Winfo_Get(Get_Main_Window(Get_Context), "height"));
      SavePreferences;
      if Settings.ClearTrashOnExit then
         NewAction := CLEARTRASH;
         if DeleteSelected then
            null;
         end if;
      end if;
   end Quit_Command;

   function Hide_Entry_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* MainWindow-Commands/Hide_Entry_Command
      -- FUNCTION
      -- Hide text entry and clear its content
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Hide_Entry_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      TextFrame: Ttk_Frame;
      TextEntry: Ttk_Entry;
   begin
      TextEntry.Interp := Get_Context;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Delete(TextEntry, "0", "end");
      TextFrame.Interp := Get_Context;
      TextFrame.Name := New_String(".mainframe.textframe");
      Tcl.Tk.Ada.Grid.Grid_Remove(TextFrame);
      return TCL_OK;
   end Hide_Entry_Command;

   function Hide_Widget_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* MainWindow-Commands/Hide_Widget_Command
      -- FUNCTION
      -- Hide text entry or message, depends on which is visible
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Hide_Widget_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      Frame: Ttk_Frame;
      Button: Ttk_Button;
   begin
      Frame.Interp := Get_Context;
      Button.Interp := Get_Context;
      Frame.Name := New_String(".mainframe.message");
      if Winfo_Get(Frame, "ismapped") = "1" then
         Button.Name :=
           New_String(".mainframe.message.buttonsbox.buttonclose");
         if Invoke(Button) /= "" then
            raise Program_Error with "Can't hide message";
         end if;
         return TCL_OK;
      end if;
      Frame.Name := New_String(".mainframe.textframe");
      if Winfo_Get(Frame, "ismapped") = "1" then
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.searchbutton");
         State(Button, "!selected");
         Button.Name := New_String(".mainframe.textframe.closebutton");
         if Invoke(Button) /= "" then
            raise Program_Error with "Can't hide text entry";
         end if;
         return TCL_OK;
      end if;
      return TCL_OK;
   end Hide_Widget_Command;

   function Toggle_Selection_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* MainWindow-Commands/Toggle_Selection_Command
      -- FUNCTION
      -- Select all or deselect all items in directory view
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Toggle_Selection_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      DirectoryTree: Ttk_Tree_View;
   begin
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      if Selection(DirectoryTree) = Children(DirectoryTree, "{}") then
         UpdateDirectoryList;
      else
         Selection_Set
           (DirectoryTree, "[list " & Children(DirectoryTree, "{}") & " ]");
      end if;
      return TCL_OK;
   end Toggle_Selection_Command;

   function Arrange_Path_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* MainWindow-Commands/Arrange_Path_Command
      -- FUNCTION
      -- Arrange path buttons when they window were resized
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command
      -- SOURCE
   function Arrange_Path_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      -- ****
      PathButtonsFrame: Ttk_Frame;
      Buttons: Unbounded_String;
      Tokens: Slice_Set;
      Row, Column, Width: Natural := 0;
      Button: Ttk_Button;
      PreviewCanvas: Ttk_Frame;
   begin
      PathButtonsFrame.Interp := Get_Context;
      PathButtonsFrame.Name := New_String(CArgv.Arg(Argv, 1));
      Buttons :=
        To_Unbounded_String(Tcl.Tk.Ada.Grid.Grid_Slaves(PathButtonsFrame));
      if Buttons = Null_Unbounded_String then
         return TCL_OK;
      end if;
      Create(Tokens, To_String(Buttons), " ");
      Button.Interp := PathButtonsFrame.Interp;
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
      PreviewCanvas.Name :=
        New_String(".mainframe.paned.previewframe.previewcanvas");
      PreviewCanvas.Interp := Get_Context;
      if (Settings.ScaleImages and Settings.ShowPreview)
        and then Winfo_Get(PreviewCanvas, "ismapped") = "1" then
         ScaleImage;
      end if;
      return TCL_OK;
   end Arrange_Path_Command;

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
      AddCommand("Sort", Sort_Command'Access);
      AddCommand("HideEntry", Hide_Entry_Command'Access);
      AddCommand("HideWidget", Hide_Widget_Command'Access);
      AddCommand("ToggleSelection", Toggle_Selection_Command'Access);
      AddCommand("ArrangePath", Arrange_Path_Command'Access);
      ExitCommand.Tcl_CreateExitHandler(Quit_Command'Access, 0);
   end AddCommands;

end MainWindow.Commands;
