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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Common; use Common;
with Bookmarks.UI; use Bookmarks.UI;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with RefreshData; use RefreshData;
with Utils.UI; use Utils.UI;

package body Bookmarks.Commands.UI is

   -- ****if* Commands/Commands.UpdateNew_Action
   -- FUNCTION
   -- Update New_Action and toolbars if needed
   -- SOURCE
   procedure UpdateNew_Action is
      -- ****
   begin
      if New_Action /= MOVE then
         if New_Action = SHOWTRASH then
            Toggle_Tool_Buttons(New_Action, True);
         end if;
         New_Action := COPY;
      end if;
   end UpdateNew_Action;

   -- ****o* Commands/Commands.SetDestination_Command
   -- FUNCTION
   -- Show text entry to enter directory destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDestination
   -- SOURCE
   function SetDestination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function SetDestination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextFrame: constant Ttk_Frame :=
        Get_Widget(".mainframe.textframe", Interp);
      Button: Ttk_Button := Get_Widget(TextFrame & ".okbutton", Interp);
      TextEntry: constant Ttk_Entry :=
        Get_Widget(TextFrame & ".textentry", Interp);
   begin
      UpdateNew_Action;
      configure(Button, "-command GoToDestination");
      Add(Button, Mc(Interp, "{Go to the selected destination}"));
      Focus(TextEntry);
      Insert(TextEntry, "0", To_String(Common.Current_Directory));
      Add(TextEntry, Mc(Interp, "{Enter the selected destination}"));
      Unbind(TextEntry, "<KeyRelease>");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button.Name := New_String(".mainframe.textframe.closebutton");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
      return TCL_OK;
   end SetDestination_Command;

   -- ****o* Commands/Commands.GoToDestination_Command
   -- FUNCTION
   -- Go to the destination directory selected by the user
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToDestination
   -- SOURCE
   function GoToDestination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToDestination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextEntry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
      HideButton: constant Ttk_Button :=
        Get_Widget(".mainframe.textframe.closebutton", Interp);
   begin
      UpdateNew_Action;
      if not Ada.Directories.Exists(Get(TextEntry)) then
         Show_Message
           (Mc(Interp, "{Directory}") & " '" & Get(TextEntry) & "' " &
            Mc(Interp, "{doesn't exist.}"));
         return TCL_OK;
      end if;
      Common.Current_Directory := To_Unbounded_String(Get(TextEntry));
      if Invoke(HideButton) /= "" then
         return TCL_ERROR;
      end if;
      Load_Directory(To_String(Common.Current_Directory));
      Update_Directory_List(True);
      Update_Watch(To_String(Common.Current_Directory));
      return TCL_OK;
   end GoToDestination_Command;

   -- ****o* Commands/Commands.Add_Bookmark_Command
   -- FUNCTION
   -- Add the bookmark to the selected directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AddBookmark
   -- SOURCE
   function Add_Bookmark_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Add_Bookmark_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      File: File_Type;
   begin
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         Open(File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      else
         Create_Path(Value("HOME") & "/.config/gtk-3.0/");
         Create
           (File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Put_Line(File, "file://" & Current_Selected);
      Close(File);
      Create_Bookmark_Menu;
      Set_Bookmark_Button;
      return TCL_OK;
   end Add_Bookmark_Command;

   -- ****o* Commands/Commands.Remove_Bookmark_Command
   -- FUNCTION
   -- Remove the bookmark to the selected directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RemoveBookmark
   -- SOURCE
   function Remove_Bookmark_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Remove_Bookmark_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      NewFile, OldFile: File_Type;
      Line, Path: Unbounded_String;
      Added: Boolean := False;
   begin
      Rename
        (Value("HOME") & "/.config/gtk-3.0/bookmarks",
         Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Open(OldFile, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Create(NewFile, Out_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      Update_Bookmarks_Loop :
      while not End_Of_File(OldFile) loop
         Line := Get_Line(OldFile);
         if Length(Line) > 7 and then Slice(Line, 1, 7) = "file://" then
            Path := Unbounded_Slice(Line, 8, Length(Line));
            if Path /= Current_Selected then
               Put_Line(NewFile, Line);
               Added := True;
            end if;
         end if;
      end loop Update_Bookmarks_Loop;
      Close(NewFile);
      Close(OldFile);
      Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      if not Added then
         Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Create_Bookmark_Menu;
      Set_Bookmark_Button;
      return TCL_OK;
   end Remove_Bookmark_Command;

   procedure AddUICommands is
      use Utils;

   begin
      Add_Command("SetDestination", SetDestination_Command'Access);
      Add_Command("GoToDestination", GoToDestination_Command'Access);
      Add_Command("AddBookmark", Add_Bookmark_Command'Access);
      Add_Command("RemoveBookmark", Remove_Bookmark_Command'Access);
   end AddUICommands;

end Bookmarks.Commands.UI;
