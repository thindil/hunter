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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with LoadData; use LoadData;
with Preferences; use Preferences;
with MainWindow; use MainWindow;
with Utils; use Utils;

package body SearchItems is

   -- ****o* SearchItems/Toggle_Search_Command
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
   -- ToggleSearch
   -- SOURCE
   function Toggle_Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextFrame: constant Ttk_Frame :=
        Get_Widget(".mainframe.textframe", Interp);
      Button: Ttk_Button := Get_Widget(TextFrame & ".closebutton");
      TextEntry: constant Ttk_Entry := Get_Widget(TextFrame & ".textentry");
      Hunter_Search_Exception: exception;
   begin
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.searchbutton");
         State(Button, "selected");
         Button.Name := New_String(TextFrame & ".okbutton");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Add
           (TextEntry,
            Mc
              (Interp,
               "{Enter the name of the file or directory to search for}"));
         Bind(TextEntry, "<KeyRelease>", "{Search}");
         Focus(TextEntry);
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
      else
         if Invoke(Button) /= "" then
            raise Hunter_Search_Exception
              with Mc(Interp, "{Can't hide search text bar}");
         end if;
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.searchbutton");
         State(Button, "!selected");
         UpdateDirectoryList;
      end if;
      return TCL_OK;
   end Toggle_Search_Command;

   -- ****o* SearchItems/Search_Command
   -- FUNCTION
   -- Search current directory for the selected text (case insensitive) and
   -- show only matching files and directories
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Search
   -- SOURCE
   function Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextEntry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
      DirectoryTree: constant Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.directoryframe.directorytree", Interp);
      Query: Unbounded_String;
      Selected: Boolean := False;
   begin
      Query := To_Unbounded_String(Get(TextEntry));
      if Length(Query) = 0 then
         UpdateDirectoryList;
         return TCL_OK;
      end if;
      for I in ItemsList.First_Index .. ItemsList.Last_Index loop
         if Index
             (To_Lower(To_String(ItemsList(I).Name)),
              To_Lower(To_String(Query))) =
           0 then
            Detach(DirectoryTree, Positive'Image(I));
         elsif (Settings.ShowHidden and ItemsList(I).IsHidden) or
           not ItemsList(I).IsHidden then
            Move(DirectoryTree, Positive'Image(I), "{}", Natural'Image(I - 1));
            if not Selected then
               Selection_Set(DirectoryTree, Positive'Image(I));
               Selected := True;
            end if;
         end if;
      end loop;
      return TCL_OK;
   end Search_Command;

   procedure CreateSearchUI is
   begin
      AddCommand("ToggleSearch", Toggle_Search_Command'Access);
      AddCommand("Search", Search_Command'Access);
   end CreateSearchUI;

end SearchItems;
