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
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with Preferences; use Preferences;

package body MainWindow.Commands is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);
   package ExitCommand is new Tcl.Ada.Generic_ExitHandler(Integer);

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Sort_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
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
      end if;
      Items_Sorting.Sort(ItemsList);
      UpdateDirectoryList(True);
      return 0;
   end Sort_Command;

   procedure Quit_Command(ClientData: in Integer) with
      Convention => C;

   procedure Quit_Command(ClientData: in Integer) is
      pragma Unreferenced(ClientData);
   begin
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

   function Hide_Entry_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      TextFrame: Ttk_Frame;
      TextEntry: Ttk_Entry;
   begin
      TextEntry.Interp := Get_Context;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Delete(TextEntry, "0", "end");
      TextFrame.Interp := Get_Context;
      TextFrame.Name := New_String(".mainframe.textframe");
      Tcl.Tk.Ada.Grid.Grid_Remove(TextFrame);
      return 0;
   end Hide_Entry_Command;

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
      ExitCommand.Tcl_CreateExitHandler(Quit_Command'Access, 0);
   end AddCommands;

end MainWindow.Commands;
