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
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with LoadData; use LoadData;

package body SearchItems is

   package CreateCommands is new Tcl.Ada.Generic_Command(Integer);

   function Toggle_Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* SearchItems/Start_Search_Command
      -- FUNCTION
      -- Show text entry to enter directory destination
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Toggle_Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      TextFrame: Ttk_Frame;
      Button: Ttk_Button;
      TextEntry: Ttk_Entry;
   begin
      TextEntry.Interp := Get_Context;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Button.Interp := Get_Context;
      Button.Name := New_String(".mainframe.textframe.closebutton");
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name := New_String(".mainframe.textframe.okbutton");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Add
           (TextEntry,
            "Enter the name of the file or directory to search for");
         Bind(TextEntry, "<KeyRelease>", "{Search}");
         TextFrame.Interp := Get_Context;
         TextFrame.Name := New_String(".mainframe.textframe");
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
      else
         if Invoke(Button) /= "" then
            raise Program_Error with "Can't hide search text bar";
         end if;
      end if;
      return 0;
   end Toggle_Search_Command;

   function Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

   function Search_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      TextEntry: Ttk_Entry;
      DirectoryTree: Ttk_Tree_View;
      Query: Unbounded_String;
   begin
      TextEntry.Interp := Get_Context;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Query := To_Unbounded_String(Get(TextEntry));
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      if Length(Query) = 0 then
         for I in ItemsList.First_Index .. ItemsList.Last_Index loop
            Move(DirectoryTree, Positive'Image(I), "{}", Natural'Image(I - 1));
         end loop;
         return 0;
      end if;
      for I in ItemsList.First_Index .. ItemsList.Last_Index loop
         if Index
             (To_Lower(To_String(ItemsList(I).Name)),
              To_Lower(To_String(Query))) =
           0 then
            Detach(DirectoryTree, Positive'Image(I));
         else
            Move(DirectoryTree, Positive'Image(I), "{}", Natural'Image(I - 1));
         end if;
      end loop;
      return 0;
   end Search_Command;

   procedure CreateSearchUI is
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
      AddCommand("ToggleSearch", Toggle_Search_Command'Access);
      AddCommand("Search", Search_Command'Access);
   end CreateSearchUI;

   -- ****if* SearchItems/ToggleSearch
   -- FUNCTION
   -- Show or hide search text entry
   -- PARAMETERS
   -- Self - Gtk_Toggle_Button which was clicked. Unused.
   -- SOURCE
--   procedure ToggleSearch(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--   begin
--      if not Is_Visible(SearchEntry) then
--         Show_All(SearchEntry);
--         Grab_Focus(SearchEntry);
--      else
--         Set_Text(Gtk_GEntry(SearchEntry), "");
--         Hide(SearchEntry);
--         Grab_Focus(DirectoryView);
--      end if;
--   end ToggleSearch;
--
--   function VisibleItems
--     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
--   begin
--      if Setting then
--         return True;
--      end if;
--      if (Get_Int(Model, Iter, 1) = 1 or Get_Int(Model, Iter, 1) = 3) and
--        not Settings.ShowHidden then
--         return False;
--      end if;
--      if Get_Text(SearchEntry) = "" then
--         return True;
--      end if;
--      if Index
--          (To_Lower(Get_String(Model, Iter, 0)),
--           To_Lower(Get_Text(SearchEntry)), 1) >
--        0 then
--         return True;
--      end if;
--      return False;
--   end VisibleItems;
--
--   -- ****if* SearchItems/SearchItem
--   -- FUNCTION
--   -- Search for files and directories as user enter text in search entry
--   -- PARAMETERS
--   -- Self - Which search entry was used for search.
--   -- SOURCE
--   procedure SearchItem(Self: access Gtk_Search_Entry_Record'Class) is
--      -- ****
--      TreeView: Gtk_Tree_View := DirectoryView;
--   begin
--      if Get_Visible_Child_Name(InfoStack) = "destination" then
--         TreeView :=
--           Gtk_Tree_View
--             (Get_Child(Gtk_Scrolled_Window(Get_Visible_Child(InfoStack))));
--         Refilter
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model(TreeView)))));
--      else
--         Refilter
--           (-(Gtk.Tree_Model_Sort.Get_Model
--               (-(Gtk.Tree_View.Get_Model(DirectoryView)))));
--      end if;
--      if Gtk.Tree_Model_Sort.N_Children(-(Get_Model(TreeView)), Null_Iter) >
--        0 then
--         Set_Cursor(TreeView, Gtk_Tree_Path_New_From_String("0"), null, False);
--      else
--         CurrentSelected := CurrentDirectory;
--         PreviewItem(null);
--      end if;
--      Grab_Focus(Self);
--      Select_Region(Self, 0, 0);
--      Set_Position(Self, Get_Text(Self)'Length);
--   end SearchItem;

end SearchItems;
