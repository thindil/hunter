-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****o* SearchItems/SearchItems.Toggle_Search_Command
   -- FUNCTION
   -- Show text entry to enter directory destination
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleSearch
   -- SOURCE
   function Toggle_Search_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Search_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Text_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.textframe", Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget(pathName => Text_Frame & ".closebutton");
      Text_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Text_Frame & ".textentry");
      Hunter_Search_Exception: exception;
   begin
      if Winfo_Get(Widgt => Text_Entry, Info => "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Button);
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.actiontoolbar.searchbutton");
         State(Widget => Button, StateSpec => "selected");
         Button.Name := New_String(Str => Text_Frame & ".okbutton");
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Button);
         Add
           (Widget => Text_Entry,
            Message =>
              Mc
                (Interp => Interp,
                 Src_String =>
                   "{Enter the name of the file or directory to search for}"));
         Bind
           (Widgt => Text_Entry, Sequence => "<KeyRelease>",
            Script => "{Search}");
         Focus(Widgt => Text_Entry);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Text_Frame, Options => "-row 1 -columnspan 2 -sticky we");
      else
         if Invoke(Buttn => Button) /= "" then
            raise Hunter_Search_Exception
              with Mc
                (Interp => Interp,
                 Src_String => "{Can't hide search text bar}");
         end if;
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.actiontoolbar.searchbutton");
         State(Widget => Button, StateSpec => "!selected");
         Update_Directory_List;
      end if;
      return TCL_OK;
   end Toggle_Search_Command;

   -- ****o* SearchItems/SearchItems.Search_Command
   -- FUNCTION
   -- Search current directory for the selected text (case insensitive) and
   -- show only matching files and directories
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Search
   -- SOURCE
   function Search_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => ".mainframe.textframe.textentry", Interp => Interp);
      Directory_Tree: constant Ttk_Tree_View :=
        Get_Widget
          (pathName => ".mainframe.paned.directoryframe.directorytree",
           Interp => Interp);
      Query: Unbounded_String;
      Selected: Boolean := False;
   begin
      Query := To_Unbounded_String(Source => Get(Widgt => Text_Entry));
      if Length(Source => Query) = 0 then
         Update_Directory_List;
         return TCL_OK;
      end if;
      Search_Item_Loop :
      for I in Items_List.First_Index .. Items_List.Last_Index loop
         if Index
             (Source =>
                To_Lower(Item => To_String(Source => Items_List(I).Name)),
              Pattern => To_Lower(Item => To_String(Source => Query))) =
           0 then
            Detach
              (TreeViewWidget => Directory_Tree,
               ItemsList => Positive'Image(I));
         elsif (Settings.Show_Hidden and Items_List(I).Is_Hidden) or
           not Items_List(I).Is_Hidden then
            Move
              (TreeViewWidget => Directory_Tree, Item => Positive'Image(I),
               Parent => "{}", Index => Natural'Image(I - 1));
            if not Selected then
               Selection_Set
                 (TreeViewWidget => Directory_Tree,
                  Items => Positive'Image(I));
               Selected := True;
            end if;
         end if;
      end loop Search_Item_Loop;
      return TCL_OK;
   end Search_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ToggleSearch", Ada_Command => Toggle_Search_Command'Access);
      Add_Command(Name => "Search", Ada_Command => Search_Command'Access);
   end Add_Commands;

end SearchItems;
