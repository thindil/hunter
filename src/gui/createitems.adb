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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body CreateItems is

   -- ****o* CreateItems/CreateItems.Show_Create_Command
   -- FUNCTION
   -- Show text entry to enter a name of the new item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCreate itemtype
   -- Itemtype is an item type which will be created. Can be file or directory
   -- SOURCE
   function Show_Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Frame: constant Ttk_Frame := Get_Widget(".mainframe.textframe", Interp);
      Button: Ttk_Button := Get_Widget(Frame & ".closebutton", Interp);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget(Frame & ".textentry", Interp);
   begin
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button.Name := New_String(Frame & ".okbutton");
      configure(Button, "-command {Create " & CArgv.Arg(Argv, 1) & "}");
      Add
        (Button,
         Mc(Interp, "{Create a new}") & " " & Mc(Interp, CArgv.Arg(Argv, 1)) &
         " " & Mc(Interp, "{with the selected name.}"));
      Add
        (Text_Entry,
         Mc(Interp, "{Enter a name for the newly created}") & " " &
         Mc(Interp, CArgv.Arg(Argv, 1)) & ".");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Unbind(Text_Entry, "<KeyRelease>");
      Focus(Text_Entry);
      Tcl.Tk.Ada.Grid.Grid(Frame, "-row 1 -columnspan 2 -sticky we");
      if CArgv.Arg(Argv, 1) = "file" then
         New_Action := CREATEFILE;
      elsif CArgv.Arg(Argv, 1) = "directory" then
         New_Action := CREATEDIRECTORY;
      else
         New_Action := CREATELINK;
         ShowDestination;
      end if;
      Toggle_Tool_Buttons(New_Action);
      return TCL_OK;
   end Show_Create_Command;

   -- ****o* CreateItems/CreateItems.Create_Command
   -- FUNCTION
   -- Show text entry to enter a name of the new item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Create itemtype
   -- Itemtype is an item type which will be created. Can be file or directory
   -- SOURCE
   function Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
      NewItemName, ActionString, ActionBlocker, Destination: Unbounded_String;
      Button: constant Ttk_Button :=
        Get_Widget(".mainframe.textframe.closebutton", Interp);
      File: File_Descriptor;
      DirectoryView: constant Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.previewframe.directorytree", Interp);
      Hunter_Create_Exception: exception;
   begin
      NewItemName := MainWindow.Current_Directory & "/" & Get(Text_Entry);
      if Exists(To_String(NewItemName)) or
        Is_Symbolic_Link(To_String(NewItemName)) then
         ActionString :=
           To_Unbounded_String
             (Mc(Interp, "{create}") & " " & CArgv.Arg(Argv, 1) & " " &
              Mc(Interp, "{with}"));
         ActionBlocker :=
           (if Is_Directory(To_String(NewItemName)) then
              To_Unbounded_String(Mc(Interp, "directory"))
            else To_Unbounded_String(Mc(Interp, "file")));
         ShowMessage
           (Mc(Interp, "{You can't}") & " " & To_String(ActionString) & " " &
            Mc(Interp, "{name}") & " '" & To_String(NewItemName) & "' " &
            Mc(Interp, "{because there exists}") & " " &
            To_String(ActionBlocker) & " " & Mc(Interp, "{with that name.}"));
         goto End_Of_Create;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewItemName))) then
         ShowMessage
           (Mc(Interp, "{You don't have permissions to write to}") & " " &
            Containing_Directory(To_String(NewItemName)));
         goto End_Of_Create;
      end if;
      case New_Action is
         when CREATEDIRECTORY =>
            Create_Path(To_String(NewItemName));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(NewItemName)));
            File := Create_File(To_String(NewItemName), Binary);
            Close(File);
         when CREATELINK =>
            Destination := DestinationDirectory;
            if Selection(DirectoryView)'Length > 0 then
               Destination :=
                 DestinationDirectory &
                 SecondItemsList(Positive'Value(Selection(DirectoryView)))
                   .Name;
            end if;
            Tcl_Eval
              (Interp,
               "file link -symbolic {" & To_String(NewItemName) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interp, "{Invalid action type}");
      end case;
      if not Settings.Stay_In_Old and then New_Action /= CREATELINK then
         MainWindow.Current_Directory :=
           To_Unbounded_String(Containing_Directory(To_String(NewItemName)));
      end if;
      LoadDirectory(To_String(MainWindow.Current_Directory));
      UpdateWatch(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      <<End_Of_Create>>
      if Invoke(Button) /= "" then
         return TCL_ERROR;
      end if;
      Toggle_Tool_Buttons(New_Action, True);
      return TCL_OK;
   end Create_Command;

   procedure Create_Create_Ui is
   begin
      Add_Command("ShowCreate", Show_Create_Command'Access);
      Add_Command("Create", Create_Command'Access);
   end Create_Create_Ui;

end CreateItems;
