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
      Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.textframe", Interp => Interp);
      Button: Ttk_Button :=
        Get_Widget(pathName => Frame & ".closebutton", Interp => Interp);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Frame & ".textentry", Interp => Interp);
   begin
      Tcl.Tk.Ada.Grid.Grid(Slave => Button);
      Button.Name := New_String(Str => Frame & ".okbutton");
      configure
        (Widgt => Button,
         options =>
           "-command {Create " & CArgv.Arg(Argv => Argv, N => 1) & "}");
      Add
        (Widget => Button,
         Message =>
           Mc(Interp => Interp, Src_String => "{Create a new}") & " " &
           Mc(Interp => Interp,
              Src_String => CArgv.Arg(Argv => Argv, N => 1)) &
           " " &
           Mc(Interp => Interp, Src_String => "{with the selected name.}"));
      Add
        (Widget => Text_Entry,
         Message =>
           Mc
             (Interp => Interp,
              Src_String => "{Enter a name for the newly created}") &
           " " &
           Mc(Interp => Interp,
              Src_String => CArgv.Arg(Argv => Argv, N => 1)) &
           ".");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button);
      Unbind(Widgt => Text_Entry, Sequence => "<KeyRelease>");
      Focus(Widgt => Text_Entry);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Frame, Options => "-row 1 -columnspan 2 -sticky we");
      if CArgv.Arg(Argv => Argv, N => 1) = "file" then
         New_Action := CREATEFILE;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "directory" then
         New_Action := CREATEDIRECTORY;
      else
         New_Action := CREATELINK;
         ShowDestination;
      end if;
      Toggle_Tool_Buttons(Action => New_Action);
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
        Get_Widget
          (pathName => ".mainframe.textframe.textentry", Interp => Interp);
      New_Item_Name: Unbounded_String;
      Action_String, Action_Blocker, Destination: Unbounded_String :=
        Null_Unbounded_String;
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName => ".mainframe.textframe.closebutton", Interp => Interp);
      File: File_Descriptor := Null_FD;
      Directory_View: constant Ttk_Tree_View :=
        Get_Widget
          (pathName => ".mainframe.paned.previewframe.directorytree",
           Interp => Interp);
      Hunter_Create_Exception: exception;
   begin
      New_Item_Name :=
        MainWindow.Current_Directory & "/" & Get(Widgt => Text_Entry);
      if Exists(Name => To_String(Source => New_Item_Name)) or
        Is_Symbolic_Link(Name => To_String(Source => New_Item_Name)) then
         Action_String :=
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interp, Src_String => "{create}") & " " &
                CArgv.Arg(Argv => Argv, N => 1) & " " &
                Mc(Interp => Interp, Src_String => "{with}"));
         Action_Blocker :=
           (if Is_Directory(Name => To_String(Source => New_Item_Name)) then
              To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "directory"))
            else To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "file")));
         ShowMessage
           (Mc(Interp, "{You can't}") & " " & To_String(Action_String) & " " &
            Mc(Interp, "{name}") & " '" & To_String(New_Item_Name) & "' " &
            Mc(Interp, "{because there exists}") & " " &
            To_String(Action_Blocker) & " " & Mc(Interp, "{with that name.}"));
         goto End_Of_Create;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(New_Item_Name))) then
         ShowMessage
           (Mc(Interp, "{You don't have permissions to write to}") & " " &
            Containing_Directory(To_String(New_Item_Name)));
         goto End_Of_Create;
      end if;
      case New_Action is
         when CREATEDIRECTORY =>
            Create_Path(To_String(New_Item_Name));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(New_Item_Name)));
            File := Create_File(To_String(New_Item_Name), Binary);
            Close(File);
         when CREATELINK =>
            Destination := DestinationDirectory;
            if Selection(Directory_View)'Length > 0 then
               Destination :=
                 DestinationDirectory &
                 SecondItemsList(Positive'Value(Selection(Directory_View)))
                   .Name;
            end if;
            Tcl_Eval
              (Interp,
               "file link -symbolic {" & To_String(New_Item_Name) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interp, "{Invalid action type}");
      end case;
      if not Settings.Stay_In_Old and then New_Action /= CREATELINK then
         MainWindow.Current_Directory :=
           To_Unbounded_String(Containing_Directory(To_String(New_Item_Name)));
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
