-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Directories;
with Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings;
with GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tklib.Ada.Tooltip;
with LoadData;
with LoadData.UI;
with Common; use Common;
with MainWindow;
with Preferences;
with RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body CreateItems.UI is

   -- ****o* CreateItemsUI/CreateItemsUI.Show_Create_Command
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
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tklib.Ada.Tooltip;

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
         Show_Destination;
      end if;
      Toggle_Tool_Buttons(Action => New_Action);
      return TCL_OK;
   end Show_Create_Command;

   -- ****o* CreateItemsUI/CreateItemsUI.Create_Command
   -- FUNCTION
   -- Show text entry to enter a name of the new item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Create
   -- SOURCE
   function Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Create_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use GNAT.OS_Lib;
      use Tcl.Ada;
      use Tcl.Tk.Ada.Widgets.TtkTreeView;
      use LoadData;
      use LoadData.UI;
      use MainWindow;
      use Preferences;
      use RefreshData;

      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => ".mainframe.textframe.textentry", Interp => Interp);
      New_Item_Name: Unbounded_String;
      Destination: Unbounded_String := Null_Unbounded_String;
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
        Common.Current_Directory & "/" & Get(Widgt => Text_Entry);
      if not Is_Creating_Possible
          (New_Item_Name => To_String(Source => New_Item_Name),
           Interp => Interp) then
         goto End_Of_Create;
      end if;
      case New_Action is
         when CREATEDIRECTORY =>
            Create_Path(New_Directory => To_String(Source => New_Item_Name));
         when CREATEFILE =>
            Create_Path
              (New_Directory =>
                 Containing_Directory
                   (Name => To_String(Source => New_Item_Name)));
            File :=
              Create_File
                (Name => To_String(Source => New_Item_Name), Fmode => Binary);
            Close(FD => File);
         when CREATELINK =>
            Destination := Destination_Directory;
            if Selection(TreeViewWidget => Directory_View)'Length > 0 then
               Destination :=
                 Destination_Directory &
                 Second_Items_List
                   (Positive'Value
                      (Selection(TreeViewWidget => Directory_View)))
                   .Name;
            end if;
            Tcl_Eval
              (interp => Interp,
               strng =>
                 "file link -symbolic {" & To_String(Source => New_Item_Name) &
                 "} {" & To_String(Source => Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interp => Interp, Src_String => "{Invalid action type}");
      end case;
      if not Settings.Stay_In_Old and then New_Action /= CREATELINK then
         Common.Current_Directory :=
           To_Unbounded_String
             (Source =>
                Containing_Directory
                  (Name => To_String(Source => New_Item_Name)));
      end if;
      Load_Directory
        (Directory_Name => To_String(Source => Common.Current_Directory));
      Update_Watch(Path => To_String(Source => Common.Current_Directory));
      Update_Directory_List(Clear => True);
      <<End_Of_Create>>
      if Invoke(Buttn => Button) /= "" then
         return TCL_ERROR;
      end if;
      Toggle_Tool_Buttons(Action => New_Action, Finished => True);
      return TCL_OK;
   end Create_Command;

   procedure Add_Commands is
      use Utils;

   begin
      Add_Command
        (Name => "ShowCreate", Ada_Command => Show_Create_Command'Access);
      Add_Command(Name => "Create", Ada_Command => Create_Command'Access);
   end Add_Commands;

end CreateItems.UI;
