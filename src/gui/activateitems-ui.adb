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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with Utils.UI; use Utils.UI;

package body ActivateItems.UI is

   -- ****o* ActivateItems/ActivateItems.Toggle_Execute_With_Command
   -- FUNCTION
   -- Show text entry to enter with what program execute selected file or
   -- directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleExecuteWith
   -- SOURCE
   function Toggle_Execute_With_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Execute_With_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextFrame: constant Ttk_Frame :=
        Get_Widget(".mainframe.textframe", Interp);
      Button: Ttk_Button := Get_Widget(TextFrame & ".closebutton");
      TextEntry: constant Ttk_Entry :=
        Get_Widget(TextFrame & ".textentry", Interp);
   begin
      if Winfo_Get(TextEntry, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(TextFrame & ".okbutton");
         configure(Button, "-command ExecuteWith");
         Add
           (Button,
            Mc
              (Interp,
               "{Execute the selected file or directory with the entered program.}"));
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.openwithbutton");
         State(Button, "selected");
         Add
           (TextEntry,
            Mc(Interp, "{Enter command to use to open selected item.}"));
         Unbind(TextEntry, "<KeyRelease>");
         Focus(TextEntry);
         Tcl.Tk.Ada.Grid.Grid(TextFrame, "-row 1 -columnspan 2 -sticky we");
      else
         if Invoke(Button) /= "" then
            return TCL_ERROR;
         end if;
         Button.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.openwithbutton");
         State(Button, "!selected");
      end if;
      return TCL_OK;
   end Toggle_Execute_With_Command;

   -- ****o* ActivateItems/ActivateItems.Execute_With_Command
   -- FUNCTION
   -- Execute the selected file or directory with the selected command
   -- entered by an user
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK;
   -- COMMANDS
   -- ExecuteWith
   -- SOURCE
   function Execute_With_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_With_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      TextEntry: constant Ttk_Entry :=
        Get_Widget(".mainframe.textframe.textentry", Interp);
      Value: constant String := Get(TextEntry);
      CommandName: Unbounded_String;
      Pid: GNAT.OS_Lib.Process_Id;
      SpaceIndex: Natural range 0 .. Value'Length;
      Arguments: Argument_List_Access;
   begin
      if Value'Length = 0 then
         return TCL_OK;
      end if;
      SpaceIndex := Index(Value, " ");
      CommandName :=
        (if SpaceIndex > 0 then To_Unbounded_String(Value(1 .. SpaceIndex - 1))
         else To_Unbounded_String(Value));
      CommandName :=
        To_Unbounded_String(Find_Executable(To_String(CommandName)));
      if CommandName = Null_Unbounded_String then
         Show_Message
           (Mc(Interp, "{Can't find command:}") & " " &
            Value(1 .. SpaceIndex));
         return TCL_OK;
      end if;
      Arguments :=
        (if SpaceIndex > 0 then
           Argument_String_To_List(Value(SpaceIndex .. Value'Length) & " @2")
         else Argument_String_To_List("@2"));
      Replace_Substitutes_Loop :
      for I in Arguments'Range loop
         if Arguments(I).all = "@2" then
            Arguments(I) := new String'(To_String(Current_Selected));
         end if;
      end loop Replace_Substitutes_Loop;
      Pid :=
        Non_Blocking_Spawn(Full_Name(To_String(CommandName)), Arguments.all);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         Show_Message(Mc(Interp, "{Can't execute this command}"));
         return TCL_OK;
      end if;
      return Toggle_Execute_With_Command(ClientData, Interp, Argc, Argv);
   end Execute_With_Command;

   procedure CreateActivateUI is
   begin
      Add_Command("ToggleExecuteWith", Toggle_Execute_With_Command'Access);
      Add_Command("ExecuteWith", Execute_With_Command'Access);
   end CreateActivateUI;

end ActivateItems.UI;
