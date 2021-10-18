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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv; use CArgv;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with Messages.UI; use Messages.UI;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Modules.Commands is

   -- ****o* MCommands2/MCommands2.Toggle_Module_Command
   -- FUNCTION
   -- Enable or disable the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleModule path
   -- Path is the path to the module which will be enabled or disabled
   -- SOURCE
   function Toggle_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ModulePath: constant Unbounded_String :=
        To_Unbounded_String(CArgv.Arg(Argv, 1));
   begin
      if Enabled_Modules.Contains(ModulePath) then
         Enabled_Modules.Delete(Enabled_Modules.Find_Index(ModulePath));
         Tcl_Eval(Interp, Simple_Name(To_String(ModulePath)) & "::on_disable");
         Tcl_Eval
           (Interp, "namespace delete " & Simple_Name(To_String(ModulePath)));
      else
         Enabled_Modules.Append(ModulePath);
         Tcl_EvalFile(Interp, To_String(ModulePath) & "/module.tcl");
         Tcl_Eval
           (Interp,
            Simple_Name(To_String(ModulePath)) & "::on_enable {" &
            To_String(ModulePath) & "}");
      end if;
      return TCL_OK;
   exception
      when Tcl_Error_Exception =>
         return TCL_ERROR;
   end Toggle_Module_Command;

   -- ****o* MCommands2/MCommands2.Get_Config_Command
   -- FUNCTION
   -- Get the value of the selected configuration option of the program
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetConfig optionname
   -- Optionname is the name of the program's option which value will be get
   -- SOURCE
   function Get_Config_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_Config_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      procedure GetBoolean(Value: Boolean) is
      begin
         if Value then
            Tcl_SetResult(Interp, "1");
         else
            Tcl_SetResult(Interp, "0");
         end if;
      end GetBoolean;
   begin
      if Argc /= 2 then
         Tcl_AddErrorInfo
           (Interp,
            New_String("Command GetConfig must have exactly one parameter"));
         return TCL_ERROR;
      end if;
      if CArgv.Arg(Argv, 1) = "showhidden" then
         GetBoolean(Settings.Show_Hidden);
      elsif CArgv.Arg(Argv, 1) = "showlastmodified" then
         GetBoolean(Settings.Show_Last_Modified);
      elsif CArgv.Arg(Argv, 1) = "scaleimages" then
         GetBoolean(Settings.Scale_Images);
      elsif CArgv.Arg(Argv, 1) = "autoclosemessagestime" then
         Tcl_SetResult
           (Interp,
            Trim(Natural'Image(Settings.Auto_Close_Messages_Time), Left));
      elsif CArgv.Arg(Argv, 1) = "windowwidth" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.Window_Width), Left));
      elsif CArgv.Arg(Argv, 1) = "windowheight" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.Window_Height), Left));
      elsif CArgv.Arg(Argv, 1) = "showpreview" then
         GetBoolean(Settings.Show_Preview);
      elsif CArgv.Arg(Argv, 1) = "stayinold" then
         GetBoolean(Settings.Stay_In_Old);
      elsif CArgv.Arg(Argv, 1) = "colortext" then
         GetBoolean(Settings.Color_Text);
      elsif CArgv.Arg(Argv, 1) = "colortheme" then
         Tcl_SetResult(Interp, To_String(Settings.Color_Theme));
      elsif CArgv.Arg(Argv, 1) = "deletefiles" then
         GetBoolean(Settings.Delete_Files);
      elsif CArgv.Arg(Argv, 1) = "cleartrashonexit" then
         GetBoolean(Settings.Clear_Trash_On_Exit);
      elsif CArgv.Arg(Argv, 1) = "showfinishedinfo" then
         GetBoolean(Settings.Show_Finished_Info);
      elsif CArgv.Arg(Argv, 1) = "overwriteonexist" then
         GetBoolean(Settings.Overwrite_On_Exist);
      elsif CArgv.Arg(Argv, 1) = "toolbarsontop" then
         GetBoolean(Settings.Toolbars_On_Top);
      elsif CArgv.Arg(Argv, 1) = "autorefreshinterval" then
         Tcl_SetResult
           (Interp, Trim(Natural'Image(Settings.Auto_Refresh_Interval), Left));
      elsif CArgv.Arg(Argv, 1) = "uitheme" then
         Tcl_SetResult(Interp, To_String(Settings.Ui_Theme));
      elsif CArgv.Arg(Argv, 1) = "toolbarssize" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.Toolbars_Size), Left));
      elsif CArgv.Arg(Argv, 1) = "monospacefont" then
         GetBoolean(Settings.Monospace_Font);
      else
         Tcl_AddErrorInfo
           (Interp,
            New_String
              ("Parameter for GetConfig must be one of: showhidden, showlastmodified, scaleimages, autoclosemessagestime, windowwidth, windowheight, showpreview, stayinold, colortext, colortheme, deletefiles, cleartrashonexit, showfinishedinfo, overwriteonexits, toolsbarsontop, autorefreshinterval, uitheme, toolbarssize, monospacefont"));
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Get_Config_Command;

   -- ****o* MCommands2/MCommands2.Execute_Module_Command_Command
   -- FUNCTION
   -- Execute the selected module command and show its output if needed
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteCommand command ?path?
   -- Command is the command with attributes to execute. Path is the working
   -- directory in which the command should be executed
   -- SOURCE
   function Execute_Module_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Module_Command_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Value, CommandName: Unbounded_String;
      SpaceIndex: Natural;
      Result: Expect_Match;
      ProcessDesc: Process_Descriptor;
      Arguments: Argument_List_Access;
      Success: Boolean := False;
      CurrentDir: constant String := Ada.Directories.Current_Directory;
   begin
      if Argc = 3 then
         Set_Directory(CArgv.Arg(Argv, 2));
      end if;
      Value := To_Unbounded_String(CArgv.Arg(Argv, 1));
      SpaceIndex := Index(Value, " ");
      CommandName :=
        (if SpaceIndex > 0 then Unbounded_Slice(Value, 1, SpaceIndex - 1)
         else Value);
      CommandName :=
        To_Unbounded_String(Find_Executable(To_String(CommandName)));
      if CommandName = Null_Unbounded_String then
         Show_Message
           (Mc(Interp, "{Can't find command:}") & " " &
            Slice(Value, 1, SpaceIndex));
         if Argc = 3 then
            Set_Directory(CurrentDir);
         end if;
         return TCL_OK;
      end if;
      if SpaceIndex > 0 then
         Arguments :=
           Argument_String_To_List(Slice(Value, SpaceIndex, Length(Value)));
      end if;
      Replace_Arguments_Loop :
      for I in Arguments'Range loop
         if Arguments(I).all = "@1" then
            Arguments(I) :=
              new String'(To_String(Common.Current_Directory));
         elsif Arguments(I).all = "@2" then
            Arguments(I) := new String'(To_String(Current_Selected));
         end if;
      end loop Replace_Arguments_Loop;
      Non_Blocking_Spawn
        (ProcessDesc, Full_Name(To_String(CommandName)), Arguments.all);
      ShowOutput;
      Show_Output_Loop :
      loop
         Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 300_000);
         exit Show_Output_Loop when Result /= 1;
         UpdateOutput(Expect_Out_Match(ProcessDesc) & LF);
         Success := True;
      end loop Show_Output_Loop;
      Close(ProcessDesc);
      if Argc = 3 then
         Set_Directory(CurrentDir);
      end if;
      return TCL_OK;
   exception
      when Process_Died =>
         if not Success then
            Show_Message
              (Mc(Interp, "{Can't execute command:}") & " " &
               Slice(Value, 1, SpaceIndex));
         end if;
         if Argc = 3 then
            Set_Directory(CurrentDir);
         end if;
         return TCL_OK;
   end Execute_Module_Command_Command;

   procedure AddCommands is
   begin
      Add_Command("ToggleModule", Toggle_Module_Command'Access);
      Add_Command("GetConfig", Get_Config_Command'Access);
      Add_Command
        ("ExecuteModuleCommand", Execute_Module_Command_Command'Access);
   end AddCommands;

end Modules.Commands;
