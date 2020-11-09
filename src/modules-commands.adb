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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Preferences; use Preferences;
with Utils; use Utils;

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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ModulePath: constant Unbounded_String :=
        To_Unbounded_String
          (Normalize_Pathname
             (CArgv.Arg(Argv, 1), Containing_Directory(Command_Name)));
   begin
      if Enabled_Modules.Contains(ModulePath) then
         Enabled_Modules.Delete(Enabled_Modules.Find_Index(ModulePath));
         begin
            Tcl_Eval
              (Interp, Simple_Name(To_String(ModulePath)) & "::on_disable");
         exception
            when Tcl_Error_Exception =>
               null;
         end;
         Tcl_Eval
           (Interp, "namespace delete " & Simple_Name(To_String(ModulePath)));
      else
         Enabled_Modules.Append(ModulePath);
         Tcl_EvalFile(Interp, To_String(ModulePath) & "/module.tcl");
         begin
            Tcl_Eval
              (Interp, Simple_Name(To_String(ModulePath)) & "::on_enable");
         exception
            when Tcl_Error_Exception =>
               null;
         end;
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_Config_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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
         GetBoolean(Settings.ShowHidden);
      elsif CArgv.Arg(Argv, 1) = "showlastmodified" then
         GetBoolean(Settings.ShowLastModified);
      elsif CArgv.Arg(Argv, 1) = "scaleimages" then
         GetBoolean(Settings.ScaleImages);
      elsif CArgv.Arg(Argv, 1) = "autoclosemessagestime" then
         Tcl_SetResult
           (Interp, Trim(Natural'Image(Settings.AutoCloseMessagesTime), Left));
      elsif CArgv.Arg(Argv, 1) = "windowwidth" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.WindowWidth), Left));
      elsif CArgv.Arg(Argv, 1) = "windowheight" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.WindowHeight), Left));
      elsif CArgv.Arg(Argv, 1) = "showpreview" then
         GetBoolean(Settings.ShowPreview);
      elsif CArgv.Arg(Argv, 1) = "stayinold" then
         GetBoolean(Settings.StayInOld);
      elsif CArgv.Arg(Argv, 1) = "colortext" then
         GetBoolean(Settings.ColorText);
      elsif CArgv.Arg(Argv, 1) = "colortheme" then
         Tcl_SetResult(Interp, To_String(Settings.ColorTheme));
      elsif CArgv.Arg(Argv, 1) = "deletefiles" then
         GetBoolean(Settings.DeleteFiles);
      elsif CArgv.Arg(Argv, 1) = "cleartrashonexit" then
         GetBoolean(Settings.ClearTrashOnExit);
      elsif CArgv.Arg(Argv, 1) = "showfinishedinfo" then
         GetBoolean(Settings.ShowFinishedInfo);
      elsif CArgv.Arg(Argv, 1) = "overwriteonexist" then
         GetBoolean(Settings.OverwriteOnExist);
      elsif CArgv.Arg(Argv, 1) = "toolbarsontop" then
         GetBoolean(Settings.ToolbarsOnTop);
      elsif CArgv.Arg(Argv, 1) = "autorefreshinterval" then
         Tcl_SetResult
           (Interp, Trim(Natural'Image(Settings.AutoRefreshInterval), Left));
      elsif CArgv.Arg(Argv, 1) = "uitheme" then
         Tcl_SetResult(Interp, To_String(Settings.UITheme));
      elsif CArgv.Arg(Argv, 1) = "toolbarssize" then
         Tcl_SetResult
           (Interp, Trim(Positive'Image(Settings.ToolbarsSize), Left));
      elsif CArgv.Arg(Argv, 1) = "monospacefont" then
         GetBoolean(Settings.MonospaceFont);
      else
         Tcl_AddErrorInfo
           (Interp,
            New_String
              ("Parameter for GetConfig must be one of: showhidden, showlastmodified, scaleimages, autoclosemessagestime, windowwidth, windowheight, showpreview, stayinold, colortext, colortheme, deletefiles, cleartrashonexit, showfinishedinfo, overwriteonexits, toolsbarsontop, autorefreshinterval, uitheme, toolbarssize, monospacefont"));
         return TCL_ERROR;
      end if;
      return TCL_OK;
   end Get_Config_Command;

   procedure AddCommands is
   begin
      AddCommand("ToggleModule", Toggle_Module_Command'Access);
      AddCommand("GetConfig", Get_Config_Command'Access);
   end AddCommands;

end Modules.Commands;
