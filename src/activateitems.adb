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

with Ada.Containers; use Ada.Containers;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with ActivateItems.UI; use ActivateItems.UI;
with Inotify; use Inotify;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Modules; use Modules;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body ActivateItems is

   -- ****o* ActivateItems/ActivateItems.Activate_Item_Command
   -- FUNCTION
   -- "Activate" selected file or directory. Action depends on what selected
   -- item is. For example: it go to selected directory, opens text files in
   -- editor and so on.
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.a Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ActivateItem
   -- SOURCE
   function Activate_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Activate_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Is_Directory(To_String(Current_Selected)) then
         if not Is_Read_Accessible_File(To_String(Current_Selected)) then
            ShowMessage(Mc(Interp, "{You can't enter this directory.}"));
            return TCL_OK;
         end if;
         Temporary_Stop := True;
         Current_Directory := Current_Selected;
         if Settings.Show_Preview then
            ItemsList := SecondItemsList;
            if SecondItemsList.Length > 0 then
               if Ada.Directories.Containing_Directory
                   (To_String(SecondItemsList(1).Path)) /=
                 Current_Directory then
                  LoadDirectory(To_String(Current_Directory));
               end if;
            end if;
         else
            LoadDirectory(To_String(Current_Directory));
         end if;
         if New_Action = SHOWTRASH then
            DestinationDirectory :=
              Delete
                (Current_Directory, 1,
                 Length
                   (To_Unbounded_String
                      (Value("HOME") & "/.local/share/Trash/files")));
         end if;
         Update_Directory_List(True);
         UpdateWatch(To_String(Current_Directory));
         Execute_Modules(On_Enter, "{" & To_String(Current_Directory) & "}");
      else
         declare
            MimeType: constant String :=
              Get_Mime_Type(To_String(Current_Selected));
            Pid: GNAT.OS_Lib.Process_Id;
            Openable: Boolean := Can_Be_Opened(MimeType);
            ExecutableName: constant String := Find_Executable("xdg-open");
            Arguments: Argument_List_Access;
         begin
            if MimeType(1 .. 4) = "text" and not Openable then
               Openable := Can_Be_Opened("text/plain");
            end if;
            if not Openable then
               if not Is_Executable_File(To_String(Current_Selected)) then
                  ShowMessage
                    (Mc
                       (Interp,
                        "{I can't open this file. No application associated with this type of files.}"));
                  Tcl_SetResult(Interp, "0");
                  return TCL_OK;
               end if;
               Pid :=
                 Non_Blocking_Spawn
                   (To_String(Current_Selected),
                    Argument_String_To_List("").all);
               if Pid = GNAT.OS_Lib.Invalid_Pid then
                  ShowMessage(Mc(Interp, "{I can't execute this file.}"));
                  Tcl_SetResult(Interp, "0");
                  return TCL_OK;
               end if;
            else
               if ExecutableName = "" then
                  Tcl_SetResult(Interp, "0");
                  return TCL_OK;
               end if;
               Arguments := Argument_String_To_List("@2");
               Arguments(1) := new String'(To_String(Current_Selected));
               Pid := Non_Blocking_Spawn(ExecutableName, Arguments.all);
            end if;
            if Pid = GNAT.OS_Lib.Invalid_Pid then
               Tcl_SetResult(Interp, "0");
               ShowMessage
                 (Mc
                    (Interp,
                     "{I can't open this file. Can't start application asociated with this type of files.}"));
            end if;
         end;
      end if;
      Execute_Modules(On_Activate, "{" & To_String(Current_Selected) & "}");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Activate_Item_Command;

   -- ****o* ActivateItems/ActivateItems.Execute_Command
   -- FUNCTION
   -- Execute the selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Execute
   -- SOURCE
   function Execute_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Pid: GNAT.OS_Lib.Process_Id;
   begin
      Pid :=
        Non_Blocking_Spawn
          (To_String(Current_Selected), Argument_String_To_List("").all);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         ShowMessage(Mc(Interp, "{Can't execute this command}"));
      end if;
      return TCL_OK;
   end Execute_Command;

   procedure Add_Commands is
   begin
      Add_Command("ActivateItem", Activate_Item_Command'Access);
      Add_Command("Execute", Execute_Command'Access);
      CreateActivateUI;
   end Add_Commands;

end ActivateItems;
