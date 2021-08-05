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

with Ada.Containers;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with ActivateItems.UI;
with Inotify;
with LoadData;
with LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Modules;
with Preferences;
with RefreshData;
with ShowItems;
with Utils;
with Utils.UI; use Utils.UI;

package body ActivateItems is

   -- ****o* ActivateItems/ActivateItems.Activate_Item_Command
   -- FUNCTION
   -- "Activate" selected file or directory. Action depends on what selected
   -- item is. For example: it go to selected directory, opens text files in
   -- editor and so on.
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.a Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ActivateItem
   -- SOURCE
   function Activate_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Activate_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Containers;
      use Ada.Environment_Variables;
      use Tcl.Ada;
      use Inotify;
      use LoadData;
      use LoadData.UI;
      use Modules;
      use Preferences;
      use RefreshData;
      use ShowItems;

   begin
      if Is_Directory(Name => To_String(Source => Current_Selected)) then
         if not Is_Read_Accessible_File
             (Name => To_String(Source => Current_Selected)) then
            ShowMessage
              (Message =>
                 Mc
                   (Interp => Interp,
                    Src_String => "{You can't enter this directory.}"));
            return TCL_OK;
         end if;
         Temporary_Stop := True;
         Current_Directory := Current_Selected;
         if Settings.Show_Preview then
            Items_List := Second_Items_List;
            if Second_Items_List.Length > 0 then
               if Ada.Directories.Containing_Directory
                   (Name => To_String(Source => Second_Items_List(1).Path)) /=
                 Current_Directory then
                  LoadDirectory
                    (DirectoryName => To_String(Source => Current_Directory));
               end if;
            end if;
         else
            LoadDirectory
              (DirectoryName => To_String(Source => Current_Directory));
         end if;
         if New_Action = SHOWTRASH then
            DestinationDirectory :=
              Delete
                (Source => Current_Directory, From => 1,
                 Through =>
                   Length
                     (Source =>
                        To_Unbounded_String
                          (Source =>
                             Value(Name => "HOME") &
                             "/.local/share/Trash/files")));
         end if;
         Update_Directory_List(Clear => True);
         UpdateWatch(Path => To_String(Source => Current_Directory));
         Execute_Modules
           (State => On_Enter,
            Arguments => "{" & To_String(Source => Current_Directory) & "}");
      else
         Execute_File_Block :
         declare
            use Utils;

            Mime_Type: constant String :=
              Get_Mime_Type
                (File_Name => To_String(Source => Current_Selected));
            Pid: GNAT.OS_Lib.Process_Id;
            Openable: Boolean := Can_Be_Opened(Mime_Type => Mime_Type);
            Executable_Name: constant String :=
              Find_Executable(Name => "xdg-open");
            Arguments: Argument_List_Access;
         begin
            if Mime_Type(1 .. 4) = "text" and not Openable then
               Openable := Can_Be_Opened(Mime_Type => "text/plain");
            end if;
            if Openable then
               if Executable_Name = "" then
                  Tcl_SetResult(interp => Interp, str => "0");
                  return TCL_OK;
               end if;
               Arguments := Argument_String_To_List(Arg_String => "@2");
               Arguments(1) :=
                 new String'(To_String(Source => Current_Selected));
               Pid :=
                 Non_Blocking_Spawn
                   (Program_Name => Executable_Name, Args => Arguments.all);
            else
               if not Is_Executable_File
                   (Name => To_String(Source => Current_Selected)) then
                  ShowMessage
                    (Message =>
                       Mc
                         (Interp => Interp,
                          Src_String =>
                            "{I can't open this file. No application associated with this type of files.}"));
                  Tcl_SetResult(interp => Interp, str => "0");
                  return TCL_OK;
               end if;
               Pid :=
                 Non_Blocking_Spawn
                   (Program_Name => To_String(Source => Current_Selected),
                    Args => Argument_String_To_List(Arg_String => "").all);
               if Pid = GNAT.OS_Lib.Invalid_Pid then
                  ShowMessage
                    (Message =>
                       Mc
                         (Interp => Interp,
                          Src_String => "{I can't execute this file.}"));
                  Tcl_SetResult(interp => Interp, str => "0");
                  return TCL_OK;
               end if;
            end if;
            if Pid = GNAT.OS_Lib.Invalid_Pid then
               Tcl_SetResult(interp => Interp, str => "0");
               ShowMessage
                 (Message =>
                    Mc
                      (Interp => Interp,
                       Src_String =>
                         "{I can't open this file. Can't start application asociated with this type of files.}"));
            end if;
         end Execute_File_Block;
      end if;
      Execute_Modules
        (State => On_Activate,
         Arguments => "{" & To_String(Source => Current_Selected) & "}");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Activate_Item_Command;

   -- ****o* ActivateItems/ActivateItems.Execute_Command
   -- FUNCTION
   -- Execute the selected file or directory
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Execute
   -- SOURCE
   function Execute_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Pid: GNAT.OS_Lib.Process_Id;
   begin
      Pid :=
        Non_Blocking_Spawn
          (Program_Name => To_String(Source => Current_Selected),
           Args => Argument_String_To_List(Arg_String => "").all);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         ShowMessage
           (Message =>
              Mc
                (Interp => Interp,
                 Src_String => "{Can't execute this command}"));
      end if;
      return TCL_OK;
   end Execute_Command;

   procedure Add_Commands is
      use ActivateItems.UI;
   begin
      Add_Command
        (Name => "ActivateItem", Ada_Command => Activate_Item_Command'Access);
      Add_Command(Name => "Execute", Ada_Command => Execute_Command'Access);
      CreateActivateUI;
   end Add_Commands;

end ActivateItems;
