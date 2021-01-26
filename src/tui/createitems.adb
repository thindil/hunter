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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body CreateItems is

   -- ****o* CreateItemsTUI/CreateItemsTUI.Create_Item_Command
   -- FUNCTION
   -- Create the new item of the selected name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CreateItem itemname
   -- ItemName is the name of item to create
   -- SOURCE
   function Create_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Create_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      NewItemName, ActionString, ActionBlocker, Destination: Unbounded_String;
      File: File_Descriptor;
      Hunter_Create_Exception: exception;
   begin
      NewItemName := CurrentDirectory & "/" & CArgv.Arg(Argv, 1);
      if Exists(To_String(NewItemName)) or
        Is_Symbolic_Link(To_String(NewItemName)) then
        if NewAction = CREATEFILE then
         ActionString :=
           To_Unbounded_String
             (Mc(Interp, "{create}") & " file " &
              Mc(Interp, "{with}"));
        elsif NewAction = CREATEDIRECTORY then
         ActionString :=
           To_Unbounded_String
             (Mc(Interp, "{create}") & " directory " &
              Mc(Interp, "{with}"));
        else
         ActionString :=
           To_Unbounded_String
             (Mc(Interp, "{create}") & " link " &
              Mc(Interp, "{with}"));
        end if;
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
      case NewAction is
         when CREATEDIRECTORY =>
            Create_Path(To_String(NewItemName));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(NewItemName)));
            File := Create_File(To_String(NewItemName), Binary);
            Close(File);
         when CREATELINK =>
            Destination := DestinationDirectory;
--            if Selection(DirectoryView)'Length > 0 then
--               Destination :=
--                 DestinationDirectory &
--                 SecondItemsList(Positive'Value(Selection(DirectoryView)))
--                   .Name;
--            end if;
            Tcl_Eval
              (Interp,
               "file link -symbolic {" & To_String(NewItemName) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interp, "{Invalid action type}");
      end case;
      if not Settings.StayInOld and then NewAction /= CREATELINK then
         CurrentDirectory :=
           To_Unbounded_String(Containing_Directory(To_String(NewItemName)));
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateWatch(To_String(CurrentDirectory));
      <<End_Of_Create>>
      return TCL_OK;
   end Create_Item_Command;

   procedure AddCommands is
   begin
      AddCommand("CreateItem", Create_Item_Command'Access);
   end AddCommands;

end CreateItems;
