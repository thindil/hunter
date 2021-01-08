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

with Ada.Strings;
with Tcl.Ada;
with Utils.UI; use Utils.UI;

package body ShowItems is

   procedure ShowPreview is
   begin
      null;
   end ShowPreview;

   -- ****if* ShowItemsTUI/ShowItemsTUI.ShowInfo
   -- FUNCTION
   -- Show information about the currently selected file or directory.
   -- SOURCE
   procedure ShowInfo is
      -- ****
   begin
      null;
   end ShowInfo;

   -- ****o* ShowItemsTUI/ShowItemsTUI.Show_Preview_Or_Info_Command
   -- FUNCTION
   -- Show preview or information about the currently selected file or
   -- directory, depends which button was clicked
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowPreviewOrInfo
   -- SOURCE
   function Show_Preview_Or_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Preview_Or_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl.Ada.Tcl_GetVar(Interp, "previewtype") = "preview" then
         ShowPreview;
      else
         ShowInfo;
      end if;
      return TCL_OK;
   end Show_Preview_Or_Info_Command;

   function Show_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Show_Selected_Command;

   -- ****o* ShowItemsTUI/ShowItemsTUI.Set_Permissions_Command
   -- FUNCTION
   -- Set the permissions for the selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetPermissions
   -- SOURCE
   function Set_Permissions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Permissions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Set_Permissions_Command;

   -- ****o* ShowItemsTUI/ShowItemsTUI.GoToDirectory_Command
   -- FUNCTION
   -- Go to the selected directory in preview
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToDirectory ?selecteditem?
   -- Selecteditem is full path to the currently selected file or directory
   -- SOURCE
   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end GoToDirectory_Command;

   procedure CreateShowItemsUI is
   begin
      AddCommand("ShowSelected", Show_Selected_Command'Access);
      AddCommand("ShowPreviewOrInfo", Show_Preview_Or_Info_Command'Access);
      AddCommand("SetPermissions", Set_Permissions_Command'Access);
      AddCommand("GoToDirectory", GoToDirectory_Command'Access);
   end CreateShowItemsUI;

   procedure ShowDestination is
   begin
      null;
   end ShowDestination;

   procedure ShowOutput is
   begin
      null;
   end ShowOutput;

   procedure UpdateOutput(Text: String) is
   begin
      null;
   end UpdateOutput;

end ShowItems;
