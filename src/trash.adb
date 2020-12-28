-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Modules; use Modules;
with ShowItems; use ShowItems;
with Trash.UI; use Trash.UI;
with Utils.UI; use Utils.UI;

package body Trash is

   -- ****o* Trash/Trash.Restore_Item_Command
   -- FUNCTION
   -- Restore the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RestoreItem
   -- SOURCE
   function Restore_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      RestoreInfo, FileLine, Destination, ItemType: Unbounded_String;
      StartIndex: Positive;
      FileInfo: File_Type;
   begin
      for Item of SelectedItems loop
         StartIndex := Index(Item, "files");
         RestoreInfo :=
           Unbounded_Slice(Item, 1, StartIndex - 1) & "info" &
           Unbounded_Slice(Item, StartIndex + 5, Length(Item)) &
           To_Unbounded_String(".trashinfo");
         Open(FileInfo, In_File, To_String(RestoreInfo));
         Skip_Line(FileInfo);
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Destination := Unbounded_Slice(FileLine, 6, Length(FileLine));
               if Ada.Directories.Exists(To_String(Destination)) then
                  ItemType :=
                    (if Is_Directory(To_String(Destination)) then
                       To_Unbounded_String(Mc(Interp, "{Directory}"))
                     else To_Unbounded_String(Mc(Interp, "{File}")));
                  ShowMessage
                    (Mc(Interp, "{Can't restore}") & " " &
                     To_String(Destination) & " " & To_String(ItemType) & " " &
                     Mc(Interp, "{with that name exists.}"));
                  Close(FileInfo);
                  return Show_Trash_Command(ClientData, Interp, Argc, Argv);
               end if;
               Rename(To_String(Item), Slice(FileLine, 6, Length(FileLine)));
            end if;
         end loop;
         Close(FileInfo);
         Delete_File(To_String(RestoreInfo));
      end loop;
      return Show_Trash_Command(ClientData, Interp, Argc, Argv);
   end Restore_Item_Command;

   -- ****o* Trash/Trash.Clear_Trash_Command
   -- FUNCTION
   -- Remove everything from the Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ClearTrash
   -- SOURCE
   function Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      NewAction := CLEARTRASH;
      ToggleToolButtons(NewAction);
      ShowMessage
        (Mc(Interp, "{Remove all files and directories from Trash?}"),
         "question");
      return TCL_OK;
   end Clear_Trash_Command;

   -- ****o* Trash/Trash.GoToTrash_Command
   -- FUNCTION
   -- Go to the selected directory in Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToTrash path
   -- Path is the full path to the directory which will be set as current
   -- directory (and show to the user)
   -- SOURCE
   function GoToTrash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToTrash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      CurrentDirectory :=
        To_Unbounded_String(Normalize_Pathname(CArgv.Arg(Argv, 1)));
      DestinationDirectory :=
        Delete
          (CurrentDirectory, 1,
           Length
             (To_Unbounded_String
                (Value("HOME") & "/.local/share/Trash/files")));
      LoadDirectory(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      Execute_Modules(On_Enter, "{" & To_String(CurrentDirectory) & "}");
      return TCL_OK;
   end GoToTrash_Command;

   procedure CreateTrash is
   begin
      AddCommand("RestoreItems", Restore_Item_Command'Access);
      AddCommand("ClearTrash", Clear_Trash_Command'Access);
      AddCommand("GoToTrash", GoToTrash_Command'Access);
      CreateTrashUI;
   end CreateTrash;

end Trash;
