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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      RestoreInfo, FileLine, Destination, ItemType: Unbounded_String;
      StartIndex: Positive;
      FileInfo: File_Type;
   begin
      Restore_Items_Loop :
      for Item of Selected_Items loop
         StartIndex := Index(Item, "files");
         RestoreInfo :=
           Unbounded_Slice(Item, 1, StartIndex - 1) & "info" &
           Unbounded_Slice(Item, StartIndex + 5, Length(Item)) &
           To_Unbounded_String(".trashinfo");
         Open(FileInfo, In_File, To_String(RestoreInfo));
         Skip_Line(FileInfo);
         Restore_Item_Loop :
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Destination := Unbounded_Slice(FileLine, 6, Length(FileLine));
               if Ada.Directories.Exists(To_String(Destination)) then
                  ItemType :=
                    (if Is_Directory(To_String(Destination)) then
                       To_Unbounded_String(Mc(Interp, "{Directory}"))
                     else To_Unbounded_String(Mc(Interp, "{File}")));
                  Show_Message
                    (Mc(Interp, "{Can't restore}") & " " &
                     To_String(Destination) & " " & To_String(ItemType) & " " &
                     Mc(Interp, "{with that name exists.}"));
                  Close(FileInfo);
                  return Show_Trash_Command(ClientData, Interp, Argc, Argv);
               end if;
               Rename(To_String(Item), Slice(FileLine, 6, Length(FileLine)));
            end if;
         end loop Restore_Item_Loop;
         Close(FileInfo);
         Delete_File(To_String(RestoreInfo));
      end loop Restore_Items_Loop;
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Clear_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      New_Action := CLEARTRASH;
      Toggle_Tool_Buttons(New_Action);
      Show_Message
        (Mc(Interp, "{Remove all files and directories from Trash?}"),
         "question");
      return TCL_OK;
   end Clear_Trash_Command;

   -- ****o* Trash/Trash.GoToTrash_Command
   -- FUNCTION
   -- Go to the selected directory in Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToTrash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
   begin
      Common.Current_Directory :=
        To_Unbounded_String(Normalize_Pathname(CArgv.Arg(Argv, 1)));
      DestinationDirectory :=
        Delete
          (Common.Current_Directory, 1,
           Length
             (To_Unbounded_String
                (Value("HOME") & "/.local/share/Trash/files")));
      Load_Directory(To_String(Common.Current_Directory));
      Update_Directory_List(True);
      Execute_Modules
        (Interp, ON_ENTER,
         "{" & To_String(Common.Current_Directory) & "}");
      return TCL_OK;
   end GoToTrash_Command;

   procedure CreateTrash is
   begin
      Add_Command("RestoreItems", Restore_Item_Command'Access);
      Add_Command("ClearTrash", Clear_Trash_Command'Access);
      Add_Command("GoToTrash", GoToTrash_Command'Access);
      CreateTrashUI;
   end CreateTrash;

end Trash;
