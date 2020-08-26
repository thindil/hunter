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

with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with CopyItems; use CopyItems;
with LoadData; use LoadData;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body MoveItems is

   -- ****iv* MoveItems/SourceDirectory
   -- FUNCTION
   -- Full path to the source directory of moved files and directories
   -- SOURCE
   SourceDirectory: Unbounded_String;
   -- ****

   -- ****o* MoveItems/Move_Data_Command
   -- FUNCTION
   -- Enter or quit moving items mode
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveData
   -- SOURCE
   function Move_Data_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Data_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      OverwriteItem: Boolean := False;
   begin
      if MoveItemsList.Length > 0
        and then Containing_Directory(To_String(MoveItemsList(1))) =
          To_String(DestinationDirectory) then
         MoveItemsList.Clear;
         ShowPreview;
         ToggleToolButtons(NewAction, True);
         return TCL_OK;
      end if;
      if MoveItemsList.Length = 0 then
         MoveItemsList := SelectedItems;
         SourceDirectory := CurrentDirectory;
         NewAction := MOVE;
         ToggleToolButtons(NewAction);
         ShowDestination;
         Bind_To_Main_Window
           (Interp, "<Escape>",
            "{.mainframe.toolbars.actiontoolbar.cancelbutton invoke}");
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File(To_String(CurrentDirectory)) then
         ShowMessage
           (Mc
              (Interp,
               "{You don't have permissions to move selected items here.}"));
         return TCL_OK;
      end if;
      NewAction := MOVE;
      SetProgressBar(Positive(MoveItemsList.Length));
      MoveSelected(OverwriteItem);
      return TCL_OK;
   end Move_Data_Command;

   procedure MoveSelected(Overwrite: in out Boolean) is
      ItemType: Unbounded_String;
      Success: Boolean := True;
      NewName, FileExtension: Unbounded_String;
   begin
      while MoveItemsList.Length > 0 loop
         NewName :=
           DestinationDirectory & To_Unbounded_String("/") &
           Simple_Name(To_String(MoveItemsList(1)));
         if Exists(To_String(NewName)) then
            if not Overwrite and Settings.OverwriteOnExist then
               if Is_Directory(To_String(NewName)) then
                  ItemType :=
                    To_Unbounded_String(Mc(Get_Context, "{Directory}"));
               else
                  ItemType := To_Unbounded_String(Mc(Get_Context, "{File}"));
               end if;
               ShowMessage
                 (To_String(ItemType) & " " &
                  Simple_Name(To_String(MoveItemsList(1))) &
                  Mc(Get_Context, "{ exists. Do you want to overwrite it?}"),
                  "question");
               return;
            end if;
            if not Settings.OverwriteOnExist then
               FileExtension :=
                 To_Unbounded_String(Extension(To_String(MoveItemsList(1))));
               loop
                  NewName :=
                    DestinationDirectory &
                    To_Unbounded_String
                      ("/" & Ada.Directories.Base_Name(To_String(NewName)) &
                       "_");
                  if Length(FileExtension) > 0 then
                     NewName :=
                       NewName & To_Unbounded_String(".") & FileExtension;
                  end if;
                  exit when not Exists(To_String(NewName));
               end loop;
            end if;
         end if;
         Rename_File(To_String(MoveItemsList(1)), To_String(NewName), Success);
         if not Success then
            CopyItem
              (To_String(MoveItemsList(1)), DestinationDirectory, Success);
            if Success then
               if Is_Directory(To_String(MoveItemsList(1))) then
                  Remove_Dir(To_String(MoveItemsList(1)), True);
               else
                  Delete_File(To_String(MoveItemsList(1)));
               end if;
            else
               ShowMessage
                 (Mc(Get_Context, "{Can't move }") &
                  To_String(MoveItemsList(1)) & ".");
               return;
            end if;
         end if;
         MoveItemsList.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
         UpdateProgressBar;
      end loop;
      MoveItemsList.Clear;
      if Settings.ShowFinishedInfo then
         ShowMessage
           (Mc
              (Get_Context,
               "{All selected files and directories have been moved.}"),
            "message");
      end if;
      if Settings.StayInOld then
         CurrentDirectory := SourceDirectory;
      else
         CurrentDirectory := DestinationDirectory;
      end if;
      CurrentSelected :=
        CurrentDirectory & "/" & Simple_Name(To_String(CurrentSelected));
      LoadDirectory(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      UpdateWatch(To_String(CurrentDirectory));
      ShowPreview;
      ToggleToolButtons(NewAction, True);
   end MoveSelected;

   procedure SkipMoving is
      OverwriteItem: Boolean := False;
   begin
      MoveItemsList.Delete(Index => 1);
      UpdateProgressBar;
      MoveSelected(OverwriteItem);
   end SkipMoving;

   procedure CreateMoveUI is
   begin
      AddCommand("MoveData", Move_Data_Command'Access);
   end CreateMoveUI;

end MoveItems;
