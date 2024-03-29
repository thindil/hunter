-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Common; use Common;
with LoadData;
with LoadData.UI;
with MainWindow;
with Messages;
with Messages.UI; use Messages.UI;
with Preferences;
with RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body MoveItems.UI is

   --## rule off GLOBAL_REFERENCES
   -- ****iv* MoveItems/MoveItems.Source_Directory
   -- FUNCTION
   -- Full path to the source directory of moved files and directories
   -- SOURCE
   Source_Directory: Unbounded_String;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****o* MoveItems/MoveItems.Move_Data_Command
   -- FUNCTION
   -- Enter or quit moving items mode
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveData
   -- SOURCE
   function Move_Data_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Data_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl;
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;

      Overwrite_Item: Boolean := False;
   begin
      if Move_Items_List.Length > 0
        and then
          Containing_Directory
            (Name => To_String(Source => Move_Items_List(1))) =
          To_String(Source => Destination_Directory) then
         Move_Items_List.Clear;
         Show_Preview;
         Toggle_Tool_Buttons(Action => New_Action, Finished => True);
         return TCL_OK;
      end if;
      if Move_Items_List.Length = 0 then
         Move_Items_List := Selected_Items;
         Source_Directory := Common.Current_Directory;
         New_Action := MOVE;
         Toggle_Tool_Buttons(Action => New_Action);
         Show_Destination;
         Bind_To_Main_Window
           (Interp => Interp, Sequence => "<Escape>",
            Script =>
              "{.mainframe.toolbars.actiontoolbar.cancelbutton invoke}");
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File
          (Name => To_String(Source => Common.Current_Directory)) then
         Show_Message
           (Message =>
              Mc
                (Interp => Interp,
                 Src_String =>
                   "{You don't have permissions to move selected items here.}"));
         return TCL_OK;
      end if;
      New_Action := MOVE;
      Update_Progress_Bar(Amount => Positive(Move_Items_List.Length));
      Move_Selected(Overwrite => Overwrite_Item);
      return TCL_OK;
   end Move_Data_Command;

   procedure Move_Selected(Overwrite: in out Boolean) is
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada;
      use LoadData.UI;
      use MainWindow;
      use Messages;
      use Preferences;
      use RefreshData;

      Success: Boolean := True;
      Item_Type, New_Name, File_Extension: Unbounded_String :=
        Null_Unbounded_String;
   begin
      Move_Items_Loop :
      while Move_Items_List.Length > 0 loop
         New_Name :=
           Destination_Directory & To_Unbounded_String(Source => "/") &
           Simple_Name(Name => To_String(Source => Move_Items_List(1)));
         if Exists(Name => To_String(Source => New_Name)) then
            if not Overwrite and Settings.Overwrite_On_Exist then
               Item_Type :=
                 (if Is_Directory(Name => To_String(Source => New_Name)) then
                    To_Unbounded_String
                      (Source =>
                         Mc
                           (Interp => Get_Context,
                            Src_String => "{Directory}"))
                  else To_Unbounded_String
                      (Source =>
                         Mc(Interp => Get_Context, Src_String => "{File}")));
               Show_Message
                 (Message =>
                    To_String(Source => Item_Type) & " " &
                    Simple_Name
                      (Name => To_String(Source => Move_Items_List(1))) &
                    " " &
                    Mc(Interp => Get_Context,
                       Src_String => "{exists. Do you want to overwrite it?}"),
                  Message_Type => "question");
               return;
            end if;
            if not Settings.Overwrite_On_Exist then
               File_Extension :=
                 To_Unbounded_String
                   (Source =>
                      Extension
                        (Name => To_String(Source => Move_Items_List(1))));
               New_File_Name_Loop :
               loop
                  New_Name :=
                    Destination_Directory &
                    To_Unbounded_String
                      (Source =>
                         "/" &
                         Ada.Directories.Base_Name
                           (Name => To_String(Source => New_Name)) &
                         "_");
                  if Length(Source => File_Extension) > 0 then
                     New_Name := New_Name & "." & File_Extension;
                  end if;
                  exit New_File_Name_Loop when not Exists
                      (Name => To_String(Source => New_Name));
               end loop New_File_Name_Loop;
            end if;
         end if;
         Rename_File
           (Old_Name => To_String(Source => Move_Items_List(1)),
            New_Name => To_String(Source => New_Name), Success => Success);
         if not Success then
            Copy_Item
              (Name => To_String(Source => Move_Items_List(1)),
               Path => Destination_Directory, Success => Success);
            if not Success then
               Show_Message
                 (Message =>
                    Mc(Interp => Get_Context, Src_String => "{Can't move}") &
                    " " & To_String(Source => Move_Items_List(1)) & ".");
               return;
            end if;
            if Is_Directory
                (Name => To_String(Source => Move_Items_List(1))) then
               Remove_Dir
                 (Dir_Name => To_String(Source => Move_Items_List(1)),
                  Recursive => True);
            else
               Delete_File(Name => To_String(Source => Move_Items_List(1)));
            end if;
         end if;
         Move_Items_List.Delete(Index => 1);
         if not Yes_For_All then
            Overwrite := False;
         end if;
         Update_Progress_Bar;
      end loop Move_Items_Loop;
      Move_Items_List.Clear;
      if Settings.Show_Finished_Info then
         Show_Message
           (Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{All selected files and directories have been moved.}"),
            Message_Type => "message");
      end if;
      Common.Current_Directory :=
        (if Settings.Stay_In_Old then Source_Directory
         else Destination_Directory);
      Current_Selected :=
        Common.Current_Directory & "/" &
        Simple_Name(Name => To_String(Source => Current_Selected));
      Load_Directory
        (Directory_Name => To_String(Source => Common.Current_Directory));
      New_Action := CREATEFILE;
      Update_Directory_List(Clear => True);
      Update_Watch(Path => To_String(Source => Common.Current_Directory));
      Show_Preview;
      Toggle_Tool_Buttons(Action => New_Action, Finished => True);
   end Move_Selected;

   procedure Skip_Moving is
      Overwrite_Item: Boolean := False;
   begin
      Move_Items_List.Delete(Index => 1);
      Update_Progress_Bar;
      Move_Selected(Overwrite => Overwrite_Item);
   end Skip_Moving;

   procedure Create_Move_Ui is
      use Utils;

   begin
      Add_Command(Name => "MoveData", Ada_Command => Move_Data_Command'Access);
   end Create_Move_Ui;

end MoveItems.UI;
