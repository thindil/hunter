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
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with LoadData;
with LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData;
with ShowItems; use ShowItems;
with Utils;
with Utils.UI; use Utils.UI;

package body CopyItems.UI is

   -- ****iv* CopyItemsUI/CopyItemsUI.Source_Directory
   -- FUNCTION
   -- Full path to the source directory of copied files and directories
   -- SOURCE
   Source_Directory: Unbounded_String;
   -- ****

   -- ****o* CopyItemsUI/CopyItemsUI.Copy_Data_Command
   -- FUNCTION
   -- Enter or quit copying items mode
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CopyData
   -- SOURCE
   function Copy_Data_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Copy_Data_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;

      Overwrite_Item: Boolean := False;
   begin
      if Copy_Items_List.Length > 0
        and then
          Containing_Directory
            (Name => To_String(Source => Copy_Items_List(1))) =
          To_String(Source => DestinationDirectory) then
         Copy_Items_List.Clear;
         ShowPreview;
         Toggle_Tool_Buttons(Action => New_Action, Finished => True);
         return TCL_OK;
      end if;
      if Copy_Items_List.Length = 0 then
         Copy_Items_List := Selected_Items;
         Source_Directory := MainWindow.Current_Directory;
         New_Action := COPY;
         Toggle_Tool_Buttons(Action => New_Action);
         ShowDestination;
         Bind_To_Main_Window
           (Interp => Interp, Sequence => "<Escape>",
            Script =>
              "{.mainframe.toolbars.actiontoolbar.cancelbutton invoke}");
         return TCL_OK;
      end if;
      if not Is_Write_Accessible_File
          (Name => To_String(Source => MainWindow.Current_Directory)) then
         Show_Message
           (Message =>
              Mc
                (Interp => Interp,
                 Src_String =>
                   "{You don't have permissions to copy selected items here.}"));
         return TCL_OK;
      end if;
      New_Action := COPY;
      Update_Progress_Bar(Amount => Positive(Copy_Items_List.Length));
      Copy_Selected(Overwrite => Overwrite_Item);
      return TCL_OK;
   end Copy_Data_Command;

   procedure Copy_Selected(Overwrite: in out Boolean) is
      use Tcl.Tk.Ada;
      use LoadData.UI;
      use RefreshData;

   begin
      if not Copy_Items(Get_Context, Overwrite) then
         return;
      end if;
      if Settings.Show_Finished_Info then
         Show_Message
           (Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{All selected files and directories have been copied.}"),
            Message_Type => "message");
      end if;
      MainWindow.Current_Directory :=
        (if Settings.Stay_In_Old then Source_Directory
         else DestinationDirectory);
      Load_Directory
        (Directory_Name => To_String(Source => MainWindow.Current_Directory));
      Update_Directory_List(Clear => True);
      UpdateWatch(Path => To_String(Source => MainWindow.Current_Directory));
      ShowPreview;
      Toggle_Tool_Buttons(Action => New_Action, Finished => True);
   end Copy_Selected;

   procedure Skip_Copying is
      Overwrite_Item: Boolean := False;
   begin
      Copy_Items_List.Delete(Index => 1);
      Update_Progress_Bar;
      Copy_Selected(Overwrite => Overwrite_Item);
   end Skip_Copying;

   procedure Create_Copy_Ui is
   begin
      Add_Command(Name => "CopyData", Ada_Command => Copy_Data_Command'Access);
   end Create_Copy_Ui;

end CopyItems.UI;
