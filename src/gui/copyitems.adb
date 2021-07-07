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
with Ada.Directories; use Ada.Directories;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body CopyItems is

   -- ****iv* CopyItems/CopyItems.Source_Directory
   -- FUNCTION
   -- Full path to the source directory of copied files and directories
   -- SOURCE
   Source_Directory: Unbounded_String;
   -- ****

   -- ****o* CopyItems/CopyItems.Copy_Data_Command
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
         ShowMessage
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

   procedure Copy_Item
     (Name: String; Path: Unbounded_String; Success: in out Boolean) is
      New_Path: Unbounded_String := Path;
      procedure Copy_File(File_Name: String) is
         New_Name: Unbounded_String :=
           New_Path &
           To_Unbounded_String(Source => "/" & Simple_Name(Name => File_Name));
      begin
         if Exists(Name => To_String(Source => New_Name)) then
            if Settings.Overwrite_On_Exist then
               Delete_File(Name => To_String(Source => New_Name));
            else
               New_File_Name_Loop :
               loop
                  New_Name :=
                    New_Path &
                    To_Unbounded_String
                      (Source =>
                         "/" &
                         Base_Name(Name => To_String(Source => New_Name)) &
                         "_." &
                         Extension(Name => To_String(Source => New_Name)));
                  exit New_File_Name_Loop when not Exists
                      (Name => To_String(Source => New_Name));
               end loop New_File_Name_Loop;
            end if;
         end if;
         GNAT.OS_Lib.Copy_File
           (Name => File_Name, Pathname => To_String(Source => New_Name),
            Success => Success, Mode => Copy, Preserve => Full);
      end Copy_File;
      procedure Process_File(Item: Directory_Entry_Type) is
      begin
         Copy_File(File_Name => Full_Name(Directory_Entry => Item));
      end Process_File;
      procedure Process_Directory(Item: Directory_Entry_Type) is
      begin
         if Simple_Name(Directory_Entry => Item) not in "." | ".." then
            Copy_Item
              (Name => Full_Name(Directory_Entry => Item), Path => New_Path,
               Success => Success);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end Process_Directory;
   begin
      if Is_Directory(Name => Name) then
         Append
           (Source => New_Path, New_Item => "/" & Simple_Name(Name => Name));
         if Exists(Name => To_String(Source => New_Path)) and
           not Settings.Overwrite_On_Exist then
            New_Directory_Name_Loop :
            loop
               New_Path := New_Path & "_";
               exit New_Directory_Name_Loop when not Exists
                   (Name => To_String(Source => New_Path));
            end loop New_Directory_Name_Loop;
         end if;
         Create_Path(New_Directory => To_String(Source => New_Path));
         Search
           (Name, "", (Directory => False, others => True),
            Process_File'Access);
         Search
           (Name, "", (Directory => True, others => False),
            Process_Directory'Access);
      else
         Copy_File(Name);
      end if;
      Update_Progress_Bar;
   end Copy_Item;

   procedure Copy_Selected(Overwrite: in out Boolean) is
      Path, ItemType: Unbounded_String;
      Success: Boolean := True;
   begin
      Copy_Items_Loop :
      while Copy_Items_List.Length > 0 loop
         Path := DestinationDirectory;
         if Exists
             (To_String(Path) & "/" &
              Simple_Name(To_String(Copy_Items_List(1)))) and
           not Overwrite and Settings.Overwrite_On_Exist then
            ItemType :=
              (if
                 Is_Directory
                   (To_String(Path) & "/" &
                    Simple_Name(To_String(Copy_Items_List(1))))
               then To_Unbounded_String(Mc(Get_Context, "{Directory}"))
               else To_Unbounded_String(Mc(Get_Context, "{File}")));
            ShowMessage
              (To_String(ItemType) & " " &
               Simple_Name(To_String(Copy_Items_List(1))) & " " &
               Mc(Get_Context, "{exists. Do you want to overwrite it?}"),
               "question");
            return;
         end if;
         Copy_Item(To_String(Copy_Items_List(1)), Path, Success);
         exit Copy_Items_Loop when not Success;
         Copy_Items_List.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
      end loop Copy_Items_Loop;
      Copy_Items_List.Clear;
      if Settings.Show_Finished_Info then
         ShowMessage
           (Mc
              (Get_Context,
               "{All selected files and directories have been copied.}"),
            "message");
      end if;
      MainWindow.Current_Directory :=
        (if Settings.Stay_In_Old then Source_Directory
         else DestinationDirectory);
      LoadDirectory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      ShowPreview;
      Toggle_Tool_Buttons(New_Action, True);
   end Copy_Selected;

   procedure Skip_Copying is
      Overwrite_Item: Boolean := False;
   begin
      Copy_Items_List.Delete(Index => 1);
      Update_Progress_Bar;
      Copy_Selected(Overwrite_Item);
   end Skip_Copying;

   procedure Create_Copy_Ui is
   begin
      Add_Command("CopyData", Copy_Data_Command'Access);
   end Create_Copy_Ui;

end CopyItems;
