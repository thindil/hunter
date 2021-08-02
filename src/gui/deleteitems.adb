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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body DeleteItems is

   function Delete_Selected return Boolean is
      Go_Up, Success: Boolean := False;
      Arguments: Argument_List :=
        (1 => new String'("-rf"), 2 => new String'(""));
      Old_Setting: constant Boolean := Settings.Delete_Files;
      Delete_Time: String(1 .. 19) := (others => ' ');
      procedure Move_To_Trash(Name: Unbounded_String) is
         New_Name: Unbounded_String;
         Trash_File: File_Type;
      begin
         New_Name :=
           Trim
             (Source =>
                To_Unbounded_String
                  (Source =>
                     Hash_Type'Image
                       (Ada.Strings.Unbounded.Hash
                          (Key => Name & Image(Date => Clock)))),
              Side => Both);
         Create
           (File => Trash_File, Mode => Out_File,
            Name =>
              Ada.Environment_Variables.Value(Name => "HOME") &
              "/.local/share/Trash/info/" & To_String(Source => New_Name) &
              ".trashinfo");
         Put_Line(File => Trash_File, Item => "[Trash Info]");
         Put_Line
           (File => Trash_File, Item => "Path=" & To_String(Source => Name));
         --## rule off ASSIGNMENTS
         Delete_Time := Image(Date => Clock, Time_Zone => UTC_Time_Offset);
         Delete_Time(11) := 'T';
         --## rule on ASSIGNMENTS
         Put_Line(File => Trash_File, Item => "DeletionDate=" & Delete_Time);
         Close(File => Trash_File);
         Rename_File
           (Old_Name => To_String(Source => Name),
            New_Name =>
              Ada.Environment_Variables.Value(Name => "HOME") &
              "/.local/share/Trash/files/" & To_String(Source => New_Name),
            Success => Success);
      end Move_To_Trash;
      procedure Add_Trash(Sub_Directory: String) is
         Search: Search_Type;
         Item: Directory_Entry_Type;
      begin
         Start_Search
           (Search => Search,
            Directory =>
              Ada.Environment_Variables.Value(Name => "HOME") &
              "/.local/share/Trash/" & Sub_Directory,
            Pattern => "*");
         Add_Items_To_Trash_Loop :
         while More_Entries(Search => Search) loop
            Get_Next_Entry(Search => Search, Directory_Entry => Item);
            if Simple_Name(Directory_Entry => Item) not in "." | ".." then
               Selected_Items.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Source => Full_Name(Directory_Entry => Item)));
            end if;
         end loop Add_Items_To_Trash_Loop;
         End_Search(Search => Search);
      end Add_Trash;
   begin
      Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.local/share/Trash/info");
      Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.local/share/Trash/files");
      if New_Action = CLEARTRASH then
         Settings.Delete_Files := True;
         Selected_Items.Clear;
         Add_Trash(Sub_Directory => "info");
         Add_Trash(Sub_Directory => "files");
      end if;
      Delete_Items_Loop :
      for Item of Selected_Items loop
         Update_Progress_Bar;
         if Is_Directory(Name => To_String(Source => Item)) then
            Arguments(2) := new String'(To_String(Source => Item));
            if Settings.Delete_Files or New_Action = DELETETRASH then
               Spawn
                 (Program_Name => Locate_Exec_On_Path(Exec_Name => "rm").all,
                  Args => Arguments, Success => Success);
               if not Success then
                  raise Directory_Error with To_String(Source => Item);
               end if;
            else
               Move_To_Trash(Name => Item);
            end if;
            if Item = MainWindow.Current_Directory then
               Go_Up := True;
            end if;
         else
            if Settings.Delete_Files or New_Action = DELETETRASH then
               Delete_File(Name => To_String(Source => Item));
            else
               Move_To_Trash(Name => Item);
            end if;
         end if;
         if New_Action = DELETETRASH then
            Delete_File
              (Name =>
                 Ada.Environment_Variables.Value(Name => "HOME") &
                 "/.local/share/Trash/info/" &
                 Simple_Name(Name => To_String(Source => Item)) &
                 ".trashinfo");
         end if;
      end loop Delete_Items_Loop;
      if New_Action = CLEARTRASH then
         Settings.Delete_Files := Old_Setting;
      end if;
      Selected_Items.Clear;
      Current_Selected := MainWindow.Current_Directory;
      return Go_Up;
   exception
      when An_Exception : Ada.Directories.Use_Error =>
         ShowMessage
           (Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Could not delete selected files or directories. Reason:}") &
              " " & Exception_Message(X => An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         ShowMessage
           (Message =>
              Mc
                (Interp => Get_Context,
                 Src_String => "{Can't delete selected directory:}") &
              " " & Exception_Message(X => An_Exception));
         raise;
      when others =>
         ShowMessage
           (Message =>
              Mc
                (Interp => Get_Context,
                 Src_String =>
                   "{Unknown error during deleting files or directories.}"));
         raise;
   end Delete_Selected;

   -- ****o* DeleteItems/DeleteItems.Start_Deleting_Command
   -- FUNCTION
   -- Show confirmation to delete the selected files and directories
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StartDeleting
   -- SOURCE
   function Start_Deleting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Start_Deleting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Message: Unbounded_String;
      File_Line: Unbounded_String := Null_Unbounded_String;
      File_Info: File_Type;
   begin
      New_Action := (if New_Action /= SHOWTRASH then DELETE else DELETETRASH);
      Message :=
        (if Settings.Delete_Files or New_Action = DELETETRASH then
           To_Unbounded_String
             (Source => Mc(Interp => Interp, Src_String => "{Delete?}") & LF)
         else To_Unbounded_String
             (Source =>
                Mc(Interp => Interp, Src_String => "{Move to trash?}") & LF));
      Add_Items_To_Delete_Loop :
      for I in Selected_Items.First_Index .. Selected_Items.Last_Index loop
         if New_Action = DELETE then
            Append(Source => Message, New_Item => Selected_Items(I));
         else
            Open
              (File => File_Info, Mode => In_File,
               Name =>
                 Ada.Environment_Variables.Value(Name => "HOME") &
                 "/.local/share/Trash/info/" &
                 Simple_Name(Name => To_String(Source => Selected_Items(I))) &
                 ".trashinfo");
            Skip_Line(File => File_Info);
            Get_Item_Name_Loop :
            for J in 1 .. 2 loop
               File_Line :=
                 To_Unbounded_String(Source => Get_Line(File => File_Info));
               if Slice(Source => File_Line, Low => 1, High => 4) = "Path" then
                  Append
                    (Source => Message,
                     New_Item =>
                       Simple_Name
                         (Name =>
                            Slice
                              (Source => File_Line, Low => 6,
                               High => Length(Source => File_Line))));
               end if;
            end loop Get_Item_Name_Loop;
            Close(File_Info);
         end if;
         if not Is_Symbolic_Link(To_String(Selected_Items(I)))
           and then Is_Directory(To_String(Selected_Items(I))) then
            Append(Message, Mc(Interp, "{(and its content)}"));
         end if;
         if I /= Selected_Items.Last_Index then
            Append(Message, LF);
         end if;
         if I = 10 then
            Append(Message, Mc(Interp, "{(and more)}"));
            exit Add_Items_To_Delete_Loop;
         end if;
      end loop Add_Items_To_Delete_Loop;
      Toggle_Tool_Buttons(New_Action);
      ShowMessage(To_String(Message), "question");
      return TCL_OK;
   end Start_Deleting_Command;

   procedure Create_Delete_Ui is
   begin
      Add_Command("StartDeleting", Start_Deleting_Command'Access);
   end Create_Delete_Ui;

end DeleteItems;
