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
             (To_Unbounded_String
                (Hash_Type'Image
                   (Ada.Strings.Unbounded.Hash
                      (Name & To_Unbounded_String(Image(Clock))))),
              Both);
         Create
           (Trash_File, Out_File,
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/info/" & To_String(New_Name) & ".trashinfo");
         Put_Line(Trash_File, "[Trash Info]");
         Put_Line(Trash_File, "Path=" & To_String(Name));
         --## rule off ASSIGNMENTS
         Delete_Time := Image(Date => Clock, Time_Zone => UTC_Time_Offset);
         Delete_Time(11) := 'T';
         --## rule on ASSIGNMENTS
         Put_Line(Trash_File, "DeletionDate=" & Delete_Time);
         Close(Trash_File);
         Rename_File
           (To_String(Name),
            Ada.Environment_Variables.Value("HOME") &
            "/.local/share/Trash/files/" & To_String(New_Name),
            Success);
      end Move_To_Trash;
      procedure AddTrash(SubDirectory: String) is
         Search: Search_Type;
         Item: Directory_Entry_Type;
      begin
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/" &
            SubDirectory,
            "*");
         Add_Items_To_Trash_Loop :
         while More_Entries(Search) loop
            Get_Next_Entry(Search, Item);
            if Simple_Name(Item) not in "." | ".." then
               Selected_Items.Append
                 (New_Item => To_Unbounded_String(Full_Name(Item)));
            end if;
         end loop Add_Items_To_Trash_Loop;
         End_Search(Search);
      end AddTrash;
   begin
      Create_Path
        (Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/info");
      Create_Path
        (Ada.Environment_Variables.Value("HOME") &
         "/.local/share/Trash/files");
      if New_Action = CLEARTRASH then
         Settings.Delete_Files := True;
         Selected_Items.Clear;
         AddTrash("info");
         AddTrash("files");
      end if;
      Delete_Items_Loop :
      for Item of Selected_Items loop
         Update_Progress_Bar;
         if Is_Directory(To_String(Item)) then
            Arguments(2) := new String'(To_String(Item));
            if Settings.Delete_Files or New_Action = DELETETRASH then
               Spawn(Locate_Exec_On_Path("rm").all, Arguments, Success);
               if not Success then
                  raise Directory_Error with To_String(Item);
               end if;
            else
               Move_To_Trash(Item);
            end if;
            if Item = MainWindow.Current_Directory then
               Go_Up := True;
            end if;
         else
            if Settings.Delete_Files or New_Action = DELETETRASH then
               Delete_File(To_String(Item));
            else
               Move_To_Trash(Item);
            end if;
         end if;
         if New_Action = DELETETRASH then
            Delete_File
              (Ada.Environment_Variables.Value("HOME") &
               "/.local/share/Trash/info/" & Simple_Name(To_String(Item)) &
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
           (Mc
              (Get_Context,
               "{Could not delete selected files or directories. Reason:}") &
            " " & Exception_Message(An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         ShowMessage
           (Mc(Get_Context, "{Can't delete selected directory:}") & " " &
            Exception_Message(An_Exception));
         raise;
      when others =>
         ShowMessage
           (Mc
              (Get_Context,
               "{Unknown error during deleting files or directories.}"));
         raise;
   end Delete_Selected;

   -- ****o* DeleteItems/DeleteItems.Start_Deleting_Command
   -- FUNCTION
   -- Show confirmation to delete the selected files and directories
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StartDeleting
   -- SOURCE
   function Start_Deleting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Start_Deleting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Message, FileLine: Unbounded_String;
      FileInfo: File_Type;
      I: Positive := Selected_Items.First_Index;
   begin
      New_Action := (if New_Action /= SHOWTRASH then DELETE else DELETETRASH);
      Message :=
        (if Settings.Delete_Files or New_Action = DELETETRASH then
           To_Unbounded_String(Mc(Interp, "{Delete?}") & LF)
         else To_Unbounded_String(Mc(Interp, "{Move to trash?}") & LF));
      Add_Items_To_Delete_Loop :
      while I <= Selected_Items.Last_Index loop
         if New_Action = DELETE then
            Append(Message, Selected_Items(I));
         else
            Open
              (FileInfo, In_File,
               Ada.Environment_Variables.Value("HOME") &
               "/.local/share/Trash/info/" &
               Simple_Name(To_String(Selected_Items(I))) & ".trashinfo");
            Skip_Line(FileInfo);
            Get_Item_Name_Loop :
            for I in 1 .. 2 loop
               FileLine := To_Unbounded_String(Get_Line(FileInfo));
               if Slice(FileLine, 1, 4) = "Path" then
                  Append
                    (Message,
                     Simple_Name(Slice(FileLine, 6, Length(FileLine))));
               end if;
            end loop Get_Item_Name_Loop;
            Close(FileInfo);
         end if;
         if not Is_Symbolic_Link(To_String(Selected_Items(I)))
           and then Is_Directory(To_String(Selected_Items(I))) then
            Append(Message, Mc(Interp, "{(and its content)}"));
         end if;
         if I /= Selected_Items.Last_Index then
            Append(Message, LF);
         end if;
         I := I + 1;
         if I = 11 then
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
