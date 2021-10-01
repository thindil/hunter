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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Containers;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Tcl.MsgCat.Ada;
with MainWindow;
with Messages.UI;
with Preferences;
with Utils.UI;

package body DeleteItems is

   function Delete_Selected(Interpreter: Tcl_Interp) return Boolean is
      use Ada.Directories;
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      use GNAT.Directory_Operations;
      use GNAT.OS_Lib;
      use Tcl.MsgCat.Ada;
      use MainWindow;
      use Messages.UI;
      use Preferences;
      use Utils.UI;

      Go_Up, Success: Boolean := False;
      Arguments: Argument_List :=
        (1 => new String'("-rf"), 2 => new String'(""));
      Old_Setting: constant Boolean := Settings.Delete_Files;
      Delete_Time: String(1 .. 19) := (others => ' ');
      procedure Move_To_Trash(Name: Unbounded_String) is
         use Ada.Calendar;
         use Ada.Calendar.Formatting;
         use Ada.Calendar.Time_Zones;
         use Ada.Containers;
         use Ada.Strings;
         use Ada.Text_IO;

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
         Show_Message
           (Message =>
              Mc
                (Interp => Interpreter,
                 Src_String =>
                   "{Could not delete selected files or directories. Reason:}") &
              " " & Exception_Message(X => An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         Show_Message
           (Message =>
              Mc
                (Interp => Interpreter,
                 Src_String => "{Can't delete selected directory:}") &
              " " & Exception_Message(X => An_Exception));
         raise;
      when others =>
         Show_Message
           (Message =>
              Mc
                (Interp => Interpreter,
                 Src_String =>
                   "{Unknown error during deleting files or directories.}"));
         raise;
   end Delete_Selected;

end DeleteItems;
