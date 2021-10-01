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

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;
with GNAT.OS_Lib;
with CArgv;
with Tcl;
with Tcl.MsgCat.Ada;
with MainWindow;
with Messages.UI;
with Preferences;
with Utils;
with Utils.UI; use Utils.UI;

package body DeleteItems.UI is

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
      use Ada.Characters.Latin_1;
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use GNAT.OS_Lib;
      use Tcl.MsgCat.Ada;
      use MainWindow;
      use Messages.UI;
      use Preferences;

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
            Append
              (Source => Message,
               New_Item =>
                 Simple_Name(Name => To_String(Source => Selected_Items(I))));
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
            Close(File => File_Info);
         end if;
         if not Is_Symbolic_Link
             (Name => To_String(Source => Selected_Items(I)))
           and then Is_Directory
             (Name => To_String(Source => Selected_Items(I))) then
            Append
              (Source => Message,
               New_Item =>
                 Mc(Interp => Interp, Src_String => "{(and its content)}"));
         end if;
         if I /= Selected_Items.Last_Index then
            Append(Source => Message, New_Item => LF);
         end if;
         if I = 10 then
            Append
              (Source => Message,
               New_Item => Mc(Interp => Interp, Src_String => "{(and more)}"));
            exit Add_Items_To_Delete_Loop;
         end if;
      end loop Add_Items_To_Delete_Loop;
      Toggle_Tool_Buttons(Action => New_Action);
      Show_Message
        (Message => To_String(Source => Message), Message_Type => "question");
      return TCL_OK;
   end Start_Deleting_Command;

   procedure Create_Delete_Ui is
   begin
      Add_Command
        (Name => "StartDeleting",
         Ada_Command => Start_Deleting_Command'Access);
   end Create_Delete_Ui;

end DeleteItems.UI;
