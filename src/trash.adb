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

with Ada.Calendar.Formatting;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with Inotify;
with LoadData;
with LoadData.UI;
with MainWindow;
with Messages.UI; use Messages.UI;
with Modules;
with ShowItems; use ShowItems;
with Trash.UI; use Trash.UI;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Trash is

   -- ****o* Trash/Trash.Restore_Item_Command
   -- FUNCTION
   -- Restore the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RestoreItem
   -- SOURCE
   function Restore_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Restore_Info, File_Line, Destination, Item_Type: Unbounded_String :=
        Null_Unbounded_String;
      Start_Index: Positive := 1;
      File_Info: File_Type;
   begin
      Restore_Items_Loop :
      for Item of Selected_Items loop
         Start_Index := Index(Source => Item, Pattern => "files");
         Restore_Info :=
           Unbounded_Slice(Source => Item, Low => 1, High => Start_Index - 1) &
           "info" &
           Unbounded_Slice
             (Source => Item, Low => Start_Index + 5,
              High => Length(Source => Item)) &
           To_Unbounded_String(Source => ".trashinfo");
         Open
           (File => File_Info, Mode => In_File,
            Name => To_String(Source => Restore_Info));
         Skip_Line(File => File_Info);
         Restore_Item_Loop :
         for I in 1 .. 2 loop
            File_Line :=
              To_Unbounded_String(Source => Get_Line(File => File_Info));
            if Slice(Source => File_Line, Low => 1, High => 4) = "Path" then
               Destination :=
                 Unbounded_Slice
                   (Source => File_Line, Low => 6,
                    High => Length(Source => File_Line));
               if Ada.Directories.Exists
                   (Name => To_String(Source => Destination)) then
                  Item_Type :=
                    (if Is_Directory(Name => To_String(Source => Destination))
                     then
                       To_Unbounded_String
                         (Source =>
                            Mc(Interp => Interp, Src_String => "{Directory}"))
                     else To_Unbounded_String
                         (Source =>
                            Mc(Interp => Interp, Src_String => "{File}")));
                  Show_Message
                    (Message =>
                       Mc(Interp => Interp, Src_String => "{Can't restore}") &
                       " " & To_String(Source => Destination) & " " &
                       To_String(Source => Item_Type) & " " &
                       Mc(Interp => Interp,
                          Src_String => "{with that name exists.}"));
                  Close(File => File_Info);
                  return
                    Show_Trash_Command
                      (ClientData => Client_Data, Interp => Interp,
                       Argc => Argc, Argv => Argv);
               end if;
               Rename
                 (Old_Name => To_String(Source => Item),
                  New_Name =>
                    Slice
                      (Source => File_Line, Low => 6,
                       High => Length(Source => File_Line)));
            end if;
         end loop Restore_Item_Loop;
         Close(File => File_Info);
         Delete_File(Name => To_String(Source => Restore_Info));
      end loop Restore_Items_Loop;
      return
        Show_Trash_Command
          (ClientData => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Restore_Item_Command;

   -- ****o* Trash/Trash.Clear_Trash_Command
   -- FUNCTION
   -- Remove everything from the Trash
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ClearTrash
   -- SOURCE
   function Clear_Trash_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Clear_Trash_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
   begin
      New_Action := CLEARTRASH;
      Toggle_Tool_Buttons(Action => New_Action);
      Show_Message
        (Message =>
           Mc
             (Interp => Interp,
              Src_String => "{Remove all files and directories from Trash?}"),
         Message_Type => "question");
      return TCL_OK;
   end Clear_Trash_Command;

   -- ****o* Trash/Trash.Go_To_Trash_Command
   -- FUNCTION
   -- Go to the selected directory in Trash
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToTrash path
   -- Path is the full path to the directory which will be set as current
   -- directory (and show to the user)
   -- SOURCE
   function Go_To_Trash_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Go_To_Trash_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use LoadData.UI;
      use MainWindow;
      use Modules;

   begin
      Common.Current_Directory :=
        To_Unbounded_String
          (Source =>
             Normalize_Pathname(Name => CArgv.Arg(Argv => Argv, N => 1)));
      Destination_Directory :=
        Delete
          (Source => Common.Current_Directory, From => 1,
           Through =>
             Length
               (Source =>
                  To_Unbounded_String
                    (Source =>
                       Value(Name => "HOME") & "/.local/share/Trash/files")));
      Load_Directory
        (Directory_Name => To_String(Source => Common.Current_Directory));
      Update_Directory_List(Clear => True);
      Execute_Modules
        (Interpreter => Interp, State => On_Enter_Trigger,
         Arguments =>
           "{" & To_String(Source => Common.Current_Directory) & "}");
      return TCL_OK;
   end Go_To_Trash_Command;

   procedure Create_Trash is
   begin
      Add_Command
        (Name => "RestoreItems", Ada_Command => Restore_Item_Command'Access);
      Add_Command
        (Name => "ClearTrash", Ada_Command => Clear_Trash_Command'Access);
      Add_Command
        (Name => "GoToTrash", Ada_Command => Go_To_Trash_Command'Access);
      CreateTrashUI;
   end Create_Trash;

   procedure Load_Trash_Data is
      use Ada.Calendar.Formatting;
      use GNAT.Directory_Operations;
      use Inotify;
      use LoadData;

      Directory, Sub_Directory: Dir_Type;
      File_Name, Sub_File_Name: String(1 .. 1_024) := (others => ' ');
      Last: Natural range 0 .. File_Name'Last := 0;
      Sub_Last: Natural range 0 .. Sub_File_Name'Last := 0;
      File_Info: File_Type;
      Size: File_Size := 0;
      File_Line, Full_Name, Mime_Type: Unbounded_String :=
        Null_Unbounded_String;
      Item: Item_Record := Empty_Item;
   begin
      Temporary_Stop := True;
      Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.local/share/Trash/info");
      Create_Path
        (New_Directory =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.local/share/Trash/files");
      if New_Action /= SHOWTRASH then
         New_Action := SHOWTRASH;
         Toggle_Tool_Buttons(Action => SHOWTRASH);
      end if;
      Items_List.Clear;
      Common.Current_Directory :=
        To_Unbounded_String
          (Source => Value(Name => "HOME") & "/.local/share/Trash/files");
      Destination_Directory :=
        Delete
          (Source => Common.Current_Directory, From => 1,
           Through =>
             Length
               (Source =>
                  To_Unbounded_String
                    (Source =>
                       Value(Name => "HOME") & "/.local/share/Trash/files")));
      Open
        (Dir => Directory,
         Dir_Name => Value(Name => "HOME") & "/.local/share/Trash/files");
      Read_Trash_Content_Loop :
      loop
         Read(Dir => Directory, Str => File_Name, Last => Last);
         exit Read_Trash_Content_Loop when Last = 0;
         if File_Name(1 .. Last) = "." or File_Name(1 .. Last) = ".." then
            goto End_Of_Loop;
         end if;
         Full_Name :=
           To_Unbounded_String
             (Source =>
                Value(Name => "HOME") & "/.local/share/Trash/files/" &
                File_Name(1 .. Last));
         Item.Path := Full_Name;
         Open
           (File => File_Info, Mode => In_File,
            Name =>
              Value(Name => "HOME") & "/.local/share/Trash/info/" &
              File_Name(1 .. Last) & ".trashinfo");
         Skip_Line(File => File_Info);
         Read_File_Path_Loop :
         for I in 1 .. 2 loop
            File_Line :=
              To_Unbounded_String(Source => Get_Line(File => File_Info));
            if Slice(Source => File_Line, Low => 1, High => 4) = "Path" then
               Item.Name :=
                 To_Unbounded_String
                   (Source =>
                      Simple_Name
                        (Name =>
                           Slice
                             (Source => File_Line, Low => 6,
                              High => Length(Source => File_Line))));
            else
               File_Line :=
                 Unbounded_Slice
                   (Source => File_Line, Low => 14,
                    High => Length(Source => File_Line));
               Replace_Slice
                 (Source => File_Line, Low => 11, High => 11, By => " ");
               Item.Modified := Value(Date => To_String(Source => File_Line));
            end if;
         end loop Read_File_Path_Loop;
         Close(File => File_Info);
         Item.Is_Hidden := (if File_Name(1) = '.' then True else False);
         if Is_Directory(Name => To_String(Source => Full_Name)) then
            Item.Is_Directory := True;
            Item.Image :=
              (if Is_Symbolic_Link(Name => To_String(Source => Full_Name)) then
                 To_Unbounded_String(Source => "emblem-symbolic-link")
               else To_Unbounded_String(Source => "folder"));
            if Is_Read_Accessible_File
                (Name => To_String(Source => Full_Name)) then
               Open
                 (Dir => Sub_Directory,
                  Dir_Name => To_String(Source => Full_Name));
               Size := 0;
               Count_Directory_Size_Loop :
               loop
                  Read
                    (Dir => Sub_Directory, Str => Sub_File_Name,
                     Last => Sub_Last);
                  exit Count_Directory_Size_Loop when Sub_Last = 0;
                  Size := Size + 1;
               end loop Count_Directory_Size_Loop;
               Close(Dir => Sub_Directory);
               Item.Size := Item_Size(Size - 2);
            else
               Item.Size := -1;
            end if;
         else
            Item.Is_Directory := False;
            if Is_Symbolic_Link(Name => To_String(Source => Full_Name)) then
               Item.Image :=
                 To_Unbounded_String(Source => "emblem-symbolic-link");
            elsif Is_Executable_File
                (Name => To_String(Source => Full_Name)) then
               Item.Image :=
                 To_Unbounded_String(Source => "application-x-executable");
            else
               Mime_Type :=
                 To_Unbounded_String
                   (Source =>
                      Get_Mime_Type
                        (File_Name => To_String(Source => Full_Name)));
               if Index(Source => Mime_Type, Pattern => "audio") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "audio-x-generic");
               elsif Index(Source => Mime_Type, Pattern => "font") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "font-x-generic");
               elsif Index(Source => Mime_Type, Pattern => "image") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "image-x-generic");
               elsif Index(Source => Mime_Type, Pattern => "video") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "video-x-generic");
               elsif Index(Source => Mime_Type, Pattern => "text/x-script") >
                 0 then
                  Item.Image := To_Unbounded_String(Source => "text-x-script");
               elsif Mime_Type =
                 To_Unbounded_String(Source => "text/html") then
                  Item.Image := To_Unbounded_String(Source => "text-html");
               elsif Index(Source => Mime_Type, Pattern => "zip") > 0 or
                 Index(Source => Mime_Type, Pattern => "x-xz") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "package-x-generic");
               elsif Index(Source => Mime_Type, Pattern => "text") > 0 then
                  Item.Image :=
                    To_Unbounded_String(Source => "text-x-generic");
               else
                  Item.Image :=
                    To_Unbounded_String(Source => "text-x-generic-template");
               end if;
            end if;
            if not Is_Read_Accessible_File
                (Name => To_String(Source => Full_Name)) then
               Item.Size := -1;
               Items_List.Append(New_Item => Item);
               goto End_Of_Loop;
            end if;
            if Is_Symbolic_Link(Name => To_String(Source => Full_Name)) then
               Item.Size := -2;
            elsif Is_Regular_File(Name => To_String(Source => Full_Name)) then
               Item.Size :=
                 Item_Size
                   (Ada.Directories.Size
                      (Name => To_String(Source => Full_Name)));
            else
               Item.Size := Empty_Item_Size;
            end if;
         end if;
         Items_List.Append(New_Item => Item);
         <<End_Of_Loop>>
      end loop Read_Trash_Content_Loop;
      Close(Dir => Directory);
   end Load_Trash_Data;

end Trash;
