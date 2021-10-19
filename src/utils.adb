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

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LibMagic;
with Messages.UI;

package body Utils is

   function Get_Mime_Type(File_Name: String) return String is
      use LibMagic;

   begin
      return Magic_File(Name => File_Name);
   end Get_Mime_Type;

   function Can_Be_Opened(Mime_Type: String) return Boolean is
      use GNAT.OS_Lib;

      Executable_Name: constant String := Find_Executable(Name => "xdg-mime");
      Return_Code: Integer;
      Output_File: File_Descriptor;
   begin
      if Executable_Name = "" then
         return False;
      end if;
      Output_File := Open_Append(Name => "/dev/null", Fmode => Text);
      Spawn
        (Program_Name => Executable_Name,
         Args =>
           Argument_String_To_List
             (Arg_String => "query default " & Mime_Type).all,
         Output_File_Descriptor => Output_File, Return_Code => Return_Code,
         Err_To_Out => True);
      Close(FD => Output_File);
      if Return_Code /= 0 then
         return False;
      end if;
      return True;
   end Can_Be_Opened;

   function Count_File_Size(Size: File_Size) return String is
      Multiplier: Natural range 0 .. 8;
      New_Size: File_Size;
      Size_Shortcuts: constant array(Natural range 0 .. 8) of String(1 .. 3) :=
        (0 => "B  ", 1 => "KiB", 2 => "MiB", 3 => "GiB", 4 => "TiB",
         5 => "PiB", 6 => "EiB", 7 => "ZiB", 8 => "YiB");
   begin
      New_Size := Size;
      Multiplier := 0;
      Count_Size_Loop :
      while New_Size > 1_024 loop
         exit Count_Size_Loop when Multiplier = 8;
         New_Size := New_Size / 1_024;
         Multiplier := Multiplier + 1;
      end loop Count_Size_Loop;
      return File_Size'Image(New_Size) & " " & Size_Shortcuts(Multiplier);
   end Count_File_Size;

   function Is_Text(Mime_Type: String) return Boolean is
      use Ada.Strings.Unbounded;

      Text_Mimes: constant array(1 .. 3) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "application/x-shellscript"),
         2 => To_Unbounded_String(Source => "application/x-desktop"),
         3 => To_Unbounded_String(Source => "application/xml"));
   begin
      Check_Mime_Loop :
      for Mime of Text_Mimes loop
         if Mime_Type = To_String(Source => Mime) then
            return True;
         end if;
      end loop Check_Mime_Loop;
      if Mime_Type(Mime_Type'First .. Mime_Type'First + 3) = "text" then
         return True;
      end if;
      return False;
   end Is_Text;

   function Find_Executable
     (Name: String; Display_Message: Boolean := True) return String is
      use Ada.Command_Line;
      use GNAT.OS_Lib;
      use Messages.UI;

      Executable_Path: GNAT.OS_Lib.String_Access;
   begin
      if Exists
          (Name =>
             Containing_Directory(Name => Command_Name) & "/" & Name) then
         return Containing_Directory(Name => Command_Name) & "/" & Name;
      end if;
      Executable_Path := Locate_Exec_On_Path(Exec_Name => Name);
      if Executable_Path = null then
         if Display_Message then
            Show_Message
              (Message =>
                 Mc
                   (Interp => Interpreter,
                    Src_String => "{Could not found executable:}") &
                 " " & Name);
         end if;
         return "";
      end if;
      return Executable_Path.all;
   end Find_Executable;

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (interp => Interpreter, cmdName => Name, proc => Ada_Command,
           data => 0, deleteProc => null);
      if Command = null then
         raise Hunter_Add_Command_Exception
           with Mc
             (Interp => Interpreter, Src_String => "{Can't add command}") &
           " " & Name;
      end if;
   end Add_Command;

end Utils;
