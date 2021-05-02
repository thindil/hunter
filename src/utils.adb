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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with LibMagic; use LibMagic;
with Utils.UI; use Utils.UI;

package body Utils is

   function Get_Mime_Type(File_Name: String) return String is
   begin
      return Magic_File(File_Name);
   end Get_Mime_Type;

   function Can_Be_Opened(Mime_Type: String) return Boolean is
      Executable_Name: constant String := FindExecutable("xdg-mime");
      Return_Code: Integer;
      Output_File: File_Descriptor;
   begin
      if Executable_Name = "" then
         return False;
      end if;
      Output_File := Open_Append("/dev/null", Text);
      Spawn
        (Executable_Name,
         Argument_String_To_List("query default " & Mime_Type).all,
         Output_File, Return_Code, True);
      Close(Output_File);
      if Return_Code /= 0 then
         return False;
      end if;
      return True;
   end Can_Be_Opened;

   function Count_File_Size(Size: File_Size) return String is
      Multiplier: Natural range 0 .. 8;
      New_Size: File_Size;
      Size_Shortcuts: constant array(Natural range 0 .. 8) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
   begin
      New_Size := Size;
      Multiplier := 0;
      Count_Size_Loop :
      while New_Size > 1024 loop
         exit Count_Size_Loop when Multiplier = 8;
         New_Size := New_Size / 1024;
         Multiplier := Multiplier + 1;
      end loop Count_Size_Loop;
      return File_Size'Image(New_Size) & " " & Size_Shortcuts(Multiplier);
   end Count_File_Size;

end Utils;
