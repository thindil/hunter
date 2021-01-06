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

   function GetMimeType(FileName: String) return String is
   begin
      return MagicFile(FileName);
   end GetMimeType;

   function CanBeOpened(MimeType: String) return Boolean is
      ExecutableName: constant String := FindExecutable("xdg-mime");
      Success: Boolean;
   begin
      if ExecutableName = "" then
         return False;
      end if;
      Spawn
        (ExecutableName,
         Argument_String_To_List("query default " & MimeType).all, Success);
      if not Success then
         return False;
      end if;
      return True;
   end CanBeOpened;

   function CountFileSize(Size: File_Size) return String is
      Multiplier: Natural range 0 .. 8;
      NewSize: File_Size;
      SizeShortcuts: constant array(Natural range 0 .. 8) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
   begin
      NewSize := Size;
      Multiplier := 0;
      Count_Size_Loop :
      while NewSize > 1024 loop
         exit Count_Size_Loop when Multiplier = 8;
         NewSize := NewSize / 1024;
         Multiplier := Multiplier + 1;
      end loop Count_Size_Loop;
      return File_Size'Image(NewSize) & " " & SizeShortcuts(Multiplier);
   end CountFileSize;

end Utils;
