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

with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Utils; use Utils;

package body LoadData is

   overriding function "="(Left, Right: Item_Record) return Boolean is
   begin
      return Left.Name = Right.Name;
   end "=";

   function "<"(Left, Right: Item_Record) return Boolean is
   begin
      if Left.IsDirectory and not Right.IsDirectory then
         return True;
      end if;
      if not Left.IsDirectory and Right.IsDirectory then
         return False;
      end if;
      if Left.IsHidden and not Right.IsHidden then
         return True;
      end if;
      if not Left.IsHidden and Right.IsHidden then
         return False;
      end if;
      case SortOrder is
         when NameAsc =>
            return
              Translate(Left.Name, Lower_Case_Map) <
              Translate(Right.Name, Lower_Case_Map);
         when NameDesc =>
            return
              Translate(Left.Name, Lower_Case_Map) >
              Translate(Right.Name, Lower_Case_Map);
         when ModifiedAsc =>
            return Left.Modified < Right.Modified;
         when ModifiedDesc =>
            return Left.Modified > Right.Modified;
         when SizeAsc =>
            return Left.Size < Right.Size;
         when SizeDesc =>
            return Left.Size > Right.Size;
      end case;
   end "<";

   procedure AddItem(Path: String; List: in out Items_Container.Vector) is
      FileName: constant String := Simple_Name(Path);
      Size: File_Size;
      SubDirectory: Dir_Type;
      SubLast, HiddenAmount: Natural;
      SubFileName: String(1 .. 1_024);
      MimeType: Unbounded_String;
      Item: Item_Record;
   begin
      Item.Name := To_Unbounded_String(FileName);
      Item.Path := To_Unbounded_String(Path);
      begin
         Item.Modified := Modification_Time(Path);
      exception
         when others =>
            Item.Modified := Time_Of(1_901, 1, 1);
      end;
      Item.IsHidden := (if FileName(1) = '.' then True else False);
      if Is_Directory(Path) then
         Item.IsDirectory := True;
         Item.Image :=
           (if Is_Symbolic_Link(Path) then
              To_Unbounded_String("emblem-symbolic-link")
            else To_Unbounded_String("folder"));
         Item.Size := -1;
         if Is_Read_Accessible_File(Path) then
            Open(SubDirectory, Path);
            Size := 0;
            HiddenAmount := 0;
            Count_Directory_Size :
            loop
               Read(SubDirectory, SubFileName, SubLast);
               exit Count_Directory_Size when SubLast = 0;
               if SubFileName(1 .. SubLast) /= "." and
                 SubFileName(1 .. SubLast) /= ".." then
                  if SubFileName(1) = '.' then
                     HiddenAmount := HiddenAmount + 1;
                  else
                     Size := Size + 1;
                  end if;
               end if;
            end loop Count_Directory_Size;
            Close(SubDirectory);
            Item.Size := Item_Size(Size);
            Item.HiddenItems := HiddenAmount;
         end if;
      else
         Item.IsDirectory := False;
         if Is_Symbolic_Link(Path) then
            Item.Image := To_Unbounded_String("emblem-symbolic-link");
         elsif Is_Executable_File(Path) then
            Item.Image := To_Unbounded_String("application-x-executable");
         else
            MimeType := To_Unbounded_String(Get_Mime_Type(Path));
            if Index(MimeType, "audio") > 0 then
               Item.Image := To_Unbounded_String("audio-x-generic");
            elsif Index(MimeType, "font") > 0 then
               Item.Image := To_Unbounded_String("font-x-generic");
            elsif Index(MimeType, "image") > 0 then
               Item.Image := To_Unbounded_String("image-x-generic");
            elsif Index(MimeType, "video") > 0 then
               Item.Image := To_Unbounded_String("video-x-generic");
            elsif Index(MimeType, "text/x-script") > 0 then
               Item.Image := To_Unbounded_String("text-x-script");
            elsif MimeType = To_Unbounded_String("text/html") then
               Item.Image := To_Unbounded_String("text-html");
            elsif Index(MimeType, "zip") > 0 or
              Index(MimeType, "x-xz") > 0 then
               Item.Image := To_Unbounded_String("package-x-generic");
            elsif Index(MimeType, "text") > 0 then
               Item.Image := To_Unbounded_String("text-x-generic");
            else
               Item.Image := To_Unbounded_String("text-x-generic-template");
            end if;
         end if;
         if not Is_Read_Accessible_File(Path) then
            Item.Size := -1;
            ItemsList.Append(Item);
            return;
         end if;
         Item.Size :=
           (if Is_Symbolic_Link(Path) then -2
            elsif Is_Regular_File(Path) then
              Item_Size(Ada.Directories.Size(Path))
            else 0);
      end if;
      List.Append(Item);
   end AddItem;

end LoadData;
