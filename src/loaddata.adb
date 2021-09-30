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

with Ada.Strings.Maps.Constants;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Utils;

package body LoadData is

   overriding function "="(Left, Right: Item_Record) return Boolean is
   begin
      return Left.Name = Right.Name;
   end "=";

   function "<"(Left, Right: Item_Record) return Boolean is
      use Ada.Strings.Maps.Constants;

   begin
      if Left.Is_Directory and not Right.Is_Directory then
         return True;
      end if;
      if not Left.Is_Directory and Right.Is_Directory then
         return False;
      end if;
      if Left.Is_Hidden and not Right.Is_Hidden then
         return True;
      end if;
      if not Left.Is_Hidden and Right.Is_Hidden then
         return False;
      end if;
      case Sort_Order is
         when NAMEASC =>
            return
              Translate(Source => Left.Name, Mapping => Lower_Case_Map) <
              Translate(Source => Right.Name, Mapping => Lower_Case_Map);
         when NAMEDESC =>
            return
              Translate(Source => Left.Name, Mapping => Lower_Case_Map) >
              Translate(Source => Right.Name, Mapping => Lower_Case_Map);
         when MODIFIEDASC =>
            return Left.Modified < Right.Modified;
         when MODIFIEDDESC =>
            return Left.Modified > Right.Modified;
         when SIZEASC =>
            return Left.Size < Right.Size;
         when SIZEDESC =>
            return Left.Size > Right.Size;
      end case;
   end "<";

   procedure Add_Item(Path: String; List: in out Items_Container.Vector) is
      use GNAT.OS_Lib;
      use Utils;

      File_Name: constant String := Simple_Name(Name => Path);
      Size: File_Size := 0;
      Sub_Directory: Dir_Type;
      Sub_Last, Hidden_Amount: Natural := 0;
      Sub_File_Name: String(1 .. 1_024) := (others => ' ');
      Mime_Type: Unbounded_String := Null_Unbounded_String;
      Item: Item_Record := Empty_Item;
   begin
      Item.Name := To_Unbounded_String(Source => File_Name);
      Item.Path := To_Unbounded_String(Source => Path);
      Set_Item_Modified_Time_Block :
      begin
         Item.Modified := Modification_Time(Name => Path);
      exception
         when others =>
            Item.Modified := Time_Of(Year => 1_901, Month => 1, Day => 1);
      end Set_Item_Modified_Time_Block;
      Item.Is_Hidden := (if File_Name(1) = '.' then True else False);
      if Is_Directory(Name => Path) then
         Item.Is_Directory := True;
         Item.Image :=
           (if Is_Symbolic_Link(Name => Path) then
              To_Unbounded_String(Source => "emblem-symbolic-link")
            else To_Unbounded_String(Source => "folder"));
         Item.Size := -1;
         if Is_Read_Accessible_File(Name => Path) then
            Open(Dir => Sub_Directory, Dir_Name => Path);
            Size := 0;
            Hidden_Amount := 0;
            Count_Directory_Size_Loop :
            loop
               Read
                 (Dir => Sub_Directory, Str => Sub_File_Name,
                  Last => Sub_Last);
               exit Count_Directory_Size_Loop when Sub_Last = 0;
               if Sub_File_Name(1 .. Sub_Last) /= "." and
                 Sub_File_Name(1 .. Sub_Last) /= ".." then
                  if Sub_File_Name(1) = '.' then
                     Hidden_Amount := Hidden_Amount + 1;
                  else
                     Size := Size + 1;
                  end if;
               end if;
            end loop Count_Directory_Size_Loop;
            Close(Dir => Sub_Directory);
            Item.Size := Item_Size(Size);
            Item.Hidden_Items := Hidden_Amount;
         end if;
      else
         Item.Is_Directory := False;
         if Is_Symbolic_Link(Name => Path) then
            Item.Image :=
              To_Unbounded_String(Source => "emblem-symbolic-link");
         elsif Is_Executable_File(Name => Path) then
            Item.Image :=
              To_Unbounded_String(Source => "application-x-executable");
         else
            Mime_Type :=
              To_Unbounded_String(Source => Get_Mime_Type(File_Name => Path));
            if Index(Source => Mime_Type, Pattern => "audio") > 0 then
               Item.Image := To_Unbounded_String(Source => "audio-x-generic");
            elsif Index(Source => Mime_Type, Pattern => "font") > 0 then
               Item.Image := To_Unbounded_String(Source => "font-x-generic");
            elsif Index(Source => Mime_Type, Pattern => "image") > 0 then
               Item.Image := To_Unbounded_String(Source => "image-x-generic");
            elsif Index(Source => Mime_Type, Pattern => "video") > 0 then
               Item.Image := To_Unbounded_String(Source => "video-x-generic");
            elsif Index(Source => Mime_Type, Pattern => "text/x-script") >
              0 then
               Item.Image := To_Unbounded_String(Source => "text-x-script");
            elsif Mime_Type = To_Unbounded_String(Source => "text/html") then
               Item.Image := To_Unbounded_String(Source => "text-html");
            elsif Index(Source => Mime_Type, Pattern => "zip") > 0 or
              Index(Source => Mime_Type, Pattern => "x-xz") > 0 then
               Item.Image :=
                 To_Unbounded_String(Source => "package-x-generic");
            elsif Index(Source => Mime_Type, Pattern => "text") > 0 then
               Item.Image := To_Unbounded_String(Source => "text-x-generic");
            else
               Item.Image :=
                 To_Unbounded_String(Source => "text-x-generic-template");
            end if;
         end if;
         if not Is_Read_Accessible_File(Name => Path) then
            Item.Size := -1;
            Items_List.Append(New_Item => Item);
            return;
         end if;
         Item.Size :=
           (if Is_Symbolic_Link(Name => Path) then -2
            elsif Is_Regular_File(Name => Path) then
              Item_Size(Ada.Directories.Size(Name => Path))
            else 0);
      end if;
      List.Append(New_Item => Item);
   end Add_Item;

   procedure Load_Selected_Directory
     (Directory_Name: String; Second: Boolean) is
      Directory: Dir_Type;
      File_Name: String(1 .. 1_024) := (others => ' ');
      Last: Natural range 0 .. File_Name'Last := 0;
   begin
      Open(Dir => Directory, Dir_Name => Directory_Name);
      Read_Directory_Loop :
      loop
         Read(Dir => Directory, Str => File_Name, Last => Last);
         exit Read_Directory_Loop when Last = 0;
         if File_Name(1 .. Last) /= "." and File_Name(1 .. Last) /= ".." then
            if Second then
               Add_Item
                 (Path => Directory_Name & "/" & File_Name(1 .. Last),
                  List => Second_Items_List);
            else
               Add_Item
                 (Path => Directory_Name & "/" & File_Name(1 .. Last),
                  List => Items_List);
            end if;
         end if;
      end loop Read_Directory_Loop;
      Close(Dir => Directory);
   end Load_Selected_Directory;

end LoadData;
