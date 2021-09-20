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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Bookmarks is

   procedure Fill_Bookmarks_List is
      Xdg_Bookmarks: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "XDG_DESKTOP_DIR"),
         2 => To_Unbounded_String(Source => "XDG_DOWNLOAD_DIR"),
         3 => To_Unbounded_String(Source => "XDG_PUBLICSHARE_DIR"),
         4 => To_Unbounded_String(Source => "XDG_DOCUMENTS_DIR"),
         5 => To_Unbounded_String(Source => "XDG_MUSIC_DIR"),
         6 => To_Unbounded_String(Source => "XDG_PICTURES_DIR"),
         7 => To_Unbounded_String(Source => "XDG_VIDEOS_DIR"));
      Path: Unbounded_String := Null_Unbounded_String;
      function Get_Xdg_Directory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String := Null_Unbounded_String;
         Equal_Index: Natural := 0;
      begin
         if Value(Name, "") = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            Load_Bookmarks_Loop :
            while not End_Of_File(File) loop
               Line := Get_Line(File);
               Equal_Index := Index(Line, "=");
               if Equal_Index > 0 then
                  if Slice(Line, 1, Equal_Index - 1) = Name then
                     Set(Name, Slice(Line, Equal_Index + 2, Length(Line) - 1));
                     exit Load_Bookmarks_Loop;
                  end if;
               end if;
            end loop Load_Bookmarks_Loop;
            Close(File);
         end if;
         return To_Unbounded_String(Expand_Path(Value(Name)));
      end Get_Xdg_Directory;
   begin
      Bookmarks_List.Clear;
      Set_XDGBookmarks_List_Loop :
      for I in Xdg_Bookmarks'Range loop
         Path := Get_Xdg_Directory(To_String(Xdg_Bookmarks(I)));
         if Ada.Directories.Exists(To_String(Path)) then
            Bookmarks_List.Include
              (Simple_Name(To_String(Path)), To_String(Path));
         end if;
      end loop Set_XDGBookmarks_List_Loop;
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         declare
            File: File_Type;
            Line, Path: Unbounded_String;
            BookmarkExist: Boolean;
         begin
            Open(File, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
            Load_User_Bookmarks_Loop :
            while not End_Of_File(File) loop
               Line := Get_Line(File);
               if Length(Line) < 7 or else Slice(Line, 1, 7) /= "file://" then
                  goto End_Of_Loop;
               end if;
               Path := Unbounded_Slice(Line, 8, Length(Line));
               BookmarkExist := False;
               Check_Bookmark_Existence_Loop :
               for I in Bookmarks_List.Iterate loop
                  if Bookmarks_List(I) = To_String(Path) then
                     BookmarkExist := True;
                     exit Check_Bookmark_Existence_Loop;
                  end if;
               end loop Check_Bookmark_Existence_Loop;
               if not BookmarkExist and
                 Ada.Directories.Exists(To_String(Path)) then
                  Bookmarks_List.Include
                    (Simple_Name(To_String(Path)), To_String(Path));
               end if;
               <<End_Of_Loop>>
            end loop Load_User_Bookmarks_Loop;
            Close(File);
         end;
      end if;
   end Fill_Bookmarks_List;

end Bookmarks;
