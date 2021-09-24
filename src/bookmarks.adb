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
         if Value(Name => Name, Default => "") = "" then
            Open
              (File => File, Mode => In_File,
               Name => Value(Name => "HOME") & "/.config/user-dirs.dirs");
            Load_Bookmarks_Loop :
            while not End_Of_File(File => File) loop
               Line := Get_Line(File => File);
               Equal_Index := Index(Source => Line, Pattern => "=");
               if Equal_Index > 0 then
                  if Slice(Source => Line, Low => 1, High => Equal_Index - 1) =
                    Name then
                     Set
                       (Name => Name,
                        Value =>
                          Slice
                            (Source => Line, Low => Equal_Index + 2,
                             High => Length(Source => Line) - 1));
                     exit Load_Bookmarks_Loop;
                  end if;
               end if;
            end loop Load_Bookmarks_Loop;
            Close(File => File);
         end if;
         return
           To_Unbounded_String
             (Source => Expand_Path(Path => Value(Name => Name)));
      end Get_Xdg_Directory;
   begin
      Bookmarks_List.Clear;
      Set_XDGBookmarks_List_Loop :
      for Xdg_Bookmark of Xdg_Bookmarks loop
         Path := Get_Xdg_Directory(Name => To_String(Source => Xdg_Bookmark));
         if Ada.Directories.Exists(Name => To_String(Source => Path)) then
            Bookmarks_List.Include
              (Key => Simple_Name(Name => To_String(Source => Path)),
               New_Item => To_String(Source => Path));
         end if;
      end loop Set_XDGBookmarks_List_Loop;
      if Ada.Directories.Exists
          (Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks") then
         Load_User_Bookmarks_Block :
         declare
            File: File_Type;
            Line, User_Path: Unbounded_String := Null_Unbounded_String;
            Bookmark_Exist: Boolean := False;
         begin
            Open
              (File => File, Mode => In_File,
               Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks");
            Load_User_Bookmarks_Loop :
            while not End_Of_File(File => File) loop
               Line := Get_Line(File => File);
               if Length(Source => Line) < 7
                 or else Slice(Source => Line, Low => 1, High => 7) /=
                   "file://" then
                  goto End_Of_Loop;
               end if;
               User_Path :=
                 Unbounded_Slice
                   (Source => Line, Low => 8, High => Length(Source => Line));
               Bookmark_Exist := False;
               Check_Bookmark_Existence_Loop :
               for I in Bookmarks_List.Iterate loop
                  if Bookmarks_List(I) = To_String(Source => User_Path) then
                     Bookmark_Exist := True;
                     exit Check_Bookmark_Existence_Loop;
                  end if;
               end loop Check_Bookmark_Existence_Loop;
               if not Bookmark_Exist and
                 Ada.Directories.Exists
                   (Name => To_String(Source => User_Path)) then
                  Bookmarks_List.Include
                    (Key =>
                       Simple_Name(Name => To_String(Source => User_Path)),
                     New_Item => To_String(Source => User_Path));
               end if;
               <<End_Of_Loop>>
            end loop Load_User_Bookmarks_Loop;
            Close(File => File);
         end Load_User_Bookmarks_Block;
      end if;
   end Fill_Bookmarks_List;

end Bookmarks;
