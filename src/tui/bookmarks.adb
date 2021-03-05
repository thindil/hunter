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
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData.UI; use LoadData.UI;
with Modules; use Modules;
with RefreshData; use RefreshData;

package body Bookmarks is

   -- ****iv* BookmarksTUI/BookmarksTUI.BookmarksList
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   BookmarksList: Bookmarks_Container.Map;
   -- ****

   procedure Create_Bookmarks_List is
      XDGBookmarks: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("XDG_DESKTOP_DIR"),
         To_Unbounded_String("XDG_DOWNLOAD_DIR"),
         To_Unbounded_String("XDG_PUBLICSHARE_DIR"),
         To_Unbounded_String("XDG_DOCUMENTS_DIR"),
         To_Unbounded_String("XDG_MUSIC_DIR"),
         To_Unbounded_String("XDG_PICTURES_DIR"),
         To_Unbounded_String("XDG_VIDEOS_DIR"));
      Path: Unbounded_String;
      function GetXDGDirectory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String;
         EqualIndex: Natural;
      begin
         if Value(Name, "") = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            Load_Bookmarks_Loop :
            while not End_Of_File(File) loop
               Line := Get_Line(File);
               EqualIndex := Index(Line, "=");
               if EqualIndex > 0 then
                  if Slice(Line, 1, EqualIndex - 1) = Name then
                     Set(Name, Slice(Line, EqualIndex + 2, Length(Line) - 1));
                     exit Load_Bookmarks_Loop;
                  end if;
               end if;
            end loop Load_Bookmarks_Loop;
            Close(File);
         end if;
         return To_Unbounded_String(Expand_Path(Value(Name)));
      end GetXDGDirectory;
   begin
      Set_XDGBookmarks_List_Loop :
      for I in XDGBookmarks'Range loop
         Path := GetXDGDirectory(To_String(XDGBookmarks(I)));
         if Ada.Directories.Exists(To_String(Path)) then
            BookmarksList.Include
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
               for I in BookmarksList.Iterate loop
                  if BookmarksList(I) = To_String(Path) then
                     BookmarkExist := True;
                     exit Check_Bookmark_Existence_Loop;
                  end if;
               end loop Check_Bookmark_Existence_Loop;
               if not BookmarkExist and
                 Ada.Directories.Exists(To_String(Path)) then
                  BookmarksList.Include
                    (Simple_Name(To_String(Path)), To_String(Path));
               end if;
               <<End_Of_Loop>>
            end loop Load_User_Bookmarks_Loop;
            Close(File);
         end;
      end if;
   end Create_Bookmarks_List;

   function Show_Bookmarks_Menu return Item_Array_Access is
      Menu_Items: Item_Array_Access;
      MenuIndex: Positive := 1;
   begin
      Menu_Items := new Item_Array(1 .. Positive(BookmarksList.Length) + 4);
      Menu_Items.all(MenuIndex) := New_Item(Mc(Interpreter, "{Home}"));
      MenuIndex := MenuIndex + 1;
      for I in BookmarksList.Iterate loop
         Menu_Items.all(MenuIndex) := New_Item(Bookmarks_Container.Key(I));
         MenuIndex := MenuIndex + 1;
      end loop;
      Menu_Items.all(MenuIndex) := New_Item("Enter destination");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := New_Item("Close");
      MenuIndex := MenuIndex + 1;
      Menu_Items.all(MenuIndex) := Null_Item;
      return Menu_Items;
   end Show_Bookmarks_Menu;

   function Go_To_Bookmark(Bookmark: String) return UI_Locations is
   begin
      if Bookmark = "Cancel" then
         return DIRECTORY_VIEW;
      end if;
      if BookmarksList.Contains(Bookmark) then
         CurrentDirectory := To_Unbounded_String(BookmarksList(Bookmark));
      elsif Bookmark = Mc(Interpreter, "{Home}") then
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      UpdateWatch(To_String(CurrentDirectory));
      Execute_Modules(On_Enter, "{" & To_String(CurrentDirectory) & "}");
      return DIRECTORY_VIEW;
   end Go_To_Bookmark;

end Bookmarks;
