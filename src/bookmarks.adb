-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Container; use Gtk.Container;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Widget; use Gtk.Widget;
with MainWindow; use MainWindow;

package body Bookmarks is

   -- ****t* Bookmarks/Bookmark_Record
   -- FUNCTION
   -- Data structure for bookmarks
   -- PARAMETERS
   -- MenuName - Text visible to user in menu for this bookmark
   -- Path     - Full path to this bookmark location
   -- SOURCE
   type Bookmark_Record is record
      MenuName: Unbounded_String;
      Path: Unbounded_String;
   end record;
   -- ****
   -- ****t* Bookmarks/Bookmarks_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Bookmarks_Container is new Vectors(Positive, Bookmark_Record);
   -- ****

   -- ****v* Bookmarks/BookmarksList
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   BookmarksList: Bookmarks_Container.Vector;
   -- ****

   -- ****f* Bookmarks/GoToBookmark
   -- FUNCTION
   -- Go to selected bookmark location
   -- PARAMETERS
   -- Self - Selected entry in bookmarks menu
   -- SOURCE
   procedure GoToBookmark(Self: access Gtk_Menu_Item_Record'Class) is
      MenuLabel: constant Unbounded_String :=
        To_Unbounded_String(Get_Label(Self));
-- ****
   begin
      for I in BookmarksList.Iterate loop
         if MenuLabel = BookmarksList(I).MenuName then
            CurrentDirectory := BookmarksList(I).Path;
            exit;
         end if;
      end loop;
      if Ada.Directories.Exists(To_String(CurrentDirectory)) then
         Reload(Builder);
      end if;
   end GoToBookmark;

   procedure GoHome(Object: access Gtkada_Builder_Record'Class) is
   begin
      CurrentDirectory := To_Unbounded_String(Value("HOME"));
      if Ada.Directories.Exists(To_String(CurrentDirectory)) then
         Reload(Object);
      end if;
   end GoHome;

   -- ****if* Bookmarks/RemoveMenu
   -- FUNCTION
   -- Remove selected menu item from menu
   -- PARAMETERS
   -- Widget - GTK Widget to remove
   -- SOURCE
   procedure RemoveMenu
     (Widget: not null access Gtk.Widget.Gtk_Widget_Record'Class) is
-- ****
   begin
      Destroy(Widget);
   end RemoveMenu;

   procedure CreateBookmarkMenu(Object: access Gtkada_Builder_Record'Class) is
      XDGBookmarks: constant array(Positive range <>) of Bookmark_Record :=
        ((To_Unbounded_String("Desktop"),
          To_Unbounded_String("XDG_DESKTOP_DIR")),
         (To_Unbounded_String("Download"),
          To_Unbounded_String("XDG_DOWNLOAD_DIR")),
         (To_Unbounded_String("Public"),
          To_Unbounded_String("XDG_PUBLICSHARE_DIR")),
         (To_Unbounded_String("Documents"),
          To_Unbounded_String("XDG_DOCUMENTS_DIR")),
         (To_Unbounded_String("Music"), To_Unbounded_String("XDG_MUSIC_DIR")),
         (To_Unbounded_String("Pictures"),
          To_Unbounded_String("XDG_PICTURES_DIR")),
         (To_Unbounded_String("Videos"),
          To_Unbounded_String("XDG_VIDEOS_DIR")));
      function GetXDGDirectory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String;
         EqualIndex: Natural;
      begin
         if not Ada.Environment_Variables.Exists(Name)
           or else Value(Name) = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            while not End_Of_File(File) loop
               Line := To_Unbounded_String(Get_Line(File));
               EqualIndex := Index(Line, "=");
               if EqualIndex > 0 then
                  if Slice(Line, 1, EqualIndex - 1) = Name then
                     Set(Name, Slice(Line, EqualIndex + 2, Length(Line) - 1));
                     exit;
                  end if;
               end if;
            end loop;
            Close(File);
         end if;
         return To_Unbounded_String(Expand_Path(Value(Name)));
      end GetXDGDirectory;
      procedure AddMenuItem is
         MenuItem: Gtk_Menu_Item;
      begin
         MenuItem :=
           Gtk_Menu_Item_New_With_Label
             (To_String(BookmarksList(BookmarksList.Last_Index).MenuName));
         On_Activate(MenuItem, GoToBookmark'Access);
         Append(Gtk_Menu_Shell(Get_Object(Object, "bookmarksmenu")), MenuItem);
         Show_All(Gtk_Widget(MenuItem));
      end AddMenuItem;
   begin
      Foreach
        (Gtk_Container(Get_Object(Object, "bookmarksmenu")),
         RemoveMenu'Access);
      BookmarksList.Clear;
      for I in XDGBookmarks'Range loop
         if Ada.Directories.Exists
             (To_String(GetXDGDirectory(To_String(XDGBookmarks(I).Path)))) then
            BookmarksList.Append
              (New_Item =>
                 (MenuName => XDGBookmarks(I).MenuName,
                  Path => GetXDGDirectory(To_String(XDGBookmarks(I).Path))));
            AddMenuItem;
         end if;
      end loop;
      if Ada.Directories.Exists
          (Value("HOME") & "/.config/gtk-3.0/bookmarks") then
         declare
            File: File_Type;
            Line, Path: Unbounded_String;
            BookmarkExist: Boolean;
         begin
            Open(File, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
            while not End_Of_File(File) loop
               Line := To_Unbounded_String(Get_Line(File));
               if Slice(Line, 1, 7) = "file://" then
                  Path := Unbounded_Slice(Line, 8, Length(Line));
                  BookmarkExist := False;
                  for I in BookmarksList.Iterate loop
                     if BookmarksList(I).Path = Path then
                        BookmarkExist := True;
                        exit;
                     end if;
                  end loop;
                  if not BookmarkExist and
                    Ada.Directories.Exists(To_String(Path)) then
                     BookmarksList.Append
                       (New_Item =>
                          (MenuName =>
                             To_Unbounded_String(Simple_Name(To_String(Path))),
                           Path => Path));
                     AddMenuItem;
                  end if;
               end if;
            end loop;
            Close(File);
         end;
      end if;
   end CreateBookmarkMenu;

end Bookmarks;
