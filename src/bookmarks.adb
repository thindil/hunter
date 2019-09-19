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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Container; use Gtk.Container;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Shell; use Gtk.Menu_Shell;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Intl; use Gtkada.Intl;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with Utils; use Utils;

package body Bookmarks is

   -- ****it* Bookmarks/Bookmark_Record
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

   -- ****it* Bookmarks/Bookmarks_Container
   -- FUNCTION
   -- Used to store all bookmarks
   -- SOURCE
   package Bookmarks_Container is new Vectors(Positive, Bookmark_Record);
   -- ****

   -- ****iv* Bookmarks/BookmarksList
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   BookmarksList: Bookmarks_Container.Vector;
   -- ****

   -- ****if* Bookmarks/UpdateView
   -- FUNCTION
   -- Updated current directory listing after move to bookmark
   -- SOURCE
   procedure UpdateView is
   -- ****
   begin
      if not Is_Visible(Gtk_Widget(Get_Object(Builder, "btnsearch"))) then
         Show_All(Gtk_Widget(Get_Object(Builder, "btnselectall")));
         Show_All(Gtk_Widget(Get_Object(Builder, "btnsearch")));
         Show_All(Gtk_Widget(Get_Object(Builder, "btnnew")));
         TemporaryStop := False;
      end if;
      if Is_Visible(Gtk_Widget(Get_Object(Builder, "btntoolrestore"))) then
         ToggleToolButtons(NewAction, True);
         Set_Title
           (Gtk_Tree_View_Column(Get_Object(Builder, "modifiedcolumn")),
            Gettext("Modified"));
         SetDeleteTooltip;
      end if;
      if Ada.Directories.Exists(To_String(CurrentDirectory)) then
         if Get_Visible_Child_Name
             (Gtk_Stack(Get_Object(Builder, "infostack"))) =
           "destination" then
            LoadDirectory(To_String(CurrentDirectory), "fileslist2");
         else
            Reload(Builder);
         end if;
      end if;
   end UpdateView;

   -- ****if* Bookmarks/GoToBookmark
   -- FUNCTION
   -- Go to selected bookmark location
   -- PARAMETERS
   -- Self - Selected entry in bookmarks menu
   -- SOURCE
   procedure GoToBookmark(Self: access Gtk_Menu_Item_Record'Class) is
-- ****
      MenuLabel: constant Unbounded_String :=
        To_Unbounded_String(Get_Label(Self));
      GEntry: constant Gtk_Widget := Gtk_Widget(Get_Object(Builder, "entry"));
   begin
      if NewAction /= MOVE then
         NewAction := COPY;
      end if;
      for I in BookmarksList.Iterate loop
         if MenuLabel = BookmarksList(I).MenuName then
            if BookmarksList(I).Path /= Null_Unbounded_String then
               CurrentDirectory := BookmarksList(I).Path;
            else
               NewAction := GOTOPATH;
               Set_Icon_Tooltip_Text
                 (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary,
                  Gettext("Go to selected destination."));
               Set_Text(Gtk_GEntry(GEntry), To_String(CurrentDirectory));
               Show_All(GEntry);
               Grab_Focus(GEntry);
            end if;
            exit;
         end if;
      end loop;
      UpdateView;
   end GoToBookmark;

   procedure GoHome(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      if NewAction /= MOVE then
         NewAction := COPY;
      end if;
      CurrentDirectory := To_Unbounded_String(Value("HOME"));
      UpdateView;
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
        ((To_Unbounded_String(Gettext("Desktop")),
          To_Unbounded_String("XDG_DESKTOP_DIR")),
         (To_Unbounded_String(Gettext("Download")),
          To_Unbounded_String("XDG_DOWNLOAD_DIR")),
         (To_Unbounded_String(Gettext("Public")),
          To_Unbounded_String("XDG_PUBLICSHARE_DIR")),
         (To_Unbounded_String(Gettext("Documents")),
          To_Unbounded_String("XDG_DOCUMENTS_DIR")),
         (To_Unbounded_String(Gettext("Music")),
          To_Unbounded_String("XDG_MUSIC_DIR")),
         (To_Unbounded_String(Gettext("Pictures")),
          To_Unbounded_String("XDG_PICTURES_DIR")),
         (To_Unbounded_String(Gettext("Videos")),
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
      BookmarksList.Append
        (New_Item =>
           (MenuName => To_Unbounded_String(Gettext("Enter destination")),
            Path => Null_Unbounded_String));
      AddMenuItem;
   end CreateBookmarkMenu;

   -- ****if* Bookmarks/AddBookmark
   -- FUNCTION
   -- Add bookmark to currently selected directory
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure AddBookmark(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      File: File_Type;
   begin
      Open(File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      Put_Line(File, "file://" & To_String(CurrentSelected));
      Close(File);
      CreateBookmarkMenu(Object);
      Reload(Object);
   end AddBookmark;

   -- ****if* Bookmarks/RemoveBookmark
   -- FUNCTION
   -- Remove bookmark for currently selected directory
   -- SOURCE
   procedure RemoveBookmark(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      NewFile, OldFile: File_Type;
      Line, Path: Unbounded_String;
   begin
      Rename
        (Value("HOME") & "/.config/gtk-3.0/bookmarks",
         Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Open(OldFile, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      Create(NewFile, Out_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
      while not End_Of_File(OldFile) loop
         Line := To_Unbounded_String(Get_Line(OldFile));
         if Slice(Line, 1, 7) = "file://" then
            Path := Unbounded_Slice(Line, 8, Length(Line));
            if Path /= CurrentSelected then
               Put_Line(NewFile, To_String(Line));
            end if;
         end if;
      end loop;
      Close(NewFile);
      Close(OldFile);
      Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
      CreateBookmarkMenu(Object);
      Reload(Object);
   end RemoveBookmark;

   procedure SetBookmarkButton is
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnaddbookmark")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnremovebookmark")));
      if not Is_Directory(To_String(CurrentSelected)) then
         return;
      end if;
      for Bookmark of BookmarksList loop
         if Bookmark.Path = CurrentSelected then
            Show_All(Gtk_Widget(Get_Object(Builder, "btnremovebookmark")));
            return;
         end if;
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "btnaddbookmark")));
   end SetBookmarkButton;

   procedure CreateBookmarksUI is
   begin
      Register_Handler(Builder, "Go_Home", GoHome'Access);
      Register_Handler(Builder, "Add_Bookmark", AddBookmark'Access);
      Register_Handler(Builder, "Remove_Bookmark", RemoveBookmark'Access);
      Register_Handler
        (Builder, "Create_Bookmark_Menu", CreateBookmarkMenu'Access);
   end CreateBookmarksUI;

end Bookmarks;
