-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;

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

   procedure CreateBookmarkMenu(CreateNew: Boolean := False) is
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
      BookmarksMenu: Tk_Menu;
      MenuButton: Ttk_MenuButton;
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
   begin
      if CreateNew then
         BookmarksMenu := Create(".bookmarksmenu", "-tearoff false");
      else
         BookmarksMenu.Interp := Get_Context;
         BookmarksMenu.Name := New_String(".bookmarksmenu");
         Delete(BookmarksMenu, "0", "end");
      end if;
      BookmarksList.Clear;
      BookmarksList.Append
        (New_Item =>
           (MenuName => To_Unbounded_String("Home"),
            Path => To_Unbounded_String(Value("HOME"))));
      Add(BookmarksMenu, "command", "-label ""Home""");
      for I in XDGBookmarks'Range loop
         if Ada.Directories.Exists
             (To_String(GetXDGDirectory(To_String(XDGBookmarks(I).Path)))) then
            BookmarksList.Append
              (New_Item =>
                 (MenuName => XDGBookmarks(I).MenuName,
                  Path => GetXDGDirectory(To_String(XDGBookmarks(I).Path))));
            Add
              (BookmarksMenu, "command",
               "-label """ & To_String(XDGBookmarks(I).MenuName) & """");
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
                     Add
                       (BookmarksMenu, "command",
                        "-label """ & Simple_Name(To_String(Path)) & """");
                  end if;
               end if;
            end loop;
            Close(File);
         end;
      end if;
      BookmarksList.Append
        (New_Item =>
           (MenuName => To_Unbounded_String("Enter destination"),
            Path => Null_Unbounded_String));
      Add(BookmarksMenu, "command", "-label ""Enter destination""");
      MenuButton.Interp := BookmarksMenu.Interp;
      MenuButton.Name :=
        New_String(".mainframe.toolbars.actiontoolbar.bookmarksbutton");
      configure(MenuButton, "-menu .bookmarksmenu");
   end CreateBookmarkMenu;

   -- ****if* Bookmarks/UpdateView
   -- FUNCTION
   -- Updated current directory listing after move to bookmark
   -- SOURCE
--   procedure UpdateView is
--   -- ****
--   begin
--      if not Is_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 1))) then
--         Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 2)));
--         Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 1)));
--         Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 4)));
--         TemporaryStop := False;
--      end if;
--      if Is_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10))) then
--         ToggleToolButtons(NewAction, True);
--         Set_Title
--           (Get_Column
--              (Gtk_Tree_View
--                 (Get_Child
--                    (Gtk_Scrolled_Window
--                       (Get_Child(Gtk_Box(Get_Child1(FilesPaned)), 2)))),
--               2),
--            Gettext("Modified"));
--         SetDeleteTooltip;
--      end if;
--      if Ada.Directories.Exists(To_String(CurrentDirectory)) then
--         if Get_Visible_Child_Name(InfoStack) = "destination" then
--            LoadDirectory(To_String(CurrentDirectory), "fileslist2");
--         else
--            Reload;
--            UpdateWatch(To_String(CurrentDirectory));
--         end if;
--      end if;
--   end UpdateView;

   -- ****if* Bookmarks/GoToBookmark
   -- FUNCTION
   -- Go to selected bookmark location
   -- PARAMETERS
   -- Self - Selected entry in bookmarks menu
   -- SOURCE
--   procedure GoToBookmark(Self: access Gtk_Menu_Item_Record'Class) is
---- ****
--      MenuLabel: constant Unbounded_String :=
--        To_Unbounded_String(Get_Label(Self));
--   begin
--      if NewAction /= MOVE then
--         NewAction := COPY;
--      end if;
--      for I in BookmarksList.Iterate loop
--         if MenuLabel = BookmarksList(I).MenuName then
--            if BookmarksList(I).Path /= Null_Unbounded_String then
--               CurrentDirectory := BookmarksList(I).Path;
--            else
--               NewAction := GOTOPATH;
--               Set_Icon_Tooltip_Text
--                 (TextEntry, Gtk_Entry_Icon_Secondary,
--                  Gettext("Go to selected destination."));
--               Set_Text(TextEntry, To_String(CurrentDirectory));
--               Show_All(TextEntry);
--            end if;
--            exit;
--         end if;
--      end loop;
--      UpdateView;
--      if Is_Visible(TextEntry) then
--         Grab_Focus(TextEntry);
--      end if;
--   end GoToBookmark;

--   procedure GoHome(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--   begin
--      if NewAction /= MOVE then
--         NewAction := COPY;
--      end if;
--      CurrentDirectory := To_Unbounded_String(Value("HOME"));
--      UpdateView;
--   end GoHome;
--
--   -- ****if* Bookmarks/RemoveMenu
--   -- FUNCTION
--   -- Remove selected menu item from menu
--   -- PARAMETERS
--   -- Widget - GTK Widget to remove
--   -- SOURCE
--   procedure RemoveMenu
--     (Widget: not null access Gtk.Widget.Gtk_Widget_Record'Class) is
---- ****
--   begin
--      Destroy(Widget);
--   end RemoveMenu;

   -- ****if* Bookmarks/AddBookmark
   -- FUNCTION
   -- Add bookmark to currently selected directory
   -- PARAMETERS
   -- Self - Gtk_Tool_Button clicked. Unused. Can be null
   -- SOURCE
--   procedure AddBookmark(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--      File: File_Type;
--   begin
--      Open(File, Append_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
--      Put_Line(File, "file://" & To_String(CurrentSelected));
--      Close(File);
--      CreateBookmarkMenu;
--      SetBookmarkButton;
--   end AddBookmark;
--
--   -- ****if* Bookmarks/RemoveBookmark
--   -- FUNCTION
--   -- Remove bookmark for currently selected directory
--   -- PARAMETERS
--   -- Self - Gtk_Tool_Button clicked. Unused. Can be null
--   -- SOURCE
--   procedure RemoveBookmark(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--      NewFile, OldFile: File_Type;
--      Line, Path: Unbounded_String;
--   begin
--      Rename
--        (Value("HOME") & "/.config/gtk-3.0/bookmarks",
--         Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
--      Open(OldFile, In_File, Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
--      Create(NewFile, Out_File, Value("HOME") & "/.config/gtk-3.0/bookmarks");
--      while not End_Of_File(OldFile) loop
--         Line := To_Unbounded_String(Get_Line(OldFile));
--         if Slice(Line, 1, 7) = "file://" then
--            Path := Unbounded_Slice(Line, 8, Length(Line));
--            if Path /= CurrentSelected then
--               Put_Line(NewFile, To_String(Line));
--            end if;
--         end if;
--      end loop;
--      Close(NewFile);
--      Close(OldFile);
--      Delete_File(Value("HOME") & "/.config/gtk-3.0/bookmarks.old");
--      CreateBookmarkMenu;
--      SetBookmarkButton;
--   end RemoveBookmark;
--
--   procedure SetBookmarkButton is
--   begin
--      Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 7)));
--      Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 8)));
--      if not Is_Directory(To_String(CurrentSelected)) then
--         return;
--      end if;
--      for Bookmark of BookmarksList loop
--         if Bookmark.Path = CurrentSelected then
--            Show_All(Gtk_Widget(Get_Nth_Item(ItemToolBar, 8)));
--            return;
--         end if;
--      end loop;
--      Show_All(Gtk_Widget(Get_Nth_Item(ItemToolBar, 7)));
--   end SetBookmarkButton;
--
--   procedure CreateBookmarksUI is
--   begin
--      On_Clicked
--        (Gtk_Tool_Button(Get_Nth_Item(ItemToolBar, 7)), AddBookmark'Access);
--      On_Clicked
--        (Gtk_Tool_Button(Get_Nth_Item(ItemToolBar, 8)), RemoveBookmark'Access);
--      On_Clicked
--        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 0)), GoHome'Access);
--   end CreateBookmarksUI;

end Bookmarks;
