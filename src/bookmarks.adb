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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Bookmarks.Commands; use Bookmarks.Commands;
with MainWindow; use MainWindow;

package body Bookmarks is

   -- ****iv* Bookmarks/BookmarksList
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   BookmarksList: Bookmarks_Container.Map;
   -- ****

   procedure CreateBookmarkMenu(CreateNew: Boolean := False) is
      XDGBookmarks: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("XDG_DESKTOP_DIR"),
         To_Unbounded_String("XDG_DOWNLOAD_DIR"),
         To_Unbounded_String("XDG_PUBLICSHARE_DIR"),
         To_Unbounded_String("XDG_DOCUMENTS_DIR"),
         To_Unbounded_String("XDG_MUSIC_DIR"),
         To_Unbounded_String("XDG_PICTURES_DIR"),
         To_Unbounded_String("XDG_VIDEOS_DIR"));
      BookmarksMenu: Tk_Menu;
      MenuButton: Ttk_MenuButton;
      Path: Unbounded_String;
      function GetXDGDirectory(Name: String) return Unbounded_String is
         File: File_Type;
         Line: Unbounded_String;
         EqualIndex: Natural;
      begin
         if Value(Name, "") = "" then
            Open(File, In_File, Value("HOME") & "/.config/user-dirs.dirs");
            while not End_Of_File(File) loop
               Line := Get_Line(File);
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
         AddCommands;
      else
         BookmarksMenu.Interp := Get_Context;
         BookmarksMenu.Name := New_String(".bookmarksmenu");
         Delete(BookmarksMenu, "0", "end");
      end if;
      BookmarksList.Clear;
      BookmarksList.Include("Home", Value("HOME"));
      Add
        (BookmarksMenu, "command",
         "-label {" & Mc(Get_Context, "{Home}") &
         "} -command {GoToBookmark {" & Value("HOME") & "}}");
      for I in XDGBookmarks'Range loop
         Path := GetXDGDirectory(To_String(XDGBookmarks(I)));
         if Ada.Directories.Exists(To_String(Path)) then
            BookmarksList.Include
              (Simple_Name(To_String(Path)), To_String(Path));
            Add
              (BookmarksMenu, "command",
               "-label {" & Simple_Name(To_String(Path)) &
               "} -command {GoToBookmark {" & To_String(Path) & "}}");
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
               Line := Get_Line(File);
               if Slice(Line, 1, 7) = "file://" then
                  Path := Unbounded_Slice(Line, 8, Length(Line));
                  BookmarkExist := False;
                  for I in BookmarksList.Iterate loop
                     if BookmarksList(I) = To_String(Path) then
                        BookmarkExist := True;
                        exit;
                     end if;
                  end loop;
                  if not BookmarkExist and
                    Ada.Directories.Exists(To_String(Path)) then
                     BookmarksList.Include
                       (Simple_Name(To_String(Path)), To_String(Path));
                     Add
                       (BookmarksMenu, "command",
                        "-label {" & Simple_Name(To_String(Path)) &
                        "} -command {GoToBookmark {" & To_String(Path) & "}}");
                  end if;
               end if;
            end loop;
            Close(File);
         end;
      end if;
      BookmarksList.Include(Mc(Get_Context, "{Enter destination}"), "");
      Add
        (BookmarksMenu, "command",
         "-label {" & Mc(Get_Context, "{Enter destination}") &
         "} -command SetDestination");
      MenuButton.Interp := BookmarksMenu.Interp;
      MenuButton.Name :=
        New_String(".mainframe.toolbars.actiontoolbar.bookmarksbutton");
      configure(MenuButton, "-menu .bookmarksmenu");
   end CreateBookmarkMenu;

   procedure SetBookmarkButton is
      Button: Ttk_Button;
   begin
      Button.Interp := Get_Context;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.addbutton");
      Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      Button.Name :=
        New_String(".mainframe.toolbars.itemtoolbar.deletebutton");
      Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      if not Ada.Directories.Exists(To_String(CurrentSelected))
        or else Kind(To_String(CurrentSelected)) /= Directory then
         return;
      end if;
      for Bookmark of BookmarksList loop
         if Bookmark = CurrentSelected then
            Tcl.Tk.Ada.Pack.Pack(Button);
            return;
         end if;
      end loop;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.addbutton");
      Tcl.Tk.Ada.Pack.Pack(Button);
   end SetBookmarkButton;

end Bookmarks;
