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
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Bookmarks.Commands;
with MainWindow;

package body Bookmarks is

   -- ****iv* Bookmarks/Bookmarks.Bookmarks_List
   -- FUNCTION
   -- List of all bookmarked locations
   -- SOURCE
   Bookmarks_List: Bookmarks_Container.Map;
   -- ****

   -- ****if* Bookmarks/Bookmarks.Get_Bookmarks_List
   -- FUNCTION
   -- Get the list of the all bookmarked locations
   -- RESULT
   -- The list of all bookmarked locations
   -- SOURCE
   function Get_Bookmarks_List return Bookmarks_Container.Map is
      -- ****
   begin
      return Bookmarks_List;
   end Get_Bookmarks_List;

   procedure Create_Bookmark_Menu(Create_New: Boolean := False) is
      use Tcl.Tk.Ada.Widgets.TtkMenuButton;
      use Bookmarks.Commands;

      Xdg_Bookmarks: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "XDG_DESKTOP_DIR"),
         2 => To_Unbounded_String(Source => "XDG_DOWNLOAD_DIR"),
         3 => To_Unbounded_String(Source => "XDG_PUBLICSHARE_DIR"),
         4 => To_Unbounded_String(Source => "XDG_DOCUMENTS_DIR"),
         5 => To_Unbounded_String(Source => "XDG_MUSIC_DIR"),
         6 => To_Unbounded_String(Source => "XDG_PICTURES_DIR"),
         7 => To_Unbounded_String(Source => "XDG_VIDEOS_DIR"));
      Bookmarks_Menu: Tk_Menu;
      Menu_Button: constant Ttk_MenuButton :=
        Get_Widget
          (pathName => ".mainframe.toolbars.actiontoolbar.bookmarksbutton");
      Path: Unbounded_String := Null_Unbounded_String;
      Local_Bookmarks_List: Bookmarks_Container.Map := Get_Bookmarks_List;
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
      if Create_New then
         Bookmarks_Menu :=
           Create(pathName => ".bookmarksmenu", options => "-tearoff false");
         AddCommands;
      else
         Bookmarks_Menu := Get_Widget(pathName => ".bookmarksmenu");
         Delete
           (MenuWidget => Bookmarks_Menu, StartIndex => "0",
            EndIndex => "end");
      end if;
      Local_Bookmarks_List.Clear;
      Local_Bookmarks_List.Include
        (Key => Mc(Interp => Get_Context, Src_String => "{Home}"),
         New_Item => Value(Name => "HOME"));
      Add
        (MenuWidget => Bookmarks_Menu, EntryType => "command",
         Options =>
           "-label {" & Mc(Interp => Get_Context, Src_String => "{Home}") &
           "} -command {GoToBookmark {" & Value(Name => "HOME") & "}}");
      Set_Xdg_Bookmarks_List_Loop :
      for Bookmark of Xdg_Bookmarks loop
         Path := Get_Xdg_Directory(Name => To_String(Source => Bookmark));
         if Ada.Directories.Exists(Name => To_String(Source => Path)) then
            Local_Bookmarks_List.Include
              (Key => Simple_Name(Name => To_String(Source => Path)),
               New_Item => To_String(Source => Path));
            Add
              (MenuWidget => Bookmarks_Menu, EntryType => "command",
               Options =>
                 "-label {" & Simple_Name(Name => To_String(Source => Path)) &
                 "} -command {GoToBookmark {" & To_String(Source => Path) &
                 "}}");
         end if;
      end loop Set_Xdg_Bookmarks_List_Loop;
      if Ada.Directories.Exists
          (Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks") then
         Add_User_Bookmarks_Block :
         declare
            File: File_Type;
            Line, Bookmark_Path: Unbounded_String := Null_Unbounded_String;
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
               Bookmark_Path :=
                 Unbounded_Slice
                   (Source => Line, Low => 8, High => Length(Source => Line));
               Bookmark_Exist := False;
               Check_Bookmark_Existence_Loop :
               for I in Local_Bookmarks_List.Iterate loop
                  if Local_Bookmarks_List(I) =
                    To_String(Source => Bookmark_Path) then
                     Bookmark_Exist := True;
                     exit Check_Bookmark_Existence_Loop;
                  end if;
               end loop Check_Bookmark_Existence_Loop;
               if not Bookmark_Exist and
                 Ada.Directories.Exists
                   (Name => To_String(Source => Bookmark_Path)) then
                  Local_Bookmarks_List.Include
                    (Key =>
                       Simple_Name(Name => To_String(Source => Bookmark_Path)),
                     New_Item => To_String(Source => Bookmark_Path));
                  Add
                    (MenuWidget => Bookmarks_Menu, EntryType => "command",
                     Options =>
                       "-label {" &
                       Simple_Name
                         (Name => To_String(Source => Bookmark_Path)) &
                       "} -command {GoToBookmark {" &
                       To_String(Source => Bookmark_Path) & "}}");
               end if;
               <<End_Of_Loop>>
            end loop Load_User_Bookmarks_Loop;
            Close(File => File);
         end Add_User_Bookmarks_Block;
      end if;
      Local_Bookmarks_List.Include
        (Key => Mc(Interp => Get_Context, Src_String => "{Enter destination}"),
         New_Item => "");
      Add
        (MenuWidget => Bookmarks_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Enter destination}") &
           "} -command SetDestination");
      configure(Widgt => Menu_Button, options => "-menu .bookmarksmenu");
      Bookmarks_List := Local_Bookmarks_List;
   end Create_Bookmark_Menu;

   procedure Set_Bookmark_Button is
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use MainWindow;

      Button: Ttk_Button :=
        Get_Widget(pathName => ".mainframe.toolbars.itemtoolbar.addbutton");
      Bookmarks_Menu: constant Tk_Menu :=
        Get_Widget(pathName => ".bookmarksmenu");
      Local_Bookmarks_List: constant Bookmarks_Container.Map :=
        Get_Bookmarks_List;
   begin
      Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      Button.Name :=
        New_String(".mainframe.toolbars.itemtoolbar.deletebutton");
      Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      if not Ada.Directories.Exists(To_String(Current_Selected))
        or else Kind(To_String(Current_Selected)) /= Directory then
         return;
      end if;
      Set_Bookmark_Button_Loop :
      for I in Local_Bookmarks_List.Iterate loop
         if Local_Bookmarks_List(I) = Current_Selected then
            if Natural'Value
                (Index
                   (Bookmarks_Menu, "{" & Bookmarks_Container.Key(I) & "}")) <
              8 then
               return;
            end if;
            Tcl.Tk.Ada.Pack.Pack(Button);
            return;
         end if;
      end loop Set_Bookmark_Button_Loop;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.addbutton");
      Tcl.Tk.Ada.Pack.Pack(Button);
   end Set_Bookmark_Button;

end Bookmarks;
