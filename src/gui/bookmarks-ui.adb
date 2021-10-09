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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Bookmarks.Commands;
with Common;

package body Bookmarks.UI is

   procedure Create_Bookmark_Menu(Create_New: Boolean := False) is
      use Ada.Environment_Variables;
      use Tcl.MsgCat.Ada;
      use Tcl.Tk.Ada.Widgets.TtkMenuButton;
      use Bookmarks.Commands;

      Bookmarks_Menu: Tk_Menu;
      Menu_Button: constant Ttk_MenuButton :=
        Get_Widget
          (pathName => ".mainframe.toolbars.actiontoolbar.bookmarksbutton");
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
      Fill_Bookmarks_List;
      Add
        (MenuWidget => Bookmarks_Menu, EntryType => "command",
         Options =>
           "-label {" & Mc(Interp => Get_Context, Src_String => "{Home}") &
           "} -command {GoToBookmark {" & Value(Name => "HOME") & "}}");
      Add_Menu_Entries_Loop:
      for I in Bookmarks_List.Iterate loop
         Add
           (MenuWidget => Bookmarks_Menu, EntryType => "command",
            Options =>
              "-label {" & Bookmarks_Container.Key(Position => I) &
              "} -command {GoToBookmark {" & Bookmarks_List(I) & "}}");
      end loop Add_Menu_Entries_Loop;
      Add
        (MenuWidget => Bookmarks_Menu, EntryType => "command",
         Options =>
           "-label {" &
           Mc(Interp => Get_Context, Src_String => "{Enter destination}") &
           "} -command SetDestination");
      configure(Widgt => Menu_Button, options => "-menu .bookmarksmenu");
   end Create_Bookmark_Menu;

   procedure Set_Bookmark_Button is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Common;

      Button: Ttk_Button :=
        Get_Widget(pathName => ".mainframe.toolbars.itemtoolbar.addbutton");
      Bookmarks_Menu: constant Tk_Menu :=
        Get_Widget(pathName => ".bookmarksmenu");
   begin
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      Button.Name :=
        New_String(Str => ".mainframe.toolbars.itemtoolbar.deletebutton");
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      if not Ada.Directories.Exists
          (Name => To_String(Source => Current_Selected))
        or else Kind(Name => To_String(Source => Current_Selected)) /=
          Directory then
         return;
      end if;
      Set_Bookmark_Button_Loop :
      for I in Bookmarks_List.Iterate loop
         if Bookmarks_List(I) = Current_Selected then
            if Natural'Value
                (Index
                   (MenuWidget => Bookmarks_Menu,
                    Index =>
                      "{" & Bookmarks_Container.Key(Position => I) & "}")) <
              8 then
               return;
            end if;
            Tcl.Tk.Ada.Pack.Pack(Slave => Button);
            return;
         end if;
      end loop Set_Bookmark_Button_Loop;
      Button.Name :=
        New_String(Str => ".mainframe.toolbars.itemtoolbar.addbutton");
      Tcl.Tk.Ada.Pack.Pack(Slave => Button);
   end Set_Bookmark_Button;

end Bookmarks.UI;
