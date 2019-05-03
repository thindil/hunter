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

with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with MainWindow; use MainWindow;

package body Bookmarks is

-- ****if* Bookmarks/GoToBookmark
-- SOURCE
   procedure GoToBookmark(Self: access Gtk_Menu_Item_Record'Class) is
-- ****
      MenuLabel: constant Unbounded_String :=
        To_Unbounded_String(Get_Label(Self));
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

-- ****if* Bookmarks/GoHome
-- SOURCE
   procedure GoHome(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
-- ****
   begin
      CurrentDirectory := To_Unbounded_String(Value("HOME"));
      if Ada.Directories.Exists(To_String(CurrentDirectory)) then
         Reload(Builder);
      end if;
   end GoHome;

end Bookmarks;
