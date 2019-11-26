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

with Gtk.Box; use Gtk.Box;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtkada.Intl; use Gtkada.Intl;

package body ProgramsMenu is

   procedure SearchProgram(Self: access Gtk_Search_Entry_Record'Class) is
   begin
      null;
   end SearchProgram;

   function CreateProgramsMenu(Parent: Gtk_Widget) return Gtk_Popover is
      Menu: constant Gtk_Popover := Gtk_Popover_New(Parent);
      MenuBox: constant Gtk_VBox := Gtk_Vbox_New;
      SearchEntry: constant Gtk_Search_Entry := Gtk_Search_Entry_New;
   begin
      Set_Placeholder_Text(SearchEntry, Gettext("Search for the program"));
      On_Search_Changed(SearchEntry, SearchProgram'Access);
      Pack_Start(MenuBox, SearchEntry);
      Show_All(MenuBox);
      Add(Menu, MenuBox);
      Set_Modal(Menu, True);
      return Menu;
   end CreateProgramsMenu;

end ProgramsMenu;
