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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtkada.Builder; use Gtkada.Builder;

package MainWindow is

   -- Gtk Builder used to read data from glade file
   Builder: Gtkada_Builder;
   -- If true, the program is in the setting mode
   Setting: Boolean;
   -- Currently selected directory to show
   CurrentDirectory: Unbounded_String;
   -- Types of action on files and directories
   type ItemActions is (CREATEFILE, CREATEDIRECTORY, RENAME, DELETE);
   -- Current performed action on files or directories
   NewAction: ItemActions;
   package UnboundedString_Container is new Vectors(Positive,
      Unbounded_String);
   -- List of currently selected files and directories by user
   SelectedItems: UnboundedString_Container.Vector;

   -- Quit from program
   procedure Quit(Object: access Gtkada_Builder_Record'Class);
   -- Create main window and show content of selected directory
   procedure CreateMainWindow(NewBuilder: Gtkada_Builder; Directory: String);
   -- Reload directory listing and preview of selected item
   procedure Reload(Object: access Gtkada_Builder_Record'Class);

end MainWindow;
