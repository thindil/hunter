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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtkada.Intl; use Gtkada.Intl;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;

package body DeleteItems is

   function DeleteSelected return Boolean is
      GoUp, Success: Boolean := False;
      Arguments: Argument_List := (new String'("-rf"), new String'(""));
   begin
      for Item of SelectedItems loop
         if Is_Directory(To_String(Item)) then
            Arguments(2) := new String'(To_String(Item));
            if Settings.DeleteFiles then
               Spawn(Locate_Exec_On_Path("rm").all, Arguments, Success);
               if not Success then
                  raise Directory_Error with To_String(Item);
               end if;
            end if;
            if Item = CurrentDirectory then
               GoUp := True;
            end if;
         else
            if Settings.DeleteFiles then
               Delete_File(To_String(Item));
            end if;
         end if;
      end loop;
      return GoUp;
   exception
      when An_Exception : Use_Error =>
         ShowMessage
           (Gettext
              ("Could not delete selected files or directories. Reason: ") &
            Exception_Message(An_Exception));
         raise;
      when An_Exception : Directory_Error =>
         ShowMessage
           (Gettext("Can't delete selected directory: ") &
            Exception_Message(An_Exception));
         raise;
      when others =>
         ShowMessage
           (Gettext("Unknown error during deleting files or directories."));
         raise;
   end DeleteSelected;

   procedure DeleteItem(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      Message: Unbounded_String;
   begin
      if Settings.DeleteFiles then
         Message := To_Unbounded_String(Gettext("Delete?") & LF);
      else
         Message := To_Unbounded_String(Gettext("Move to trash?") & LF);
      end if;
      for I in SelectedItems.First_Index .. SelectedItems.Last_Index loop
         Append(Message, SelectedItems(I));
         if Is_Directory(To_String(SelectedItems(I))) then
            Append(Message, Gettext("(and its content)"));
         end if;
         if I /= SelectedItems.Last_Index then
            Append(Message, LF);
         end if;
      end loop;
      NewAction := DELETE;
      ToggleToolButtons(NewAction);
      ShowMessage(To_String(Message), Message_Question);
   end DeleteItem;

end DeleteItems;
