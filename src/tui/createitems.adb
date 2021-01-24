-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;

package body CreateItems is

   procedure Create_Item(NewName: String) is
      NewItemName, ActionString, ActionBlocker, Destination: Unbounded_String;
      File: File_Descriptor;
      Hunter_Create_Exception: exception;
   begin
      NewItemName := CurrentDirectory & "/" & NewName;
      if Exists(To_String(NewItemName)) or
        Is_Symbolic_Link(To_String(NewItemName)) then
        if NewAction = CREATEFILE then
         ActionString :=
           To_Unbounded_String
             (Mc(Interpreter, "{create}") & " file " &
              Mc(Interpreter, "{with}"));
        elsif NewAction = CREATEDIRECTORY then
         ActionString :=
           To_Unbounded_String
             (Mc(Interpreter, "{create}") & " directory " &
              Mc(Interpreter, "{with}"));
        else
         ActionString :=
           To_Unbounded_String
             (Mc(Interpreter, "{create}") & " link " &
              Mc(Interpreter, "{with}"));
        end if;
         ActionBlocker :=
           (if Is_Directory(To_String(NewItemName)) then
              To_Unbounded_String(Mc(Interpreter, "directory"))
            else To_Unbounded_String(Mc(Interpreter, "file")));
         ShowMessage
           (Mc(Interpreter, "{You can't}") & " " & To_String(ActionString) & " " &
            Mc(Interpreter, "{name}") & " '" & To_String(NewItemName) & "' " &
            Mc(Interpreter, "{because there exists}") & " " &
            To_String(ActionBlocker) & " " & Mc(Interpreter, "{with that name.}"));
         goto End_Of_Create;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewItemName))) then
         ShowMessage
           (Mc(Interpreter, "{You don't have permissions to write to}") & " " &
            Containing_Directory(To_String(NewItemName)));
         goto End_Of_Create;
      end if;
      case NewAction is
         when CREATEDIRECTORY =>
            Create_Path(To_String(NewItemName));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(NewItemName)));
            File := Create_File(To_String(NewItemName), Binary);
            Close(File);
         when CREATELINK =>
            Destination := DestinationDirectory;
--            if Selection(DirectoryView)'Length > 0 then
--               Destination :=
--                 DestinationDirectory &
--                 SecondItemsList(Positive'Value(Selection(DirectoryView)))
--                   .Name;
--            end if;
            Tcl_Eval
              (Interpreter,
               "file link -symbolic {" & To_String(NewItemName) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception
              with Mc(Interpreter, "{Invalid action type}");
      end case;
      if not Settings.StayInOld and then NewAction /= CREATELINK then
         CurrentDirectory :=
           To_Unbounded_String(Containing_Directory(To_String(NewItemName)));
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateWatch(To_String(CurrentDirectory));
      <<End_Of_Create>>
   end Create_Item;

end CreateItems;
