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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body CopyItems.UI is

   -- ****iv* CopyItems/CUITUI.SourceDirectory
   -- FUNCTION
   -- Full path to the source directory of copied files and directories
   -- SOURCE
   SourceDirectory: Unbounded_String;
   -- ****

   function CopySelected(Overwrite: in out Boolean) return UI_Locations is
      Path, ItemType: Unbounded_String;
      Success: Boolean := True;
   begin
      if DestinationDirectory = MainWindow.Current_Directory then
         UILocation := DIRECTORY_VIEW;
         Update_Directory_List(True);
         return DIRECTORY_VIEW;
      end if;
      Copy_Items_Loop :
      while Copy_Items_List.Length > 0 loop
         Path := DestinationDirectory;
         if Exists
             (To_String(Path) & "/" &
              Simple_Name(To_String(Copy_Items_List(1)))) and
           not Overwrite and Settings.Overwrite_On_Exist then
            ItemType :=
              (if
                 Is_Directory
                   (To_String(Path) & "/" &
                    Simple_Name(To_String(Copy_Items_List(1))))
               then To_Unbounded_String(Mc(Interpreter, "{Directory}"))
               else To_Unbounded_String(Mc(Interpreter, "{File}")));
            Show_Message
              (To_String(ItemType) & " " &
               Simple_Name(To_String(Copy_Items_List(1))) & " " &
               Mc(Interpreter, "{exists. Do you want to overwrite it?}"),
               "question");
            return MESSAGE_FORM;
         end if;
         Copy_Item(To_String(Copy_Items_List(1)), Path, Success);
         exit Copy_Items_Loop when not Success;
         Copy_Items_List.Delete(Index => 1);
         if not YesForAll then
            Overwrite := False;
         end if;
      end loop Copy_Items_Loop;
      Copy_Items_List.Clear;
      if Settings.Show_Finished_Info then
         Show_Message
           (Mc
              (Interpreter,
               "{All selected files and directories have been copied.}"),
            "message");
         return MESSAGE_FORM;
      end if;
      UILocation := DIRECTORY_VIEW;
      MainWindow.Current_Directory :=
        (if Settings.Stay_In_Old then SourceDirectory
         else DestinationDirectory);
      Load_Directory(To_String(MainWindow.Current_Directory));
      Update_Directory_List(True);
      UpdateWatch(To_String(MainWindow.Current_Directory));
      return DIRECTORY_VIEW;
   end CopySelected;

   function SkipCopying return UI_Locations is
      OverwriteItem: Boolean := False;
   begin
      Copy_Items_List.Delete(Index => 1);
      Update_Progress_Bar;
      return CopySelected(OverwriteItem);
   end SkipCopying;

end CopyItems.UI;
