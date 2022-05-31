-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages.UI; use Messages.UI;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body CopyItems.UI is

   function CopySelected(Overwrite: in out Boolean) return Ui_Locations is
   begin
      if Destination_Directory = Common.Current_Directory then
         Ui_Location := DIRECTORY_VIEW;
         Update_Directory_List(True);
         return DIRECTORY_VIEW;
      end if;
      if not Copy_Items(Interpreter, Overwrite) then
         return MESSAGE_FORM;
      end if;
      if Settings.Show_Finished_Info then
         Show_Message
           (Mc
              (Interpreter,
               "{All selected files and directories have been copied.}"),
            "message");
         return MESSAGE_FORM;
      end if;
      Ui_Location := DIRECTORY_VIEW;
      Common.Current_Directory :=
        (if Settings.Stay_In_Old then Common.Current_Directory
         else Destination_Directory);
      Load_Directory(To_String(Common.Current_Directory));
      Update_Directory_List(True);
      Update_Watch(To_String(Common.Current_Directory));
      if Settings.Stay_In_Old then
         Show_Preview;
      end if;
      return DIRECTORY_VIEW;
   end CopySelected;

   function SkipCopying return Ui_Locations is
      OverwriteItem: Boolean := False;
   begin
      Copy_Items_List.Delete(Index => 1);
      Update_Progress_Bar;
      return CopySelected(OverwriteItem);
   end SkipCopying;

end CopyItems.UI;
