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

with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;
with Ada.Strings.Hash;
with Inotify; use Inotify;
with Glib; use Glib;
with Glib.Main; use Glib.Main;
with Preferences; use Preferences;
with Ada.Text_IO;

package body RefreshData is

   -- ****iv* RefreshData/Source_Id
   -- FUNCTION
   -- ID of timer to hide messages
   -- SOURCE
   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   InotifyInstance: Instance;
   package Items_Container is new Indefinite_Hashed_Maps(String, Event_Kind,
      Ada.Strings.Hash, "=");
   ItemsList: Items_Container.Map;

   function CheckItems return Boolean is
   begin
      if not TemporaryStop then
         for I in ItemsList.Iterate loop
            Ada.Text_IO.Put_Line
              (Items_Container.Key(I) & " " & Event_Kind'Image(ItemsList(I)));
         end loop;
      end if;
      ItemsList.Clear;
      return True;
   end CheckItems;

   task body InotifyTask is
      procedure Handle_Event
        (Subject: Watch; Event: Event_Kind; Is_Directory: Boolean;
         Name: String) is
         pragma Unreferenced(Subject, Is_Directory);
      begin
         ItemsList.Include(Name, Event);
      end Handle_Event;
   begin
      accept Start;
      InotifyInstance.Process_Events(Handle_Event'Access);
   end InotifyTask;

   procedure StartTimer(Path: String := "") is
   begin
      if Source_Id /= No_Source_Id then
         Remove(Source_Id);
      end if;
      if Path /= "" then
         ItemsList.Clear;
         InotifyInstance.Add_Watch
           (Path,
            (Created | Metadata | Closed_Write | Moved_From | Moved_To |
             Deleted =>
               True,
             others => False));
         InotifyTask.Start;
      end if;
      Source_Id :=
        Timeout_Add
          (Guint(Settings.AutoRefreshInterval) * 1000, CheckItems'Access);
   end StartTimer;

   procedure UpdateWatch(Path: String) is
   begin
      ItemsList.Clear;
      InotifyInstance.Add_Watch
        (Path,
         (Created | Metadata | Closed_Write | Moved_From | Moved_To |
          Deleted =>
            True,
          others => False));
   end UpdateWatch;

end RefreshData;
