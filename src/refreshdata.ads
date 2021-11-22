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

-- ****h* RefreshData/RefreshData
-- FUNCTION
-- Provide code to automatically refresh directory listing on changed files
-- or directories inside
-- SOURCE
package RefreshData is
-- ****

   -- ****a* RefreshData/RefreshData.Inotify_Task
   -- FUNCTION
   -- Task to monitor changes on disk
   -- SOURCE
   task Inotify_Task is
      entry Start;
   end Inotify_Task;
   -- ****

   -- ****f* RefreshData/RefreshData.Start_Timer
   -- FUNCTION
   -- Start timer for refreshing current directory listing
   -- SOURCE
   procedure Start_Timer(Path: String := "");
   -- ****

   -- ****f* RefreshData/RefreshData.Update_Watch
   -- FUNCTION
   -- Update inotify watch with new path to watch.
   -- PARAMETERS
   -- Path - Full path to the directory which will be watched for changes.
   -- SOURCE
   procedure Update_Watch(Path: String);
   -- ****

end RefreshData;
