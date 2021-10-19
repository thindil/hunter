-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Utils.UI is

   -- ****iv* UtilsTUI/UtilsTUI.ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Natural;
   -- ****

   procedure Set_Progress_Bar(Amount: Positive) is
      pragma Unreferenced(Amount);
   begin
      ProgressIndex := 0;
   end Set_Progress_Bar;

   procedure Update_Progress_Bar is
   begin
      ProgressIndex := ProgressIndex + 1;
   end Update_Progress_Bar;

   procedure Toggle_Tool_Buttons
     (Action: Item_Actions; Finished: Boolean := False) is
   begin
      null;
   end Toggle_Tool_Buttons;

end Utils.UI;
