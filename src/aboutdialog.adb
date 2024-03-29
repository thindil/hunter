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

with Tcl.MsgCat.Ada;

package body AboutDialog is

   procedure Set_About_Dialog_Information(Interp: Tcl.Tcl_Interp) is
      use Tcl.MsgCat.Ada;

   begin
      License :=
        To_Unbounded_String
          (Source =>
             Mc(Interp => Interp, Src_String => "{License:}") & " GNU GPLv3");
      Version :=
        To_Unbounded_String
          (Source =>
             Mc(Interp => Interp, Src_String => "{Version:}") & " " &
             Version_Number & " (" &
             Mc(Interp => Interp, Src_String => "{development}") & ")");
      Website_Text :=
        To_Unbounded_String
          (Source => Mc(Interp => Interp, Src_String => "{Website}"));
      Programmers_Text :=
        To_Unbounded_String
          (Source => Mc(Interp => Interp, Src_String => "Programmers"));
      Translators_Text :=
        To_Unbounded_String
          (Source => Mc(Interp => Interp, Src_String => "Translators"));
      Close_Text :=
        To_Unbounded_String
          (Source => Mc(Interp => Interp, Src_String => "Close"));
   end Set_About_Dialog_Information;

end AboutDialog;
