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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;

package body LoadData.UI is

   procedure Load_Directory
     (Directory_Name: String; Second: Boolean := False) is
      Directory: Dir_Type;
      FileName: String(1 .. 1_024);
      Last: Natural range 0 .. FileName'Last;
   begin
      Tcl.Tk.Ada.Busy.Busy(Get_Main_Window(Get_Context));
      Tcl_Eval(Get_Context, "update");
      if not Second then
         Items_List.Clear;
      else
         Second_Items_List.Clear;
      end if;
      if not Is_Read_Accessible_File(Directory_Name) then
         Tcl.Tk.Ada.Busy.Forget(Get_Main_Window(Get_Context));
         return;
      end if;
      Open(Directory, Directory_Name);
      Read_Directory_Loop :
      loop
         Read(Directory, FileName, Last);
         exit Read_Directory_Loop when Last = 0;
         if FileName(1 .. Last) /= "." and FileName(1 .. Last) /= ".." then
            if not Second then
               Add_Item
                 (Directory_Name & "/" & FileName(1 .. Last), Items_List);
            else
               Add_Item
                 (Directory_Name & "/" & FileName(1 .. Last),
                  Second_Items_List);
            end if;
         end if;
      end loop Read_Directory_Loop;
      Close(Directory);
      if not Second then
         Items_Sorting.Sort(Items_List);
         Wm_Set
           (Get_Main_Window(Get_Context), "title",
            "{Hunter " & Directory_Name & "}");
      else
         Items_Sorting.Sort(Second_Items_List);
      end if;
      if Tcl.Tk.Ada.Busy.Status(Get_Main_Window(Get_Context)) = "1" then
         Tcl.Tk.Ada.Busy.Forget(Get_Main_Window(Get_Context));
      end if;
   end Load_Directory;

end LoadData.UI;
