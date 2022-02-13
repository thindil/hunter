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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body ProgramsMenu is

   function Get_Program_Name(Desktop_File: String) return String is
   begin
      if not Applications_List.Contains(Key => Desktop_File) then
         return Desktop_File;
      end if;
      return Applications_List(Desktop_File);
   end Get_Program_Name;

   procedure Create_Programs_Menu is
      Applications_Paths: constant array(1 .. 6) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "/usr/share/applications"),
         2 => To_Unbounded_String(Source => "/usr/share/applnk"),
         3 => To_Unbounded_String(Source => "/usr/local/share/applications"),
         4 => To_Unbounded_String(Source => "/usr/local/share/applnk"),
         5 =>
           To_Unbounded_String
             (Source => Value(Name => "HOME") & "/.local/share/applications"),
         6 =>
           To_Unbounded_String
             (Source => Value(Name => "HOME") & "/.local/share/applnk"));
      Sub_Directory: Dir_Type;
      Sub_Last: Natural := 0;
      Sub_File_Name: String(1 .. 1_024) := (others => ' ');
      File: File_Type;
      File_Line: Unbounded_String := Null_Unbounded_String;
   begin
      Create_Programs_Menu_Loop :
      for Path of Applications_Paths loop
         if not Ada.Directories.Exists(Name => To_String(Source => Path)) then
            goto End_Of_Loop;
         end if;
         Open(Dir => Sub_Directory, Dir_Name => To_String(Source => Path));
         Read_Desktop_File_Loop :
         loop
            Read(Dir => Sub_Directory, Str => Sub_File_Name, Last => Sub_Last);
            exit Read_Desktop_File_Loop when Sub_Last = 0;
            if Extension(Name => Sub_File_Name(1 .. Sub_Last)) = "desktop" then
               Open
                 (File => File, Mode => In_File,
                  Name =>
                    To_String(Source => Path) & "/" &
                    Simple_Name(Name => Sub_File_Name(1 .. Sub_Last)));
               Find_Application_Name_Loop :
               while not End_Of_File(File => File) loop
                  File_Line :=
                    To_Unbounded_String(Source => Get_Line(File => File));
                  if Length(Source => File_Line) > 5
                    and then Slice(Source => File_Line, Low => 1, High => 5) =
                      "Name=" then
                     Applications_List.Include
                       (Key => Sub_File_Name(1 .. Sub_Last),
                        New_Item =>
                          Slice
                            (Source => File_Line, Low => 6,
                             High => Length(Source => File_Line)));
                     if not Names_List.Contains
                         (Item =>
                            Unbounded_Slice
                              (Source => File_Line, Low => 6,
                               High => Length(Source => File_Line))) then
                        Names_List.Append
                          (New_Item =>
                             Unbounded_Slice
                               (Source => File_Line, Low => 6,
                                High => Length(Source => File_Line)));
                     end if;
                     exit Find_Application_Name_Loop;
                  end if;
               end loop Find_Application_Name_Loop;
               Close(File => File);
            end if;
         end loop Read_Desktop_File_Loop;
         Close(Dir => Sub_Directory);
         <<End_Of_Loop>>
      end loop Create_Programs_Menu_Loop;
      Programs_Sorting.Sort(Container => Names_List);
   end Create_Programs_Menu;

end ProgramsMenu;
