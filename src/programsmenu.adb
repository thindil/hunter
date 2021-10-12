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
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body ProgramsMenu is

   function GetProgramName(DesktopFile: String) return String is
   begin
      if not ApplicationsList.Contains(DesktopFile) then
         return DesktopFile;
      end if;
      return ApplicationsList(DesktopFile);
   end GetProgramName;

   procedure CreateProgramsMenu is
      ApplicationsPaths: constant array
        (Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("/usr/share/applications"),
         To_Unbounded_String("/usr/share/applnk"),
         To_Unbounded_String("/usr/local/share/applications"),
         To_Unbounded_String("/usr/local/share/applnk"),
         To_Unbounded_String(Value("HOME") & "/.local/share/applications"),
         To_Unbounded_String(Value("HOME") & "/.local/share/applnk"));
      SubDirectory: Dir_Type;
      SubLast: Natural;
      SubFileName: String(1 .. 1_024);
      File: File_Type;
      FileLine: Unbounded_String;
   begin
      Create_Programs_Menu_Loop :
      for Path of ApplicationsPaths loop
         if not Ada.Directories.Exists(To_String(Path)) then
            goto End_Of_Loop;
         end if;
         Open(SubDirectory, To_String(Path));
         Read_Desktop_File_Loop :
         loop
            Read(SubDirectory, SubFileName, SubLast);
            exit Read_Desktop_File_Loop when SubLast = 0;
            if Extension(SubFileName(1 .. SubLast)) = "desktop" then
               Open
                 (File, In_File,
                  To_String(Path) & "/" &
                  Simple_Name(SubFileName(1 .. SubLast)));
               Find_Application_Name_Loop :
               while not End_Of_File(File) loop
                  FileLine := To_Unbounded_String(Get_Line(File));
                  if Length(FileLine) > 5
                    and then Slice(FileLine, 1, 5) = "Name=" then
                     ApplicationsList.Include
                       (SubFileName(1 .. SubLast),
                        Slice(FileLine, 6, Length(FileLine)));
                     if not NamesList.Contains
                         (Unbounded_Slice(FileLine, 6, Length(FileLine))) then
                        NamesList.Append
                          (Unbounded_Slice(FileLine, 6, Length(FileLine)));
                     end if;
                     exit Find_Application_Name_Loop;
                  end if;
               end loop Find_Application_Name_Loop;
               Close(File);
            end if;
         end loop Read_Desktop_File_Loop;
         Close(SubDirectory);
         <<End_Of_Loop>>
      end loop Create_Programs_Menu_Loop;
      Programs_Sorting.Sort(NamesList);
   end CreateProgramsMenu;

end ProgramsMenu;
