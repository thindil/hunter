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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Box; use Gtk.Box;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with MainWindow; use MainWindow;

package body ProgramsMenu is

   ProgramsList: constant Gtk_List_Store :=
     Gtk_List_Store_Newv((GType_String, GType_String));

   procedure SearchProgram(Self: access Gtk_Search_Entry_Record'Class) is
   begin
      null;
   end SearchProgram;

   function CreateProgramsMenu(Parent: Gtk_Widget) return Gtk_Popover is
      Menu: constant Gtk_Popover := Gtk_Popover_New(Parent);
      MenuBox: constant Gtk_Vbox := Gtk_Vbox_New;
      SearchEntry: constant Gtk_Search_Entry := Gtk_Search_Entry_New;
      Scroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      ProgramsView: constant Gtk_Tree_View :=
        Gtk_Tree_View_New_With_Model(+(ProgramsList));
      Renderer: Gtk_Cell_Renderer_Text;
      Area: Gtk_Cell_Area_Box;
      Column: Gtk_Tree_View_Column;
   begin
      declare
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
         SubFileName: String(1 .. 1024);
         File: File_Type;
         FileLine: Unbounded_String;
         FileIter: Gtk_Tree_Iter;
         NamesList: UnboundedString_Container.Vector;
      begin
         for Path of ApplicationsPaths loop
            if not Ada.Directories.Exists(To_String(Path)) then
               goto End_Of_Loop;
            end if;
            Open(SubDirectory, To_String(Path));
            loop
               Read(SubDirectory, SubFileName, SubLast);
               exit when SubLast = 0;
               if Extension(SubFileName(1 .. SubLast)) = "desktop" then
                  Open
                    (File, In_File,
                     To_String(Path) & "/" &
                     Simple_Name(SubFileName(1 .. SubLast)));
                  while not End_Of_File(File) loop
                     FileLine := To_Unbounded_String(Get_Line(File));
                     if Length(FileLine) > 5
                       and then Slice(FileLine, 1, 5) = "Name="
                       and then not NamesList.Contains
                         (Unbounded_Slice(FileLine, 6, Length(FileLine))) then
                        Append(ProgramsList, FileIter);
                        Set
                          (ProgramsList, FileIter, 0,
                           Slice(FileLine, 6, Length(FileLine)));
                        Set
                          (ProgramsList, FileIter, 1,
                           SubFileName(1 .. SubLast));
                        NamesList.Append
                          (Unbounded_Slice(FileLine, 6, Length(FileLine)));
                        exit;
                     end if;
                  end loop;
                  Close(File);
               end if;
            end loop;
            Close(SubDirectory);
            <<End_Of_Loop>>
         end loop;
      end;
      Set_Placeholder_Text(SearchEntry, Gettext("Search for the program"));
      On_Search_Changed(SearchEntry, SearchProgram'Access);
      Pack_Start(MenuBox, SearchEntry, False);
      Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
      Area := Gtk_Cell_Area_Box_New;
      Column := Gtk_Tree_View_Column_New_With_Area(Area);
      Set_Sort_Column_Id(Column, 0);
      Set_Title(Column, Gettext("Name"));
      if Append_Column(ProgramsView, Column) < 0 then
         Put_Line("Error in adding columns.");
      end if;
      Pack_Start(Area, Renderer, True);
      Add_Attribute(Area, Renderer, "text", 0);
      Set_Policy(Scroll, Policy_Never, Policy_Automatic);
      Add(Scroll, ProgramsView);
      Pack_Start(MenuBox, Scroll);
      Show_All(MenuBox);
      Add(Menu, MenuBox);
      Set_Modal(Menu, True);
      Set_Size_Request(Menu, -1, 400);
      return Menu;
   end CreateProgramsMenu;

end ProgramsMenu;
