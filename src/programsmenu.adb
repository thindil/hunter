-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

--with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
--with GNAT.OS_Lib; use GNAT.OS_Lib;
--with Gtk.Box; use Gtk.Box;
--with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
--with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
--with Gtk.Enums; use Gtk.Enums;
--with Gtk.List_Store; use Gtk.List_Store;
--with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
--with Gtk.Search_Entry; use Gtk.Search_Entry;
--with Gtk.Toggle_Button; use Gtk.Toggle_Button;
--with Gtk.Tree_Model; use Gtk.Tree_Model;
--with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
--with Gtk.Tree_View; use Gtk.Tree_View;
--with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
--with Gtkada.Intl; use Gtkada.Intl;
--with Glib; use Glib;
with Bookmarks; use Bookmarks;
with MainWindow; use MainWindow;
--with Messages; use Messages;
--with Utils; use Utils;

package body ProgramsMenu is

   -- ****iv* ProgramsMenu/ApplicationsList
   -- FUNCTION
   -- List of all applications which can be used to execute files or
   -- directories
   -- SOURCE
   ApplicationsList: Bookmarks_Container.Map;
   -- ****

   procedure CreateProgramsMenu is
--      Menu: constant Gtk_Popover := Gtk_Popover_New(Parent);
--      MenuBox: constant Gtk_Vbox := Gtk_Vbox_New;
--      SearchEntry: constant Gtk_Search_Entry := Gtk_Search_Entry_New;
--      Scroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
--      ProgramsView: constant Gtk_Tree_View :=
--        Gtk_Tree_View_New_With_Model(+(ProgramsFilter));
--      Renderer: Gtk_Cell_Renderer_Text;
--      Area: Gtk_Cell_Area_Box;
--      Column: Gtk_Tree_View_Column;
--      ProgramsList: constant Gtk_List_Store := -(Get_Model(ProgramsFilter));
   begin
      --     MenuButton := Gtk_Toggle_Button(Parent);
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
                        ApplicationsList.Include
                          (SubFileName(1 .. SubLast),
                           Slice(FileLine, 6, Length(FileLine)));
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
--      Set_Sort_Column_Id(ProgramsList, 0, Sort_Ascending);
--      Set_Visible_Func(ProgramsFilter, VisiblePrograms'Access);
--      Set_Placeholder_Text(SearchEntry, Gettext("Search for the program"));
--      On_Search_Changed(SearchEntry, SearchProgram'Access);
--      Pack_Start(MenuBox, SearchEntry, False);
--      Gtk.Cell_Renderer_Text.Gtk_New(Renderer);
--      Area := Gtk_Cell_Area_Box_New;
--      Column := Gtk_Tree_View_Column_New_With_Area(Area);
--      Set_Sort_Column_Id(Column, 0);
--      Set_Title(Column, Gettext("Name"));
--      if Append_Column(ProgramsView, Column) < 0 then
--         Put_Line("Error in adding columns.");
--      end if;
--      Pack_Start(Area, Renderer, True);
--      Add_Attribute(Area, Renderer, "text", 0);
--      Set_Activate_On_Single_Click(ProgramsView, True);
--      On_Row_Activated(ProgramsView, SetProgram'Access);
--      Set_Policy(Scroll, Policy_Never, Policy_Automatic);
--      Add(Scroll, ProgramsView);
--      Pack_Start(MenuBox, Scroll);
--      Show_All(MenuBox);
--      Add(Menu, MenuBox);
--      Set_Modal(Menu, True);
--      Set_Size_Request(Menu, -1, 400);
--      return Menu;
   end CreateProgramsMenu;

   function GetProgramName(DesktopFile: String) return String is
   begin
      return ApplicationsList(DesktopFile);
   end GetProgramName;

   -- ****iv* ProgramsMenu/ProgramsFilter
   -- FUNCTION
   -- Filter and list which contains all applications
   -- SOURCE
--   ProgramsFilter: constant Gtk_Tree_Model_Filter :=
--     Gtk_Tree_Model_Filter_Filter_New
--       (+(Gtk_List_Store_Newv((GType_String, GType_String))));
--   -- ****
--
--   -- ****iv* ProgramsMenu/SearchFor
--   -- FUNCTION
--   -- String which the program will be looking for in applications list
--   -- SOURCE
--   SearchFor: Unbounded_String;
--   -- ****
--
--   -- ****iv* ProgramsMenu/MenuButton
--   -- FUNCTION
--   -- Button which is parent for the programs menu
--   -- SOURCE
--   MenuButton: Gtk_Toggle_Button;
--   -- ****
--
--   -- ****if* ProgramsMenu/VisiblePrograms
--   -- FUNCTION
--   -- Check if selected application should be visible when the user is looking
--   -- for selected name
--   -- PARAMETERS
--   -- Model - Gtk_Tree_Model which contains all applications names
--   -- Iter  - Gtk_Tree_Iter to selected application name
--   -- RESULT
--   -- True if application should be visible, otherwise false
--   -- SOURCE
--   function VisiblePrograms
--     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
--   -- ****
--   begin
--      if Setting or SearchFor = Null_Unbounded_String then
--         return True;
--      end if;
--      if Index
--          (To_Lower(Get_String(Model, Iter, 0)),
--           To_Lower(To_String(SearchFor)), 1) >
--        0 then
--         return True;
--      end if;
--      return False;
--   end VisiblePrograms;
--
--   -- ****if* ProgramsMenu/SearchProgram
--   -- FUNCTION
--   -- Start searching for the selected name in the applications list
--   -- PARAMETERS
--   -- Self - Gtk_Search_Entry from which searched text will be taken
--   -- SOURCE
--   procedure SearchProgram(Self: access Gtk_Search_Entry_Record'Class) is
--   -- ****
--   begin
--      SearchFor := To_Unbounded_String(Get_Text(Self));
--      Refilter(ProgramsFilter);
--   end SearchProgram;
--
--   -- ****if* ProgramsMenu/SetProgram
--   -- FUNCTION
--   -- Set selected application as a associated application with selected
--   -- MIME type
--   -- PARAMETERS
--   -- Self   - Gtk_Tree_View with list of applications. Ununsed.
--   -- Path   - Gtk_Tree_Path to selected application
--   -- Column - Gtk_Tree_View_Column clicked. Ununsed.
--   -- SOURCE
--   procedure SetProgram
--     (Self: access Gtk_Tree_View_Record'Class; Path: Gtk_Tree_Path;
--      Column: not null access Gtk_Tree_View_Column_Record'Class) is
--      pragma Unreferenced(Self, Column);
--      -- ****
--      Pid: GNAT.OS_Lib.Process_Id;
--      ExecutableName: constant String := FindExecutable("xdg-mime");
--      ProgramIter: constant Gtk_Tree_Iter := Get_Iter(ProgramsFilter, Path);
--   begin
--      if ExecutableName = "" then
--         return;
--      end if;
--      Pid :=
--        Non_Blocking_Spawn
--          (ExecutableName,
--           Argument_String_To_List
--             ("default " & Get_String(ProgramsFilter, ProgramIter, 1) & " " &
--              GetMimeType(To_String(CurrentSelected))).all);
--      if Pid = GNAT.OS_Lib.Invalid_Pid then
--         ShowMessage(Gettext("Could not set new associated program."));
--      else
--         Set_Label(MenuButton, Get_String(ProgramsFilter, ProgramIter, 0));
--      end if;
--      Set_Active(MenuButton, False);
--   end SetProgram;
--
--   -- ****iv* ProgramsMenu/FileName
--   -- FUNCTION
--   -- Name of application which was found, or desktop file when nothing found.
--   -- SOURCE
--   FileName: Unbounded_String;
--   -- ****
--
--   -- ****if* ProgramsMenu/FindProgramName
--   -- FUNCTION
--   -- Search for selected desktop file and return program name if found
--   -- PARAMETERS
--   -- Model - Gtk_Tree_Model with list of know applications
--   -- Path  - Gtk_Tree_Path to selected element in Model
--   -- Iter  - Gtk_Tree_Iter to selected element in Model
--   -- RESULT
--   -- True if application found, otherwise False
--   -- SOURCE
--   function FindProgramName
--     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
--      return Boolean is
--      pragma Unreferenced(Path);
--      -- ****
--   begin
--      if Get_String(Model, Iter, 1) = To_String(FileName) then
--         FileName := To_Unbounded_String(Get_String(Model, Iter, 0));
--         return True;
--      end if;
--      return False;
--   end FindProgramName;
--
end ProgramsMenu;
