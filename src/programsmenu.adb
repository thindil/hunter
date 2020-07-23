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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bookmarks; use Bookmarks;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Utils; use Utils;

package body ProgramsMenu is

   -- ****iv* ProgramsMenu/ApplicationsList
   -- FUNCTION
   -- List of all applications which can be used to execute files or
   -- directories
   -- SOURCE
   ApplicationsList: Bookmarks_Container.Map;
   -- ****

   -- ****iv* ProgramsMenu/NamesList
   -- FUNCTION
   -- List of all applications showed in the menu
   -- SOURCE
   NamesList: UnboundedString_Container.Vector;
   -- ****

   -- ****it* ProgramsMenu/Programs_Sorting
   -- FUNCTION
   -- Used in sorting available programs
   -- SOURCE
   package Programs_Sorting is new UnboundedString_Container.Generic_Sorting;
   -- ****

   -- ****if* ProgramsMenu/Toggle_Applications_Menu_Command
   -- FUNCTION
   -- Show or hide menu which allow to set a application which can be used
   -- to execute the selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Toggle_Applications_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_Applications_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ApplicationsFrame: Ttk_Frame;
      TextEntry: Ttk_Entry;
   begin
      ApplicationsFrame.Interp := Interp;
      ApplicationsFrame.Name :=
        New_String(".mainframe.paned.previewframe.infoframe.applicationsmenu");
      if Winfo_Get(ApplicationsFrame, "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid
           (ApplicationsFrame, "-column 1 -row 5 -rowspan 3");
         TextEntry.Interp := Interp;
         TextEntry.Name :=
           New_String
             (".mainframe.paned.previewframe.infoframe.applicationsmenu.searchentry");
         Focus(TextEntry);
      else
         Grid_Forget(ApplicationsFrame);
      end if;
      return TCL_OK;
   end Toggle_Applications_Menu_Command;

   -- ****if* ProgramsMenu/Search_Program_Command
   -- FUNCTION
   -- Search the programs menu for the selected text (case insensitive) and
   -- show only matching applications
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Search_Program_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Program_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      TextEntry: Ttk_Entry;
      ProgramsTree: Ttk_Tree_View;
      Query: Unbounded_String;
      Selected: Boolean := False;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name :=
        New_String
          (".mainframe.paned.previewframe.infoframe.applicationsmenu.searchentry");
      Query := To_Unbounded_String(Get(TextEntry));
      ProgramsTree.Interp := Interp;
      ProgramsTree.Name :=
        New_String
          (".mainframe.paned.previewframe.infoframe.applicationsmenu.tree");
      for I in NamesList.First_Index .. NamesList.Last_Index loop
         if Query /= Null_Unbounded_String
           and then
             Index
               (To_Lower(To_String(NamesList(I))),
                To_Lower(To_String(Query))) =
             0 then
            Detach(ProgramsTree, Positive'Image(I));
         else
            Move(ProgramsTree, Positive'Image(I), "{}", Natural'Image(I - 1));
            if not Selected then
               Selection_Set(ProgramsTree, Positive'Image(I));
               Selected := True;
            end if;
         end if;
      end loop;
      return TCL_OK;
   end Search_Program_Command;

   -- ****if* ProgramsMenu/Set_Application_Command
   -- FUNCTION
   -- Set the selected application as a default application to open the
   -- selected mime type items
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Set_Application_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Application_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      ApplicationsView: Ttk_Tree_View;
      Pid: Process_Id;
      ExecutableName: constant String := FindExecutable("xdg-mime");
      ApplicationName: Unbounded_String;
      Button: Ttk_Button;
   begin
      if ExecutableName = "" then
         return TCL_OK;
      end if;
      ApplicationsView.Interp := Interp;
      ApplicationsView.Name :=
        New_String
          (".mainframe.paned.previewframe.infoframe.applicationsmenu.tree");
      ApplicationName :=
        To_Unbounded_String
          (Item(ApplicationsView, Selection(ApplicationsView), "-text"));
      Button.Interp := Interp;
      Button.Name :=
        New_String
          (".mainframe.paned.previewframe.infoframe.associatedprogram");
      for I in ApplicationsList.Iterate loop
         if ApplicationsList(I) = ApplicationName then
            Pid :=
              Non_Blocking_Spawn
                (ExecutableName,
                 Argument_String_To_List
                   ("default " & Bookmarks_Container.Key(I) & " " &
                    GetMimeType(To_String(CurrentSelected))).all);
            if Pid = GNAT.OS_Lib.Invalid_Pid then
               ShowMessage
                 (Mc(Interp, "{Could not set new associated program.}"));
            else
               configure(Button, "-text {" & ApplicationsList(I) & "}");
            end if;
            exit;
         end if;
      end loop;
      return Toggle_Applications_Menu_Command(ClientData, Interp, Argc, Argv);
   end Set_Application_Command;

   -- ****if* ProgramsMenu/Hide_On_Focus_Out_Command
   -- FUNCTION
   -- If application menu lost focus, hide it
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- SOURCE
   function Hide_On_Focus_Out_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_On_Focus_Out_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
   begin
      if Focus(Interp) in
          ".mainframe.paned.previewframe.infoframe.applicationsmenu.searchentry" |
            ".mainframe.paned.previewframe.infoframe.applicationsmenu.tree" |
            ".mainframe.paned.previewframe.infoframe.associatedprogram" then
         return TCL_OK;
      end if;
      return Toggle_Applications_Menu_Command(ClientData, Interp, Argc, Argv);
   end Hide_On_Focus_Out_Command;

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
      SubFileName: String(1 .. 1024);
      File: File_Type;
      FileLine: Unbounded_String;
      ApplicationsFrame: constant Ttk_Frame :=
        Create
          (".mainframe.paned.previewframe.infoframe.applicationsmenu",
           "-width 200 -height 400");
      SearchEntry: constant Ttk_Entry :=
        Create(Widget_Image(ApplicationsFrame) & ".searchentry");
      ApplicationsView: constant Ttk_Tree_View :=
        Create
          (Widget_Image(ApplicationsFrame) & ".tree",
           "-show tree -yscrollcommand {" & Widget_Image(ApplicationsFrame) &
           ".scrolly set}");
      ApplicationsYScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(ApplicationsFrame) & ".scrolly",
           "-orient vertical -command [list " &
           Widget_Image(ApplicationsView) & " yview]");
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
                    and then Slice(FileLine, 1, 5) = "Name=" then
                     ApplicationsList.Include
                       (SubFileName(1 .. SubLast),
                        Slice(FileLine, 6, Length(FileLine)));
                     if not NamesList.Contains
                         (Unbounded_Slice(FileLine, 6, Length(FileLine))) then
                        NamesList.Append
                          (Unbounded_Slice(FileLine, 6, Length(FileLine)));
                     end if;
                     exit;
                  end if;
               end loop;
               Close(File);
            end if;
         end loop;
         Close(SubDirectory);
         <<End_Of_Loop>>
      end loop;
      Programs_Sorting.Sort(NamesList);
      for I in NamesList.First_Index .. NamesList.Last_Index loop
         Insert
           (ApplicationsView,
            "{} end -id" & Positive'Image(I) & " -text {" &
            To_String(NamesList(I)) & "}");
      end loop;
      Tcl.Tk.Ada.Grid.Grid(SearchEntry, "-columnspan 2 -sticky we");
      Tcl.Tk.Ada.Grid.Grid(ApplicationsView, "-column 0 -row 1 -sticky we");
      Tcl.Tk.Ada.Grid.Grid(ApplicationsYScroll, "-column 1 -row 1 -sticky ns");
      Row_Configure(ApplicationsFrame, ApplicationsView, "-weight 1");
      AddCommand
        ("ToggleApplicationsMenu", Toggle_Applications_Menu_Command'Access);
      AddCommand("SearchProgram", Search_Program_Command'Access);
      AddCommand("SetApplication", Set_Application_Command'Access);
      AddCommand("HideOnFocusOut", Hide_On_Focus_Out_Command'Access);
      Bind(SearchEntry, "<KeyRelease>", "{SearchProgram}");
      Bind(SearchEntry, "<FocusOut>", "{HideOnFocusOut}");
      Bind(ApplicationsView, "<Double-1>", "{SetApplication}");
      Bind(ApplicationsView, "<Return>", "{SetApplication}");
      Bind(ApplicationsView, "<FocusOut>", "{HideOnFocusOut}");
      Add(SearchEntry, Mc(Get_Context, "{Search for a program}"));
      Add
        (ApplicationsView,
         Mc
           (Get_Context,
            "{Press enter or double click to set the selected program as associated with that type of file or directory.}"));
   end CreateProgramsMenu;

   function GetProgramName(DesktopFile: String) return String is
   begin
      if not ApplicationsList.Contains(DesktopFile) then
         return DesktopFile;
      end if;
      return ApplicationsList(DesktopFile);
   end GetProgramName;

end ProgramsMenu;
