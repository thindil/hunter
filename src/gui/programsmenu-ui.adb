-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip;
with Common;
with Messages.UI;
with Utils; use Utils;

package body ProgramsMenu.UI is

   function Toggle_Applications_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tcl.Tk.Ada.Winfo;

      Applications_Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.applicationsmenu",
           Interp => Interp);
      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => Applications_Frame & ".searchentry", Interp => Interp);
   begin
      if Winfo_Get(Widgt => Applications_Frame, Info => "ismapped") = "0" then
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Applications_Frame,
            Options => "-column 1 -row 5 -rowspan 3");
         Focus(Widgt => Text_Entry);
      else
         Grid_Forget(Slave => Applications_Frame);
      end if;
      return TCL_OK;
   end Toggle_Applications_Menu_Command;

   -- ****o* ProgramsMenu/ProgramsMenu.Search_Program_Command
   -- FUNCTION
   -- Search the programs menu for the selected text (case insensitive) and
   -- show only matching applications
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchProgram
   -- SOURCE
   function Search_Program_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Program_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;

      Text_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.applicationsmenu.searchentry",
           Interp => Interp);
      Programs_Tree: constant Ttk_Tree_View :=
        Get_Widget
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.applicationsmenu.tree",
           Interp => Interp);
      Query: Unbounded_String;
      Selected: Boolean := False;
   begin
      Query := To_Unbounded_String(Source => Get(Widgt => Text_Entry));
      Search_Program_Loop :
      for I in Names_List.First_Index .. Names_List.Last_Index loop
         if Query /= Null_Unbounded_String
           and then
             Index
               (Source => To_Lower(Item => To_String(Source => Names_List(I))),
                Pattern => To_Lower(Item => To_String(Source => Query))) =
             0 then
            Detach
              (TreeViewWidget => Programs_Tree,
               ItemsList => Positive'Image(I));
         else
            Move
              (TreeViewWidget => Programs_Tree, Item => Positive'Image(I),
               Parent => "{}", Index => Natural'Image(I - 1));
            if not Selected then
               Selection_Set
                 (TreeViewWidget => Programs_Tree, Items => Positive'Image(I));
               Selected := True;
            end if;
         end if;
      end loop Search_Program_Loop;
      return TCL_OK;
   end Search_Program_Command;

   -- ****o* ProgramsMenu/ProgramsMenu.Set_Application_Command
   -- FUNCTION
   -- Set the selected application as a default application to open the
   -- selected mime type items
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetApplication
   -- SOURCE
   function Set_Application_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Application_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use GNAT.OS_Lib;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Common;
      use Messages.UI;

      Applications_View: constant Ttk_Tree_View :=
        Get_Widget
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.applicationsmenu.tree",
           Interp => Interp);
      Pid: Process_Id := GNAT.OS_Lib.Invalid_Pid;
      Executable_Name: constant String := Find_Executable(Name => "xdg-mime");
      Application_Name: Unbounded_String;
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.associatedprogram",
           Interp => Interp);
   begin
      if Executable_Name = "" then
         return TCL_OK;
      end if;
      Application_Name :=
        To_Unbounded_String
          (Source =>
             Item
               (TreeViewWidget => Applications_View,
                Item => Selection(TreeViewWidget => Applications_View),
                Options => "-text"));
      Set_New_Application_Loop :
      for I in Applications_List.Iterate loop
         if Applications_List(I) = Application_Name then
            Pid :=
              Non_Blocking_Spawn
                (Program_Name => Executable_Name,
                 Args =>
                   Argument_String_To_List
                     (Arg_String =>
                        "default " & Bookmarks_Container.Key(Position => I) &
                        " " &
                        Get_Mime_Type
                          (File_Name =>
                             To_String(Source => Current_Selected))).all);
            if Pid = GNAT.OS_Lib.Invalid_Pid then
               Show_Message
                 (Message =>
                    Mc
                      (Interp => Interp,
                       Src_String =>
                         "{Could not set new associated program.}"));
            else
               configure
                 (Widgt => Button,
                  options => "-text {" & Applications_List(I) & "}");
            end if;
            exit Set_New_Application_Loop;
         end if;
      end loop Set_New_Application_Loop;
      return
        Toggle_Applications_Menu_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Set_Application_Command;

   -- ****o* ProgramsMenu/ProgramsMenu.Hide_On_Focus_Out_Command
   -- FUNCTION
   -- If application menu lost focus, hide it
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- HideOnFocusOut
   -- SOURCE
   function Hide_On_Focus_Out_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_On_Focus_Out_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
   begin
      if Focus(Interp => Interp) in
          ".mainframe.paned.previewframe.infoframe.applicationsmenu.searchentry" |
            ".mainframe.paned.previewframe.infoframe.applicationsmenu.tree" |
            ".mainframe.paned.previewframe.infoframe.associatedprogram" then
         return TCL_OK;
      end if;
      return
        Toggle_Applications_Menu_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
           Argv => Argv);
   end Hide_On_Focus_Out_Command;

   procedure Create_Programs_Menu_Ui is
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tklib.Ada.Autoscroll;
      use Tcl.Tklib.Ada.Tooltip;

      Applications_Frame: constant Ttk_Frame :=
        Create
          (pathName =>
             ".mainframe.paned.previewframe.infoframe.applicationsmenu",
           options => "-width 200 -height 400");
      Search_Entry: constant Ttk_Entry :=
        Create(pathName => Applications_Frame & ".searchentry");
      Applications_View: constant Ttk_Tree_View :=
        Create
          (pathName => Applications_Frame & ".tree",
           options =>
             "-show tree -yscrollcommand {" & Applications_Frame &
             ".scrolly set}");
      Applications_Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Applications_Frame & ".scrolly",
           options =>
             "-orient vertical -command [list " & Applications_View &
             " yview]");
   begin
      Autoscroll(Scroll => Applications_Y_Scroll);
      Create_Programs_Menu;
      Fill_Applications_List_Loop :
      for I in Names_List.First_Index .. Names_List.Last_Index loop
         Insert
           (TreeViewWidget => Applications_View,
            Options =>
              "{} end -id" & Positive'Image(I) & " -text {" &
              To_String(Source => Names_List(I)) & "}");
      end loop Fill_Applications_List_Loop;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Search_Entry, Options => "-columnspan 2 -sticky we");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Applications_View, Options => "-column 0 -row 1 -sticky we");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Applications_Y_Scroll,
         Options => "-column 1 -row 1 -sticky ns");
      Row_Configure
        (Master => Applications_Frame, Slave => Applications_View,
         Options => "-weight 1");
      Add_Command
        (Name => "ToggleApplicationsMenu",
         Ada_Command => Toggle_Applications_Menu_Command'Access);
      Add_Command
        (Name => "SearchProgram",
         Ada_Command => Search_Program_Command'Access);
      Add_Command
        (Name => "SetApplication",
         Ada_Command => Set_Application_Command'Access);
      Add_Command
        (Name => "HideOnFocusOut",
         Ada_Command => Hide_On_Focus_Out_Command'Access);
      Bind
        (Widgt => Search_Entry, Sequence => "<KeyRelease>",
         Script => "{SearchProgram}");
      Bind
        (Widgt => Search_Entry, Sequence => "<FocusOut>",
         Script => "{HideOnFocusOut}");
      Bind
        (Widgt => Applications_View, Sequence => "<Double-1>",
         Script => "{SetApplication}");
      Bind
        (Widgt => Applications_View, Sequence => "<Return>",
         Script => "{SetApplication}");
      Bind
        (Widgt => Applications_View, Sequence => "<FocusOut>",
         Script => "{HideOnFocusOut}");
      Add
        (Widget => Search_Entry,
         Message =>
           Mc(Interp => Get_Context, Src_String => "{Search for a program}"));
      Add
        (Widget => Applications_View,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Press enter or double click to set the selected program as associated with that type of file or directory.}"));
   end Create_Programs_Menu_Ui;

end ProgramsMenu.UI;
