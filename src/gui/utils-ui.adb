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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bookmarks; use Bookmarks;
with Messages; use Messages;
with Preferences; use Preferences;

package body Utils.UI is

   -- ****iv* Utils/Utils.Progress_Index
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   Progress_Index: Natural;
   -- ****

   function Find_Executable
     (Name: String; Display_Message: Boolean := True) return String is
      Executable_Path: GNAT.OS_Lib.String_Access;
   begin
      if Exists
          (Name =>
             Containing_Directory(Name => Command_Name) & "/" & Name) then
         return Containing_Directory(Name => Command_Name) & "/" & Name;
      end if;
      Executable_Path := Locate_Exec_On_Path(Exec_Name => Name);
      if Executable_Path = null then
         if Display_Message then
            ShowMessage
              (Message =>
                 Mc
                   (Interp => Get_Context,
                    Src_String => "{Could not found executable:}") &
                 " " & Name);
         end if;
         return "";
      end if;
      return Executable_Path.all;
   end Find_Executable;

   procedure Update_Progress_Bar(Amount: Natural := 0) is
      Progress_Bar: constant Ttk_ProgressBar :=
        Get_Widget(pathName => ".mainframe.progressbar");
   begin
      if Amount > 0 then
         configure
           (Widgt => Progress_Bar,
            options => "-maximum" & Positive'Image(Amount) & " -value 0");
         Progress_Index := 0;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options => "-column 0 -row 1 -sticky we -columnspan 2");
         return;
      end if;
      Progress_Index := Progress_Index + 1;
      configure
        (Widgt => Progress_Bar,
         options => "-value" & Natural'Image(Progress_Index));
      if cget(Widgt => Progress_Bar, option => "-value") =
        cget(Widgt => Progress_Bar, option => "-maximum") then
         Grid_Remove(Slave => Progress_Bar);
      end if;
   end Update_Progress_Bar;

   procedure Set_Dialog
     (Dialog: Tk_Toplevel; Dialog_Title: String; Width: Width_Range;
      Height: Height_Range) is
      X: Width_Range;
      Y: Height_Range;
   begin
      Wm_Set
        (Widgt => Dialog, Action => "title",
         Options => "{" & Dialog_Title & "}");
      Wm_Set(Widgt => Dialog, Action => "transient", Options => ".");
      if Tcl_GetVar(interp => Get_Context, varName => "tcl_platform(os)") =
        "Linux" then
         Wm_Set
           (Widgt => Dialog, Action => "attributes",
            Options => "-type dialog");
      end if;
      X :=
        (Width_Range'Value(Winfo_Get(Widgt => Dialog, Info => "vrootwidth")) -
         Width) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Height_Range'Value
           (Winfo_Get(Widgt => Dialog, Info => "vrootheight")) -
         Height) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Widgt => Dialog, Action => "geometry",
         Options =>
           Trim(Source => Width_Range'Image(Width), Side => Both) & "x" &
           Trim(Source => Height_Range'Image(Height), Side => Both) & "+" &
           Trim(Source => Width_Range'Image(X), Side => Both) & "+" &
           Trim(Source => Height_Range'Image(Y), Side => Both));
      Bind
        (Widgt => Dialog, Sequence => "<Destroy>",
         Script => "{CloseDialog " & Value(Item => Dialog.Name) & "}");
   end Set_Dialog;

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (interp => Get_Context, cmdName => Name, proc => Ada_Command,
           data => 0, deleteProc => null);
      if Command = null then
         raise Hunter_Add_Command_Exception
           with Mc
             (Interp => Get_Context, Src_String => "{Can't add command}") &
           " " & Name;
      end if;
   end Add_Command;

   procedure Toggle_Tool_Buttons
     (Action: Item_Actions; Finished: Boolean := False) is
      Toolbar: Ttk_Frame := Get_Widget(pathName => ".mainframe.toolbars");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => ".mainframe.paned");
      Header_Label: constant Ttk_Label :=
        Get_Widget(pathName => ".mainframe.headerlabel");
      Buttons_Names: constant array(1 .. 7) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "new"),
         2 => To_Unbounded_String(Source => "rename"),
         3 => To_Unbounded_String(Source => "copy"),
         4 => To_Unbounded_String(Source => "move"),
         5 => To_Unbounded_String(Source => "delete"),
         6 => To_Unbounded_String(Source => "about"),
         7 => To_Unbounded_String(Source => "options"));
      Current_Button: Positive := 1;
      Delete_Menu: constant Tk_Menu := Get_Widget(pathName => ".deletemenu");
      Side: Unbounded_String := Null_Unbounded_String;
   begin
      case Action is
         when CREATEFILE | CREATEDIRECTORY | CREATELINK | RENAME | DELETE |
           DELETETRASH =>
            if Finished then
               Tcl.Tk.Ada.Grid.Grid(Slave => Toolbar);
               if Action = CREATELINK then
                  Toolbar.Name :=
                    New_String
                      (Str => ".mainframe.paned.previewframe.pathframe");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
               end if;
            else
               Grid_Remove(Slave => Toolbar);
            end if;
         when COPY =>
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.cancelbutton");
            if not Finished then
               Add
                 (Widget => Toolbar,
                  Message =>
                    Mc
                      (Interp => Get_Context,
                       Src_String =>
                         "{Stop copying files and directories \[Escape\]}"));
            end if;
            Current_Button := 3;
         when MOVE =>
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.cancelbutton");
            if not Finished then
               Add
                 (Widget => Toolbar,
                  Message =>
                    Mc
                      (Interp => Get_Context,
                       Src_String =>
                         "{Stop moving files and directories \[Escape\]}"));
            end if;
            Current_Button := 4;
         when SHOWTRASH =>
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.restorebutton");
            Current_Button := 5;
         when others =>
            return;
      end case;
      if Action in COPY | MOVE then
         Side :=
           (if not Settings.Toolbars_On_Top then
              To_Unbounded_String(Source => "top")
            else To_Unbounded_String(Source => "left"));
         if Finished then
            Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
            Toolbar.Name :=
              New_String(Str => ".mainframe.paned.previewframe.pathframe");
            Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
            Update_Buttons_Loop :
            for I in Buttons_Names'Range loop
               if I < Current_Button then
                  Toolbar.Name :=
                    New_String
                      (Str =>
                         ".mainframe.toolbars.actiontoolbar." &
                         To_String(Source => Buttons_Names(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Toolbar,
                     Options =>
                       "-before .mainframe.toolbars.actiontoolbar." &
                       To_String(Source => Buttons_Names(Current_Button)) &
                       "button");
                  Tcl.Tk.Ada.Pack.Pack_Configure
                    (Slave => Toolbar,
                     Options => "-side " & To_String(Source => Side));
               elsif I > Current_Button then
                  Toolbar.Name :=
                    New_String
                      (Str =>
                         ".mainframe.toolbars.actiontoolbar." &
                         To_String(Source => Buttons_Names(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Toolbar,
                     Options =>
                       "-after .mainframe.toolbars.actiontoolbar." &
                       To_String(Source => Buttons_Names(I - 1)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Configure
                    (Slave => Toolbar,
                     Options => "-side " & To_String(Source => Side));
               end if;
            end loop Update_Buttons_Loop;
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.separator3");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Slave => Toolbar,
               Options =>
                 "-after .mainframe.toolbars.actiontoolbar.deletebutton");
         else
            Tcl.Tk.Ada.Pack.Pack
              (Slave => Toolbar,
               Options =>
                 "-after .mainframe.toolbars.actiontoolbar.movebutton");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Slave => Toolbar,
               Options => "-side " & To_String(Source => Side));
            Remove_Buttons_Loop :
            for I in Buttons_Names'Range loop
               if I = Current_Button then
                  goto End_Of_Remove_Buttons_Loop;
               end if;
               Toolbar.Name :=
                 New_String
                   (Str =>
                      ".mainframe.toolbars.actiontoolbar." &
                      To_String(Source => Buttons_Names(I)) & "button");
               Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
               <<End_Of_Remove_Buttons_Loop>>
            end loop Remove_Buttons_Loop;
         end if;
      end if;
      if Action = SHOWTRASH then
         if Finished then
            Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.separator3");
            if Winfo_Get(Widgt => Toolbar, Info => "ismapped") = "1" then
               Update_Trash_Button_Loop :
               for I in Buttons_Names'Range loop
                  if I < Current_Button then
                     Toolbar.Name :=
                       New_String
                         (Str =>
                            ".mainframe.toolbars.actiontoolbar." &
                            To_String(Source => Buttons_Names(I)) & "button");
                     Tcl.Tk.Ada.Pack.Pack
                       (Slave => Toolbar,
                        Options =>
                          "-before .mainframe.toolbars.actiontoolbar." &
                          To_String(Source => Buttons_Names(Current_Button)) &
                          "button");
                  elsif I > Current_Button then
                     Toolbar.Name :=
                       New_String
                         (Str =>
                            ".mainframe.toolbars.actiontoolbar." &
                            To_String(Source => Buttons_Names(I)) & "button");
                     Tcl.Tk.Ada.Pack.Pack
                       (Slave => Toolbar,
                        Options =>
                          "-after .mainframe.toolbars.actiontoolbar." &
                          To_String(Source => Buttons_Names(I - 1)) &
                          "button");
                  end if;
               end loop Update_Trash_Button_Loop;
            else
               if Settings.Toolbars_On_Top then
                  configure(Widgt => Toolbar, options => "-orient vertical");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Toolbar,
                     Options => "-fill y -pady 5 -side left");
               else
                  configure(Widgt => Toolbar, options => "-orient horizontal");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Toolbar, Options => "-fill x -padx 5 -side top");
               end if;
               Add_Buttons_Loop :
               for Button_Name of Buttons_Names loop
                  Toolbar.Name :=
                    New_String
                      (Str =>
                         ".mainframe.toolbars.actiontoolbar." &
                         To_String(Source => Button_Name) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Toolbar,
                     Options =>
                       "-before .mainframe.toolbars.actiontoolbar.separator3");
               end loop Add_Buttons_Loop;
            end if;
            Toolbar.Name :=
              New_String
                (Str => ".mainframe.toolbars.actiontoolbar.separator3");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Slave => Toolbar,
               Options =>
                 "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            Delete
              (MenuWidget => Delete_Menu, StartIndex => "1",
               EndIndex => "end");
            if not Settings.Delete_Files then
               Entry_Configure
                 (MenuWidget => Delete_Menu, Index => "0",
                  Options =>
                    "-label {" &
                    Mc(Interp => Get_Context,
                       Src_String => "{Move selected to Trash}") &
                    "}");
            end if;
            Menu.Add
              (MenuWidget => Delete_Menu, EntryType => "command",
               Options =>
                 "-label {" &
                 Mc(Interp => Get_Context, Src_String => "{Show Trash}") &
                 "} -command ShowTrash");
            Menu.Add
              (MenuWidget => Delete_Menu, EntryType => "command",
               Options =>
                 "-label {" &
                 Mc(Interp => Get_Context, Src_String => "{Empty Trash}") &
                 "} -command ClearTrash");
         else
            Tcl.Tk.Ada.Pack.Pack
              (Slave => Toolbar,
               Options =>
                 "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            Remove_Trash_Buttons_Loop :
            for I in Buttons_Names'Range loop
               if I /= Current_Button then
                  Toolbar.Name :=
                    New_String
                      (Str =>
                         ".mainframe.toolbars.actiontoolbar." &
                         To_String(Source => Buttons_Names(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Toolbar);
               end if;
            end loop Remove_Trash_Buttons_Loop;
            Entry_Configure
              (MenuWidget => Delete_Menu, Index => "0",
               Options =>
                 "-label {" &
                 Mc(Interp => Get_Context, Src_String => "{Delete selected}") &
                 "}");
            Delete(MenuWidget => Delete_Menu, StartIndex => "1");
         end if;
      end if;
      Toolbar.Name := New_String(Str => ".mainframe.toolbars.itemtoolbar");
      if Finished then
         Grid_Remove(Slave => Header_Label);
         Tcl.Tk.Ada.Grid.Grid(Slave => Toolbar);
         if Settings.Show_Preview then
            SetBookmarkButton;
         else
            Toolbar.Name := New_String(Str => ".mainframe.paned.previewframe");
            Forget(Paned => Paned, SubWindow => Toolbar);
         end if;
      else
         if Action /= SHOWTRASH then
            Grid_Remove(Slave => Toolbar);
         end if;
         case Action is
            when CREATEFILE =>
               configure
                 (Header_Label,
                  "-text {" & Mc(Get_Context, "{Creating empty file}") & "}");
            when CREATEDIRECTORY =>
               configure
                 (Header_Label,
                  "-text {" & Mc(Get_Context, "{Creating new directory}") &
                  "}");
            when CREATELINK =>
               configure
                 (Header_Label,
                  "-text {" & Mc(Get_Context, "{Creating new link}") & "}");
            when RENAME =>
               configure
                 (Header_Label,
                  "-text {" & Mc(Get_Context, "{Renaming file or directory}") &
                  "}");
            when COPY =>
               configure
                 (Header_Label,
                  "-text {" &
                  Mc(Get_Context, "{Copying files and directories}") & "}");
            when MOVE =>
               configure
                 (Header_Label,
                  "-text {" &
                  Mc(Get_Context, "{Moving files and directories}") & "}");
            when DELETE | DELETETRASH =>
               if Settings.Delete_Files or Action = DELETETRASH then
                  configure
                    (Header_Label,
                     "-text {" &
                     Mc(Get_Context, "{Deleting files and directories}") &
                     "}");
               else
                  configure
                    (Header_Label,
                     "-text {" &
                     Mc(Get_Context,
                        "{Moving files and directories to trash}") &
                     "}");
               end if;
            when SHOWTRASH =>
               configure
                 (Header_Label,
                  "-text {" & Mc(Get_Context, "{Trash can}") & "}");
            when others =>
               null;
         end case;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Header_Label,
            Options => "-column 0 -row 0 -columnspan 2");
      end if;
   end Toggle_Tool_Buttons;

end Utils.UI;
