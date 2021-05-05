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
      if Exists(Containing_Directory(Command_Name) & "/" & Name) then
         return Containing_Directory(Command_Name) & "/" & Name;
      end if;
      Executable_Path := Locate_Exec_On_Path(Name);
      if Executable_Path = null then
         if Display_Message then
            ShowMessage
              (Mc(Get_Context, "{Could not found executable:}") & " " & Name);
         end if;
         return "";
      end if;
      return Executable_Path.all;
   end Find_Executable;

   procedure Set_Progress_Bar(Amount: Positive) is
      Progress_Bar: constant Ttk_ProgressBar :=
        Get_Widget(".mainframe.Progress_Bar");
   begin
      configure
        (Progress_Bar, "-maximum" & Positive'Image(Amount) & " -value 0");
      Progress_Index := 0;
      Tcl.Tk.Ada.Grid.Grid
        (Progress_Bar, "-column 0 -row 1 -sticky we -columnspan 2");
   end Set_Progress_Bar;

   procedure Update_Progress_Bar is
      Progress_Bar: constant Ttk_ProgressBar :=
        Get_Widget(".mainframe.Progress_Bar");
   begin
      Progress_Index := Progress_Index + 1;
      configure(Progress_Bar, "-value" & Natural'Image(Progress_Index));
      if cget(Progress_Bar, "-value") = cget(Progress_Bar, "-maximum") then
         Grid_Remove(Progress_Bar);
      end if;
   end Update_Progress_Bar;

   procedure Set_Dialog
     (Dialog: Tk_Toplevel; Dialog_Title: String; Width: Width_Range;
      Height: Height_Range) is
      X: Width_Range;
      Y: Height_Range;
   begin
      Wm_Set(Dialog, "title", "{" & Dialog_Title & "}");
      Wm_Set(Dialog, "transient", ".");
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(Dialog, "attributes", "-type dialog");
      end if;
      X := (Width_Range'Value(Winfo_Get(Dialog, "vrootwidth")) - Width) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Height_Range'Value(Winfo_Get(Dialog, "vrootheight")) - Height) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Dialog, "geometry",
         Trim(Width_Range'Image(Width), Both) & "x" &
         Trim(Height_Range'Image(Height), Both) & "+" &
         Trim(Width_Range'Image(X), Both) & "+" &
         Trim(Height_Range'Image(Y), Both));
      Bind(Dialog, "<Destroy>", "{CloseDialog " & Value(Dialog.Name) & "}");
   end Set_Dialog;

   procedure Add_Command
     (Name: String; Ada_Command: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, Ada_Command, 0, null);
      if Command = null then
         raise Hunter_Add_Command_Exception
           with Mc(Get_Context, "{Can't add command}") & " " & Name;
      end if;
   end Add_Command;

   procedure Toggle_Tool_Buttons
     (Action: Item_Actions; Finished: Boolean := False) is
      Toolbar: Ttk_Frame := Get_Widget(".mainframe.toolbars");
      Paned: constant Ttk_PanedWindow := Get_Widget(".mainframe.paned");
      HeaderLabel: constant Ttk_Label := Get_Widget(".mainframe.headerlabel");
      ButtonsNames: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("new"), To_Unbounded_String("rename"),
         To_Unbounded_String("copy"), To_Unbounded_String("move"),
         To_Unbounded_String("delete"), To_Unbounded_String("about"),
         To_Unbounded_String("options"));
      CurrentButton: Positive;
      DeleteMenu: constant Tk_Menu := Get_Widget(".deletemenu");
      Side: Unbounded_String;
   begin
      case Action is
         when CREATEFILE | CREATEDIRECTORY | CREATELINK | RENAME | DELETE |
           DELETETRASH =>
            if not Finished then
               Grid_Remove(Toolbar);
            else
               Tcl.Tk.Ada.Grid.Grid(Toolbar);
               if Action = CREATELINK then
                  Toolbar.Name :=
                    New_String(".mainframe.paned.previewframe.pathframe");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
               end if;
            end if;
         when COPY =>
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.cancelbutton");
            if not Finished then
               Add
                 (Toolbar,
                  Mc
                    (Get_Context,
                     "{Stop copying files and directories \[Escape\]}"));
            end if;
            CurrentButton := 3;
         when MOVE =>
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.cancelbutton");
            if not Finished then
               Add
                 (Toolbar,
                  Mc
                    (Get_Context,
                     "{Stop moving files and directories \[Escape\]}"));
            end if;
            CurrentButton := 4;
         when SHOWTRASH =>
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.restorebutton");
            CurrentButton := 5;
         when others =>
            return;
      end case;
      if Action in COPY | MOVE then
         Side :=
           (if not Settings.Toolbars_On_Top then To_Unbounded_String("top")
            else To_Unbounded_String("left"));
         if Finished then
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
            Toolbar.Name :=
              New_String(".mainframe.paned.previewframe.pathframe");
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
            Update_Buttons_Loop :
            for I in ButtonsNames'Range loop
               if I < CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Toolbar,
                     "-before .mainframe.toolbars.actiontoolbar." &
                     To_String(ButtonsNames(CurrentButton)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Configure
                    (Toolbar, "-side " & To_String(Side));
               elsif I > CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Toolbar,
                     "-after .mainframe.toolbars.actiontoolbar." &
                     To_String(ButtonsNames(I - 1)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Configure
                    (Toolbar, "-side " & To_String(Side));
               end if;
            end loop Update_Buttons_Loop;
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.separator3");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Toolbar,
               "-after .mainframe.toolbars.actiontoolbar.deletebutton");
         else
            Tcl.Tk.Ada.Pack.Pack
              (Toolbar, "-after .mainframe.toolbars.actiontoolbar.movebutton");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Toolbar, "-side " & To_String(Side));
            Remove_Buttons_Loop :
            for I in ButtonsNames'Range loop
               if I /= CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
               end if;
            end loop Remove_Buttons_Loop;
         end if;
      end if;
      if Action = SHOWTRASH then
         if not Finished then
            Tcl.Tk.Ada.Pack.Pack
              (Toolbar,
               "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            Remove_Trash_Buttons_Loop :
            for I in ButtonsNames'Range loop
               if I /= CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
               end if;
            end loop Remove_Trash_Buttons_Loop;
            Entry_Configure
              (DeleteMenu, "0",
               "-label {" & Mc(Get_Context, "{Delete selected}") & "}");
            Delete(DeleteMenu, "1");
         else
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.separator3");
            if Winfo_Get(Toolbar, "ismapped") = "1" then
               Update_Trash_Button_Loop :
               for I in ButtonsNames'Range loop
                  if I < CurrentButton then
                     Toolbar.Name :=
                       New_String
                         (".mainframe.toolbars.actiontoolbar." &
                          To_String(ButtonsNames(I)) & "button");
                     Tcl.Tk.Ada.Pack.Pack
                       (Toolbar,
                        "-before .mainframe.toolbars.actiontoolbar." &
                        To_String(ButtonsNames(CurrentButton)) & "button");
                  elsif I > CurrentButton then
                     Toolbar.Name :=
                       New_String
                         (".mainframe.toolbars.actiontoolbar." &
                          To_String(ButtonsNames(I)) & "button");
                     Tcl.Tk.Ada.Pack.Pack
                       (Toolbar,
                        "-after .mainframe.toolbars.actiontoolbar." &
                        To_String(ButtonsNames(I - 1)) & "button");
                  end if;
               end loop Update_Trash_Button_Loop;
            else
               if not Settings.Toolbars_On_Top then
                  configure(Toolbar, "-orient horizontal");
                  Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill x -padx 5 -side top");
               else
                  configure(Toolbar, "-orient vertical");
                  Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill y -pady 5 -side left");
               end if;
               Add_Buttons_Loop :
               for I in ButtonsNames'Range loop
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Toolbar,
                     "-before .mainframe.toolbars.actiontoolbar.separator3");
               end loop Add_Buttons_Loop;
            end if;
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.separator3");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Toolbar,
               "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            Delete(DeleteMenu, "1", "end");
            if not Settings.Delete_Files then
               Entry_Configure
                 (DeleteMenu, "0",
                  "-label {" & Mc(Get_Context, "{Move selected to Trash}") &
                  "}");
            end if;
            Menu.Add
              (DeleteMenu, "command",
               "-label {" & Mc(Get_Context, "{Show Trash}") &
               "} -command ShowTrash");
            Menu.Add
              (DeleteMenu, "command",
               "-label {" & Mc(Get_Context, "{Empty Trash}") &
               "} -command ClearTrash");
         end if;
      end if;
      Toolbar.Name := New_String(".mainframe.toolbars.itemtoolbar");
      if Finished then
         Grid_Remove(HeaderLabel);
         Tcl.Tk.Ada.Grid.Grid(Toolbar);
         if not Settings.Show_Preview then
            Toolbar.Name := New_String(".mainframe.paned.previewframe");
            Forget(Paned, Toolbar);
         else
            SetBookmarkButton;
         end if;
      else
         if Action /= SHOWTRASH then
            Grid_Remove(Toolbar);
         end if;
         case Action is
            when CREATEFILE =>
               configure
                 (HeaderLabel,
                  "-text {" & Mc(Get_Context, "{Creating empty file}") & "}");
            when CREATEDIRECTORY =>
               configure
                 (HeaderLabel,
                  "-text {" & Mc(Get_Context, "{Creating new directory}") &
                  "}");
            when CREATELINK =>
               configure
                 (HeaderLabel,
                  "-text {" & Mc(Get_Context, "{Creating new link}") & "}");
            when RENAME =>
               configure
                 (HeaderLabel,
                  "-text {" & Mc(Get_Context, "{Renaming file or directory}") &
                  "}");
            when COPY =>
               configure
                 (HeaderLabel,
                  "-text {" &
                  Mc(Get_Context, "{Copying files and directories}") & "}");
            when MOVE =>
               configure
                 (HeaderLabel,
                  "-text {" &
                  Mc(Get_Context, "{Moving files and directories}") & "}");
            when DELETE | DELETETRASH =>
               if Settings.Delete_Files or Action = DELETETRASH then
                  configure
                    (HeaderLabel,
                     "-text {" &
                     Mc(Get_Context, "{Deleting files and directories}") &
                     "}");
               else
                  configure
                    (HeaderLabel,
                     "-text {" &
                     Mc(Get_Context,
                        "{Moving files and directories to trash}") &
                     "}");
               end if;
            when SHOWTRASH =>
               configure
                 (HeaderLabel,
                  "-text {" & Mc(Get_Context, "{Trash can}") & "}");
            when others =>
               null;
         end case;
         Tcl.Tk.Ada.Grid.Grid(HeaderLabel, "-column 0 -row 0 -columnspan 2");
      end if;
   end Toggle_Tool_Buttons;

end Utils.UI;
