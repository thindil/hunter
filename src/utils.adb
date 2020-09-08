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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with LibMagic; use LibMagic;
with Tcl; use Tcl;
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

package body Utils is

   -- ****iv* Utils/ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Natural;
   -- ****

   function GetMimeType(FileName: String) return String is
   begin
      return MagicFile(FileName);
   end GetMimeType;

   function CanBeOpened(MimeType: String) return Boolean is
      ExecutableName: constant String := FindExecutable("xdg-mime");
      Success: Boolean;
   begin
      if ExecutableName = "" then
         return False;
      end if;
      Spawn
        (ExecutableName,
         Argument_String_To_List("query default " & MimeType).all, Success);
      if not Success then
         return False;
      end if;
      return True;
   end CanBeOpened;

   function CountFileSize(Size: File_Size) return String is
      Multiplier: Natural;
      NewSize: File_Size;
      SizeShortcuts: constant array(Natural range <>) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
   begin
      NewSize := Size;
      Multiplier := 0;
      while NewSize > 1024 loop
         NewSize := NewSize / 1024;
         Multiplier := Multiplier + 1;
      end loop;
      return File_Size'Image(NewSize) & " " & SizeShortcuts(Multiplier);
   end CountFileSize;

   function FindExecutable
     (Name: String; DisplayMessage: Boolean := True) return String is
      ExecutablePath: GNAT.OS_Lib.String_Access;
   begin
      if Exists(Containing_Directory(Command_Name) & "/" & Name) then
         return Containing_Directory(Command_Name) & "/" & Name;
      end if;
      ExecutablePath := Locate_Exec_On_Path(Name);
      if ExecutablePath = null then
         if DisplayMessage then
            ShowMessage
              (Mc(Get_Context, "{Could not found executable: }") & Name);
         end if;
         return "";
      end if;
      return ExecutablePath.all;
   end FindExecutable;

   procedure SetProgressBar(Amount: Positive) is
      ProgressBar: Ttk_ProgressBar;
   begin
      ProgressBar.Interp := Get_Context;
      ProgressBar.Name := New_String(".mainframe.progressbar");
      configure
        (ProgressBar, "-maximum" & Positive'Image(Amount) & " -value 0");
      ProgressIndex := 0;
      Tcl.Tk.Ada.Grid.Grid
        (ProgressBar, "-column 0 -row 1 -sticky we -columnspan 2");
   end SetProgressBar;

   procedure UpdateProgressBar is
      ProgressBar: Ttk_ProgressBar;
   begin
      ProgressIndex := ProgressIndex + 1;
      ProgressBar.Interp := Get_Context;
      ProgressBar.Name := New_String(".mainframe.progressbar");
      configure(ProgressBar, "-value" & Natural'Image(ProgressIndex));
      if cget(ProgressBar, "-value") = cget(ProgressBar, "-maximum") then
         Grid_Remove(ProgressBar);
      end if;
   end UpdateProgressBar;

   procedure SetDialog
     (Dialog: Tk_Toplevel; DialogTitle: String; Width, Height: Positive) is
      X, Y: Integer;
   begin
      Wm_Set(Dialog, "title", "{" & DialogTitle & "}");
      Wm_Set(Dialog, "transient", ".");
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(Dialog, "attributes", "-type dialog");
      end if;
      X := (Positive'Value(Winfo_Get(Dialog, "vrootwidth")) - Width) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y := (Positive'Value(Winfo_Get(Dialog, "vrootheight")) - Height) / 2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Dialog, "geometry",
         Trim(Positive'Image(Width), Both) & "x" &
         Trim(Positive'Image(Height), Both) & "+" &
         Trim(Positive'Image(X), Both) & "+" & Trim(Positive'Image(Y), Both));
      Bind(Dialog, "<Destroy>", "{CloseDialog " & Value(Dialog.Name) & "}");
   end SetDialog;

   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      Hunter_Add_Command_Exception: exception;
   begin
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, AdaCommand, 0, null);
      if Command = null then
         raise Hunter_Add_Command_Exception
           with Mc(Get_Context, "{Can't add command }") & Name;
      end if;
   end AddCommand;

   procedure ToggleToolButtons
     (Action: ItemActions; Finished: Boolean := False) is
      Toolbar: Ttk_Frame;
      Paned: Ttk_PanedWindow;
      HeaderLabel: Ttk_Label;
      ButtonsNames: constant array(1 .. 7) of Unbounded_String :=
        (To_Unbounded_String("new"), To_Unbounded_String("rename"),
         To_Unbounded_String("copy"), To_Unbounded_String("move"),
         To_Unbounded_String("delete"), To_Unbounded_String("about"),
         To_Unbounded_String("options"));
      CurrentButton: Positive;
      DeleteMenu: Tk_Menu;
      Side: Unbounded_String;
   begin
      Toolbar.Interp := Get_Context;
      Toolbar.Name := New_String(".mainframe.toolbars");
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".mainframe.paned");
      HeaderLabel.Interp := Get_Context;
      HeaderLabel.Name := New_String(".mainframe.headerlabel");
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
      if (Action = COPY or Action = MOVE) then
         if not Settings.ToolbarsOnTop then
            Side := To_Unbounded_String("top");
         else
            Side := To_Unbounded_String("left");
         end if;
         if Finished then
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
            Toolbar.Name :=
              New_String(".mainframe.paned.previewframe.pathframe");
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
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
            end loop;
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
            for I in ButtonsNames'Range loop
               if I /= CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
               end if;
            end loop;
         end if;
      end if;
      if Action = SHOWTRASH then
         DeleteMenu.Interp := Get_Context;
         DeleteMenu.Name := New_String(".deletemenu");
         if not Finished then
            Tcl.Tk.Ada.Pack.Pack
              (Toolbar,
               "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            for I in ButtonsNames'Range loop
               if I /= CurrentButton then
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
               end if;
            end loop;
            Entry_Configure
              (DeleteMenu, "0",
               "-label {" & Mc(Get_Context, "{Delete selected}") & "}");
            Delete(DeleteMenu, "1");
         else
            Tcl.Tk.Ada.Pack.Pack_Forget(Toolbar);
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.separator3");
            if Winfo_Get(Toolbar, "ismapped") = "1" then
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
               end loop;
            else
               if not Settings.ToolbarsOnTop then
                  configure(Toolbar, "-orient horizontal");
                  Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill x -padx 5 -side top");
               else
                  configure(Toolbar, "-orient vertical");
                  Tcl.Tk.Ada.Pack.Pack(Toolbar, "-fill y -pady 5 -side left");
               end if;
               for I in ButtonsNames'Range loop
                  Toolbar.Name :=
                    New_String
                      (".mainframe.toolbars.actiontoolbar." &
                       To_String(ButtonsNames(I)) & "button");
                  Tcl.Tk.Ada.Pack.Pack
                    (Toolbar,
                     "-before .mainframe.toolbars.actiontoolbar.separator3");
               end loop;
            end if;
            Toolbar.Name :=
              New_String(".mainframe.toolbars.actiontoolbar.separator3");
            Tcl.Tk.Ada.Pack.Pack_Configure
              (Toolbar,
               "-after .mainframe.toolbars.actiontoolbar.deletebutton");
            if not Settings.DeleteFiles then
               Entry_Configure
                 (DeleteMenu, "0",
                  "-label {" & Mc(Get_Context, "{Move selected to Trash}") &
                  "}");
            end if;
            Insert
              (DeleteMenu, "1", "command",
               "-label {" & Mc(Get_Context, "{Show Trash}") &
               "} -command ShowTrash");
         end if;
      end if;
      Toolbar.Name := New_String(".mainframe.toolbars.itemtoolbar");
      if Finished then
         Grid_Remove(HeaderLabel);
         Tcl.Tk.Ada.Grid.Grid(Toolbar);
         if not Settings.ShowPreview then
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
               if Settings.DeleteFiles or Action = DELETETRASH then
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
   end ToggleToolButtons;

end Utils;
