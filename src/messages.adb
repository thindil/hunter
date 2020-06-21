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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with CopyItems; use CopyItems;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with Utils; use Utils;

package body Messages is

   -- ****iv* Messages/MessageFrame
   -- FUNCTION
   -- Main frame for the message widget
   -- SOURCE
   MessageFrame: Ttk_Frame;
   -- ****

   -- ****iv* Messages/MessageLabel
   -- FUNCTION
   -- Label which show message text
   -- SOURCE
   MessageLabel: Ttk_Label;
   -- ****

   -- ****iv* Messages/TimerId
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   TimerId: Unbounded_String := Null_Unbounded_String;
   -- ****

   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* Messages/Close_Command
      -- FUNCTION
      -- Hide message frame
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function Close_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
   begin
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      Grid_Remove(MessageFrame);
      return TCL_OK;
   end Close_Command;

   function Response_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* Messages/Response_Command
      -- FUNCTION
      -- Hide message frame and do action, depends on user response
      -- PARAMETERS
      -- ClientData - Custom data send to the command.
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command.
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Response_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      -- ****
      OverwriteItem: Boolean := True;
      Response: constant String := CArgv.Arg(Argv, 1);
   begin
      if Response = "yesall" then
         YesForAll := True;
      else
         YesForAll := False;
      end if;
      case NewAction is
         when DELETE | CLEARTRASH | DELETETRASH =>
            if NewAction /= CLEARTRASH then
               SetProgressBar(Positive(SelectedItems.Length));
            end if;
            if Response = "yes" then
               begin
                  if DeleteSelected then
                     CurrentDirectory :=
                       To_Unbounded_String
                         (Normalize_Pathname
                            (To_String(CurrentDirectory) & "/.."));
                  end if;
               exception
                  when others =>
                     LoadDirectory(To_String(CurrentDirectory));
                     UpdateDirectoryList(True);
                     return TCL_OK;
               end;
               if NewAction = CLEARTRASH then
                  Tcl.Ada.Tcl_Eval
                    (Get_Context, "GoToBookmark {" & Value("HOME") & "}");
--               elsif NewAction = DELETETRASH then
--                  ShowTrash(null);
--                  NewAction := DELETETRASH;
               else
                  LoadDirectory(To_String(CurrentDirectory));
                  UpdateDirectoryList(True);
                  UpdateWatch(To_String(CurrentDirectory));
                  Tcl.Ada.Tcl_Eval(Get_Context, "ShowSelected");
                  Tcl.Ada.Tcl_Eval(Get_Context, "update");
               end if;
            end if;
            ToggleToolButtons(NewAction, True);
            if Settings.ShowFinishedInfo then
               if NewAction = DELETE and not Settings.DeleteFiles then
                  ShowMessage
                    ("All selected files and directories have been moved to Trash.",
                     "message");
               elsif NewAction = CLEARTRASH then
                  ShowMessage("Trash have been cleared.", "message");
               else
                  ShowMessage
                    ("All selected files and directories have been deleted.",
                     "message");
               end if;
            else
               return Close_Command(ClientData, Interp, Argc, Argv);
            end if;
         when COPY =>
            if Response = "noall" then
               ToggleToolButtons(NewAction, True);
               LoadDirectory(To_String(CurrentDirectory));
               UpdateDirectoryList(True);
               return Close_Command(ClientData, Interp, Argc, Argv);
            elsif Response = "no" then
               SkipCopying;
               return Close_Command(ClientData, Interp, Argc, Argv);
            end if;
            CopySelected(OverwriteItem);
            return Close_Command(ClientData, Interp, Argc, Argv);
         when MOVE =>
            if Response = "noall" then
               ToggleToolButtons(NewAction, True);
               LoadDirectory(To_String(CurrentDirectory));
               UpdateDirectoryList(True);
               return Close_Command(ClientData, Interp, Argc, Argv);
            elsif Response = "no" then
               SkipMoving;
               return Close_Command(ClientData, Interp, Argc, Argv);
            end if;
            MoveSelected(OverwriteItem);
            return Close_Command(ClientData, Interp, Argc, Argv);
         when others =>
            null;
      end case;
      return TCL_OK;
   end Response_Command;

   procedure CreateMessagesUI is
      ButtonsBox: Ttk_Frame;
      Button: Ttk_Button;
   begin
      AddCommand("CloseMessage", Close_Command'Access);
      AddCommand("MessageResponse", Response_Command'Access);
      Style_Configure("message.TFrame", "-background #00ff00");
      Style_Configure
        ("message.TLabel", "-background #00ff00 -foreground #000000");
      Style_Configure("error.TFrame", "-background #ff0000");
      Style_Configure
        ("error.TLabel", "-background #ff0000 -foreground #000000");
      Style_Configure("question.TFrame", "-background #0000ff");
      Style_Configure
        ("question.TLabel", "-background #0000ff -foreground #ffffff");
      MessageFrame := Create(".mainframe.message");
      MessageLabel := Create(".mainframe.message.label", "-wraplength 800");
      ButtonsBox := Create(".mainframe.message.buttonsbox");
      Button :=
        Create
          (".mainframe.message.buttonsbox.buttonno",
           "-text No -command {MessageResponse no}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (".mainframe.message.buttonsbox.buttonyes",
           "-text Yes -command {MessageResponse yes}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
      Button :=
        Create
          (".mainframe.message.buttonsbox.buttonnoall",
           "-text ""No for all"" -command {MessageResponse noall}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 2 -row 0");
      Button :=
        Create
          (".mainframe.message.buttonsbox.buttonyesall",
           "-text ""Yes for all"" -command {MessageResponse yesall}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 3 -row 0");
      Button :=
        Create
          (".mainframe.message.buttonsbox.buttonclose",
           "-text x -style Toolbutton -command CloseMessage");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 4 -row 0");
      Tcl.Tk.Ada.Pack.Pack(ButtonsBox, "-side right");
      Tcl.Tk.Ada.Pack.Pack(MessageLabel, "-expand true -fill x");
   end CreateMessagesUI;

   procedure ShowMessage(Message: String; MessageType: String := "error") is
      ButtonsNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String(".mainframe.message.buttonsbox.buttonno"),
         To_Unbounded_String(".mainframe.message.buttonsbox.buttonyes"),
         To_Unbounded_String(".mainframe.message.buttonsbox.buttonnoall"),
         To_Unbounded_String(".mainframe.message.buttonsbox.buttonyesall"),
         To_Unbounded_String(".mainframe.message.buttonsbox.buttonclose"));
      Button: Ttk_Button;
   begin
      if MessageFrame.Interp = null then
         return;
      end if;
      Button.Interp := MessageLabel.Interp;
      for ButtonName of ButtonsNames loop
         Button.Name := New_String(To_String(ButtonName));
         Grid_Remove(Button);
      end loop;
      configure
        (MessageLabel,
         "-text {" & Message & "} -style " & MessageType & ".TLabel");
      configure(MessageFrame, "-style " & MessageType & ".TFrame");
      if MessageType /= "question" then
         Button.Name := New_String(To_String(ButtonsNames(5)));
         Tcl.Tk.Ada.Grid.Grid(Button);
      else
         Button.Name := New_String(To_String(ButtonsNames(1)));
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(To_String(ButtonsNames(2)));
         Tcl.Tk.Ada.Grid.Grid(Button);
         if NewAction not in DELETE | CLEARTRASH | DELETETRASH then
            Button.Name := New_String(To_String(ButtonsNames(3)));
            Tcl.Tk.Ada.Grid.Grid(Button);
            Button.Name := New_String(To_String(ButtonsNames(4)));
            Tcl.Tk.Ada.Grid.Grid(Button);
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (MessageFrame, "-column 0 -row 2 -sticky we -columnspan 2");
      TimerId :=
        To_Unbounded_String
          (After(Settings.AutoCloseMessagesTime * 1_000, "CloseMessage"));
   end ShowMessage;

end Messages;
