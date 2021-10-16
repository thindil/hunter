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

with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Common; use Common;
with CopyItems.UI;
with DeleteItems;
with LoadData.UI;
with MainWindow;
with MoveItems.UI;
with Preferences; use Preferences;
with RefreshData;
with Trash.UI;
with Utils.UI; use Utils.UI;

package body Messages.UI is

   -- ****iv* MessagesUI/MessagesUI.Message_Frame
   -- FUNCTION
   -- Main frame for the message widget
   -- SOURCE
   Message_Frame: Ttk_Frame;
   -- ****

   -- ****if* MessagesUI/MessagesUI.Get_Message_Frame
   -- FUNCTION
   -- Get the main frame for the messages widget
   -- SOURCE
   function Get_Message_Frame return Ttk_Frame is
      -- ****
   begin
      return Message_Frame;
   end Get_Message_Frame;

   -- ****iv* MessagesUI/MessagesUI.Message_Label
   -- FUNCTION
   -- Label which show message text
   -- SOURCE
   Message_Label: Ttk_Label;
   -- ****

   -- ****if* MessagesUI/MessagesUI.Get_Message_Label
   -- FUNCTION
   -- Get the label widget with the message text
   -- SOURCE
   function Get_Message_Label return Ttk_Label is
      -- ****
   begin
      return Message_Label;
   end Get_Message_Label;

   -- ****iv* MessagesUI/MessagesUI.Timer_Id
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   Timer_Id: Unbounded_String := Null_Unbounded_String;
   -- ****

   -- ****if* MessagesIU/MessagesUI.Set_Timer
   -- FUNCTION
   -- Set the name for the timer for auto close command
   -- PARAMETERS
   -- New_Timer - The new Id of the timer
   -- SOURCE
   procedure Set_Timer(New_Timer: Unbounded_String) is
      -- ****
   begin
      Timer_Id := New_Timer;
   end Set_Timer;

   function Close_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      if Timer_Id /= Null_Unbounded_String then
         Cancel(id_or_script => To_String(Source => Timer_Id));
         Set_Timer(New_Timer => Null_Unbounded_String);
      end if;
      Grid_Remove(Slave => Get_Message_Frame);
      return TCL_OK;
   end Close_Command;

   -- ****o* Messages/Messages.Response_Command
   -- FUNCTION
   -- Hide message frame and do action, depends on user response
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MessageResponse answer
   -- Answer is the answer which the user selected by clicking in button
   -- SOURCE
   function Response_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Response_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Ada.Environment_Variables;
      use CopyItems.UI;
      use LoadData.UI;
      use MainWindow;
      use MoveItems.UI;
      use RefreshData;
      use Trash.UI;

      Overwrite_Item: Boolean := True;
      Response: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      Yes_For_All := (if Response = "yesall" then True else False);
      case New_Action is
         when DELETE | CLEARTRASH | DELETETRASH =>
            if New_Action /= CLEARTRASH then
               Update_Progress_Bar(Amount => Positive(Selected_Items.Length));
            end if;
            if Response = "yes" then
               Delete_Selected_Block :
               declare
                  use GNAT.OS_Lib;
                  use DeleteItems;
               begin
                  if Delete_Selected(Interpreter => Get_Context) then
                     Current_Directory :=
                       To_Unbounded_String
                         (Source =>
                            Normalize_Pathname
                              (Name =>
                                 To_String(Source => Current_Directory) &
                                 "/.."));
                  end if;
               exception
                  when others =>
                     Load_Directory
                       (Directory_Name =>
                          To_String(Source => Current_Directory));
                     Update_Directory_List(Clear => True);
                     return TCL_OK;
               end Delete_Selected_Block;
               case New_Action is
                  when CLEARTRASH =>
                     Tcl.Ada.Tcl_Eval
                       (interp => Get_Context,
                        strng =>
                          "GoToBookmark {" & Value(Name => "HOME") & "}");
                  when DELETETRASH =>
                     Toggle_Tool_Buttons
                       (Action => New_Action, Finished => True);
                     if Close_Command
                         (Client_Data => Client_Data, Interp => Interp,
                          Argc => Argc, Argv => Argv) =
                       TCL_OK then
                        return
                          Show_Trash_Command
                            (ClientData => Client_Data, Interp => Interp,
                             Argc => Argc, Argv => Argv);
                     end if;
                  when others =>
                     Load_Directory
                       (Directory_Name =>
                          To_String(Source => Current_Directory));
                     Update_Directory_List(Clear => True);
                     UpdateWatch
                       (Path => To_String(Source => Current_Directory));
                     Tcl.Ada.Tcl_Eval
                       (interp => Get_Context, strng => "ShowSelected");
                     Tcl.Ada.Tcl_Eval
                       (interp => Get_Context, strng => "update");
               end case;
            end if;
            Toggle_Tool_Buttons(Action => New_Action, Finished => True);
            if Settings.Show_Finished_Info then
               if New_Action = DELETE and not Settings.Delete_Files then
                  Show_Message
                    (Message =>
                       Mc
                         (Interp => Interp,
                          Src_String =>
                            "{All selected files and directories have been moved to Trash.}"),
                     Message_Type => "message");
               elsif New_Action = CLEARTRASH then
                  Show_Message
                    (Message =>
                       Mc
                         (Interp => Interp,
                          Src_String => "{Trash have been cleared.}"),
                     Message_Type => "message");
               else
                  Show_Message
                    (Message =>
                       Mc
                         (Interp => Interp,
                          Src_String =>
                            "{All selected files and directories have been deleted.}"),
                     Message_Type => "message");
               end if;
               return TCL_OK;
            end if;
            return
              Close_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         when COPY =>
            if Response = "noall" then
               Toggle_Tool_Buttons(Action => New_Action, Finished => True);
               Load_Directory
                 (Directory_Name => To_String(Source => Current_Directory));
               Update_Directory_List(Clear => True);
               return
                 Close_Command
                   (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                    Argv => Argv);
            elsif Response = "no" then
               Skip_Copying;
               return
                 Close_Command
                   (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                    Argv => Argv);
            end if;
            Copy_Selected(Overwrite => Overwrite_Item);
            return
              Close_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         when MOVE =>
            if Response = "noall" then
               Toggle_Tool_Buttons(Action => New_Action, Finished => True);
               Load_Directory
                 (Directory_Name => To_String(Source => Current_Directory));
               Update_Directory_List(Clear => True);
               return
                 Close_Command
                   (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                    Argv => Argv);
            elsif Response = "no" then
               SkipMoving;
               return
                 Close_Command
                   (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                    Argv => Argv);
            end if;
            MoveSelected(Overwrite => Overwrite_Item);
            return
              Close_Command
                (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                 Argv => Argv);
         when others =>
            null;
      end case;
      return TCL_OK;
   end Response_Command;

   procedure Create_Messages_Ui is
      use Tcl.Tk.Ada.TtkStyle;

      Buttons_Box: Ttk_Frame;
      Button: Ttk_Button;
      procedure Add_Button
        (Name, Text, Response: String; Column: Natural := 0) is
         Response_Button: constant Ttk_Button :=
           Create
             (pathName => Buttons_Box & ".button" & Name,
              options =>
                "-text {" & Text & "} -command {MessageResponse " & Response &
                "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Response_Button,
            Options => "-row 0 -column" & Natural'Image(Column));
      end Add_Button;
   begin
      Add_Command(Name => "CloseMessage", Ada_Command => Close_Command'Access);
      Add_Command
        (Name => "MessageResponse", Ada_Command => Response_Command'Access);
      Style_Configure
        (Name => "message.TFrame", Options => "-background #27ae60");
      Style_Configure
        (Name => "message.TLabel",
         Options => "-background #27ae60 -foreground #ffffff");
      Style_Configure
        (Name => "error.TFrame", Options => "-background #da4453");
      Style_Configure
        (Name => "error.TLabel",
         Options => "-background #da4453 -foreground #ffffff");
      Style_Configure
        (Name => "question.TFrame", Options => "-background #3daee9");
      Style_Configure
        (Name => "question.TLabel",
         Options => "-background #3daee9 -foreground #ffffff");
      Message_Frame := Create(pathName => ".mainframe.message");
      Message_Label :=
        Create
          (pathName => Get_Message_Frame & ".label",
           options => "-wraplength 800");
      Buttons_Box := Create(pathName => Get_Message_Frame & ".buttonsbox");
      Add_Button
        (Name => "no", Text => Mc(Interp => Get_Context, Src_String => "{No}"),
         Response => "no");
      Add_Button
        (Name => "yes",
         Text => Mc(Interp => Get_Context, Src_String => "{Yes}"),
         Response => "yes", Column => 1);
      Add_Button
        (Name => "noall",
         Text => Mc(Interp => Get_Context, Src_String => "{No for all}"),
         Response => "noall", Column => 2);
      Add_Button
        (Name => "yesall",
         Text => Mc(Interp => Get_Context, Src_String => "{Yes for all}"),
         Response => "yesall", Column => 3);
      Button :=
        Create
          (pathName => Buttons_Box & ".buttonclose",
           options => "-text x -style Toolbutton -command CloseMessage");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-column 4 -row 0");
      Tcl.Tk.Ada.Pack.Pack(Slave => Buttons_Box, Options => "-side right");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Message_Label, Options => "-expand true -fill x");
   end Create_Messages_Ui;

   procedure Show_Message(Message: String; Message_Type: String := "error") is
      use Interfaces.C.Strings;

      Buttons_Names: constant array(1 .. 5) of Unbounded_String :=
        (1 =>
           To_Unbounded_String
             (Source => Get_Message_Frame & ".buttonsbox.buttonno"),
         2 =>
           To_Unbounded_String
             (Source => Get_Message_Frame & ".buttonsbox.buttonyes"),
         3 =>
           To_Unbounded_String
             (Source => Get_Message_Frame & ".buttonsbox.buttonnoall"),
         4 =>
           To_Unbounded_String
             (Source => Get_Message_Frame & ".buttonsbox.buttonyesall"),
         5 =>
           To_Unbounded_String
             (Source => Get_Message_Frame & ".buttonsbox.buttonclose"));
      Button: Ttk_Button := Get_Widget(pathName => ".");
   begin
      if Get_Message_Frame.Name = Null_Ptr then
         return;
      end if;
      Button.Interp := Get_Message_Label.Interp;
      Remove_Buttons_Loop :
      for ButtonName of Buttons_Names loop
         Button.Name := New_String(Str => To_String(Source => ButtonName));
         Grid_Remove(Slave => Button);
      end loop Remove_Buttons_Loop;
      configure
        (Widgt => Get_Message_Label,
         options =>
           "-text {" & Message & "} -style " & Message_Type & ".TLabel");
      configure
        (Widgt => Get_Message_Frame,
         options => "-style " & Message_Type & ".TFrame");
      if Message_Type = "question" then
         Button.Name :=
           New_String(Str => To_String(Source => Buttons_Names(1)));
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
         Button.Name :=
           New_String(Str => To_String(Source => Buttons_Names(2)));
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
         if New_Action not in DELETE | CLEARTRASH | DELETETRASH then
            Button.Name :=
              New_String(Str => To_String(Source => Buttons_Names(3)));
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
            Button.Name :=
              New_String(Str => To_String(Source => Buttons_Names(4)));
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
         end if;
      else
         Button.Name :=
           New_String(Str => To_String(Source => Buttons_Names(5)));
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Get_Message_Frame,
         Options => "-column 0 -row 2 -sticky we -columnspan 2");
      if Message_Type /= "question" then
         Set_Timer
           (New_Timer =>
              To_Unbounded_String
                (Source =>
                   After
                     (Ms => Settings.Auto_Close_Messages_Time * 1_000,
                      Script => "CloseMessage")));
      end if;
   end Show_Message;

end Messages.UI;
