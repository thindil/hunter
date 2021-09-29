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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with CopyItems.UI; use CopyItems.UI;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with Trash.UI; use Trash.UI;
with Utils.UI; use Utils.UI;

package body Messages is

   -- ****iv* Messages/Messages.Message_Frame
   -- FUNCTION
   -- Main frame for the message widget
   -- SOURCE
   Message_Frame: Ttk_Frame;
   -- ****

   -- ****iv* Messages/Messages.Message_Label
   -- FUNCTION
   -- Label which show message text
   -- SOURCE
   Message_Label: Ttk_Label;
   -- ****

   -- ****iv* Messages/Messages.Timer_Id
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   Timer_Id: Unbounded_String := Null_Unbounded_String;
   -- ****

   function Close_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      if Timer_Id /= Null_Unbounded_String then
         Cancel(id_or_script => To_String(Source => Timer_Id));
         Timer_Id := Null_Unbounded_String;
      end if;
      Grid_Remove(Slave => Message_Frame);
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
                     Tcl.Ada.Tcl_Eval(Get_Context, "update");
               end case;
            end if;
            Toggle_Tool_Buttons(New_Action, True);
            if Settings.Show_Finished_Info then
               if New_Action = DELETE and not Settings.Delete_Files then
                  Show_Message
                    (Mc
                       (Interp,
                        "{All selected files and directories have been moved to Trash.}"),
                     "message");
               elsif New_Action = CLEARTRASH then
                  Show_Message
                    (Mc(Interp, "{Trash have been cleared.}"), "message");
               else
                  Show_Message
                    (Mc
                       (Interp,
                        "{All selected files and directories have been deleted.}"),
                     "message");
               end if;
            else
               return Close_Command(Client_Data, Interp, Argc, Argv);
            end if;
         when COPY =>
            if Response = "noall" then
               Toggle_Tool_Buttons(New_Action, True);
               Load_Directory(To_String(Current_Directory));
               Update_Directory_List(True);
               return Close_Command(Client_Data, Interp, Argc, Argv);
            elsif Response = "no" then
               Skip_Copying;
               return Close_Command(Client_Data, Interp, Argc, Argv);
            end if;
            Copy_Selected(Overwrite_Item);
            return Close_Command(Client_Data, Interp, Argc, Argv);
         when MOVE =>
            if Response = "noall" then
               Toggle_Tool_Buttons(New_Action, True);
               Load_Directory(To_String(Current_Directory));
               Update_Directory_List(True);
               return Close_Command(Client_Data, Interp, Argc, Argv);
            elsif Response = "no" then
               SkipMoving;
               return Close_Command(Client_Data, Interp, Argc, Argv);
            end if;
            MoveSelected(Overwrite_Item);
            return Close_Command(Client_Data, Interp, Argc, Argv);
         when others =>
            null;
      end case;
      return TCL_OK;
   end Response_Command;

   procedure Create_Messages_Ui is
      ButtonsBox: Ttk_Frame;
      Button: Ttk_Button;
      procedure AddButton
        (Name, Text, Response: String; Column: Natural := 0) is
         ResponseButton: constant Ttk_Button :=
           Create
             (ButtonsBox & ".button" & Name,
              "-text {" & Text & "} -command {MessageResponse " & Response &
              "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (ResponseButton, "-row 0 -column" & Natural'Image(Column));
      end AddButton;
   begin
      Add_Command("CloseMessage", Close_Command'Access);
      Add_Command("MessageResponse", Response_Command'Access);
      Style_Configure("message.TFrame", "-background #27ae60");
      Style_Configure
        ("message.TLabel", "-background #27ae60 -foreground #ffffff");
      Style_Configure("error.TFrame", "-background #da4453");
      Style_Configure
        ("error.TLabel", "-background #da4453 -foreground #ffffff");
      Style_Configure("question.TFrame", "-background #3daee9");
      Style_Configure
        ("question.TLabel", "-background #3daee9 -foreground #ffffff");
      Message_Frame := Create(".mainframe.message");
      Message_Label := Create(Message_Frame & ".label", "-wraplength 800");
      ButtonsBox := Create(Message_Frame & ".buttonsbox");
      AddButton("no", Mc(Get_Context, "{No}"), "no");
      AddButton("yes", Mc(Get_Context, "{Yes}"), "yes", 1);
      AddButton("noall", Mc(Get_Context, "{No for all}"), "noall", 2);
      AddButton("yesall", Mc(Get_Context, "{Yes for all}"), "yesall", 3);
      Button :=
        Create
          (ButtonsBox & ".buttonclose",
           "-text x -style Toolbutton -command CloseMessage");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 4 -row 0");
      Tcl.Tk.Ada.Pack.Pack(ButtonsBox, "-side right");
      Tcl.Tk.Ada.Pack.Pack(Message_Label, "-expand true -fill x");
   end Create_Messages_Ui;

   procedure Show_Message(Message: String; Message_Type: String := "error") is
      ButtonsNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String(Message_Frame & ".buttonsbox.buttonno"),
         To_Unbounded_String(Message_Frame & ".buttonsbox.buttonyes"),
         To_Unbounded_String(Message_Frame & ".buttonsbox.buttonnoall"),
         To_Unbounded_String(Message_Frame & ".buttonsbox.buttonyesall"),
         To_Unbounded_String(Message_Frame & ".buttonsbox.buttonclose"));
      Button: Ttk_Button;
   begin
      if Message_Frame.Name = Null_Ptr then
         return;
      end if;
      Button.Interp := Message_Label.Interp;
      Remove_Buttons_Loop :
      for ButtonName of ButtonsNames loop
         Button.Name := New_String(To_String(ButtonName));
         Grid_Remove(Button);
      end loop Remove_Buttons_Loop;
      configure
        (Message_Label,
         "-text {" & Message & "} -style " & Message_Type & ".TLabel");
      configure(Message_Frame, "-style " & Message_Type & ".TFrame");
      if Message_Type /= "question" then
         Button.Name := New_String(To_String(ButtonsNames(5)));
         Tcl.Tk.Ada.Grid.Grid(Button);
      else
         Button.Name := New_String(To_String(ButtonsNames(1)));
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(To_String(ButtonsNames(2)));
         Tcl.Tk.Ada.Grid.Grid(Button);
         if New_Action not in DELETE | CLEARTRASH | DELETETRASH then
            Button.Name := New_String(To_String(ButtonsNames(3)));
            Tcl.Tk.Ada.Grid.Grid(Button);
            Button.Name := New_String(To_String(ButtonsNames(4)));
            Tcl.Tk.Ada.Grid.Grid(Button);
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Message_Frame, "-column 0 -row 2 -sticky we -columnspan 2");
      if Message_Type /= "question" then
         Timer_Id :=
           To_Unbounded_String
             (After
                (Settings.Auto_Close_Messages_Time * 1_000, "CloseMessage"));
      end if;
   end Show_Message;

end Messages;
