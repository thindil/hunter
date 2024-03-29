-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;

package body UserCommands.UI is

   procedure UpdateUserCommandsList is
      Row: Positive := 1;
      Label: Ttk_Label;
      CommandsFrame: constant Ttk_Frame :=
        Get_Widget(".preferencesframe.canvas.notebook.actions.commandsframe");
      Item: Ttk_Frame;
      Tokens: Slice_Set;
      Image: Tk_Photo;
      Button: Ttk_Button;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CommandsFrame), " ");
      Clear_Commands_List_Loop :
      for I in 0 .. (Natural'Value(Slice(Tokens, 2)) - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (CommandsFrame, "-row" & Positive'Image(I)),
            " ");
         Delete_Items_Loop :
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop Delete_Items_Loop;
      end loop Clear_Commands_List_Loop;
      if User_Commands_List.Is_Empty then
         return;
      end if;
      Label := Create(CommandsFrame & ".name", "-text {Menu label}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Column_Configure(CommandsFrame, Label, "-weight 1");
      Label := Create(CommandsFrame & ".command", "-text {Command}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Column_Configure(CommandsFrame, Label, "-weight 1");
      Label := Create(CommandsFrame & ".output", "-text {Output}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row 0 -column 2");
      Tcl.Tk.Ada.Grid.Column_Configure(CommandsFrame, Label, "-weight 1");
      Image.Interp := Get_Context;
      Load_User_Commnads_Loop :
      for I in User_Commands_List.Iterate loop
         Label :=
           Create
             (CommandsFrame & ".name" & Trim(Positive'Image(Row), Left),
              "-text {" & Commands_Container.Key(I) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Positive'Image(Row));
         Label :=
           Create
             (CommandsFrame & ".command" & Trim(Positive'Image(Row), Left),
              "-text {" & To_String(User_Commands_List(I).Command) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 1");
         Label :=
           (if User_Commands_List(I).Need_Output then
              Create
                (CommandsFrame & ".output" & Trim(Positive'Image(Row), Left),
                 "-text {" & Mc(Get_Context, "{Yes}") & "}")
            else Create
                (CommandsFrame & ".output" & Trim(Positive'Image(Row), Left),
                 "-text {" & Mc(Get_Context, "{No}") & "}"));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 2");
         Image.Name := New_String("refreshicon");
         Button :=
           Create
             (CommandsFrame & ".editbutton" & Trim(Positive'Image(Row), Left),
              "-style Toolbutton -image " & Image &
              " -command {EditCommand {" & Commands_Container.Key(I) & "}}");
         Add
           (Button,
            Mc
              (Get_Context,
               "{Edit the selected command. If you change the menu label,}") &
            LF & Mc(Get_Context, "{a new command will be added.}"));
         Tcl.Tk.Ada.Grid.Grid
           (Button, "-row" & Positive'Image(Row) & " -column 3");
         Image.Name := New_String("edit-deleteicon");
         Button :=
           Create
             (CommandsFrame & ".deletebutton" &
              Trim(Positive'Image(Row), Left),
              "-style Toolbutton -image " & Image &
              " -command {DeleteCommand {" & Commands_Container.Key(I) & "}}");
         Add(Button, Mc(Get_Context, "{Delete the selected command.}"));
         Tcl.Tk.Ada.Grid.Grid
           (Button, "-row" & Positive'Image(Row) & " -column 4");
         Row := Row + 1;
      end loop Load_User_Commnads_Loop;
   end UpdateUserCommandsList;

end UserCommands.UI;
