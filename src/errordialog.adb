-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gdk.Cursor; use Gdk.Cursor;
with Gdk.Window; use Gdk.Window;
with Gtkada.Intl; use Gtkada.Intl;

package body ErrorDialog is

   -- ****iv* ErrorDialog/Builder
   -- FUNCTION
   -- Gtk Builder with UI data from .glade file
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   procedure SaveException
     (An_Exception: Exception_Occurrence; PrintToTerminal: Boolean) is
      ErrorFile: File_Type;
      ErrorText: Unbounded_String;
      ErrorFilePath: constant String :=
        Value("HOME") & "/.cache/hunter/error.log";
   begin
      if Ada.Directories.Exists(ErrorFilePath) then
         Open(ErrorFile, Append_File, ErrorFilePath);
      else
         Create(ErrorFile, Append_File, ErrorFilePath);
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, LF);
      Append(ErrorText, "1.2");
      Append(ErrorText, LF);
      Append(ErrorText, "Exception: " & Exception_Name(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "Message: " & Exception_Message(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Append(ErrorText, LF);
      Append(ErrorText, Symbolic_Traceback(An_Exception));
      Append(ErrorText, LF);
      Append(ErrorText, "-------------------------------------------------");
      Put_Line(ErrorFile, To_String(ErrorText));
      Close(ErrorFile);
      if PrintToTerminal then
         Put_Line(To_String(ErrorText));
      else
         Set_Text
           (Gtk_Text_Buffer(Get_Object(Builder, "errorbuffer")),
            To_String(ErrorText));
         Hide(Gtk_Widget(Get_Object(Builder, "toolbar")));
         Hide(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "filestack")), "error");
         Set_Cursor
           (Get_Window(Gtk_Widget(Get_Object(Builder, "mainwindow"))),
            Gdk_Cursor_New(Arrow));
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "mainwindow")), True);
      end if;
   end SaveException;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
   begin
      SaveException(An_Exception, False);
   end On_Exception;

   procedure CreateErrorUI(NewBuilder: Gtkada_Builder) is
      Box: constant Gtk_Vbox := Gtk_Vbox_New;
      Label: Gtk_Label;
      Button: constant Gtk_Link_Button :=
        Gtk_Link_Button_New("https://github.com/thindil/hunter/issues");
   begin
      Builder := NewBuilder;
      Label :=
        Gtk_Label_New
          (Gettext
             ("Oops, something bad happens and progam crashed. Please, remember what you done before crash and report this problem at"));
      Set_Max_Width_Chars(Label, 80);
      Set_Line_Wrap(Label, True);
      Pack_Start(Box, Label, False);
      Set_Relief(Button, Relief_None);
      Pack_Start(Box, Button, False);
      Label :=
        Gtk_Label_New
          (Gettext(" and attach (if possible) file 'error.log' from '") &
           Value("HOME") & Gettext("/.cache/hunter' directory."));
      Set_Line_Wrap(Label, True);
      Pack_Start(Box, Label, False);
      Add_Named(Gtk_Stack(Get_Object(Builder, "filestack")), Box, "errorbox");
   end CreateErrorUI;

end ErrorDialog;
