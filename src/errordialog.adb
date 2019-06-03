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
with Gtk.Container; use Gtk.Container;
with Gtk.Label; use Gtk.Label;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tool_Item; use Gtk.Tool_Item;
with Glib; use Glib;

package body ErrorDialog is

   -- ****iv* ErrorDialog/Builder
   -- FUNCTION
   -- Gtk Builder with UI data from .glade file
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   procedure HideButton(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      if Get_Item_Index
          (Gtk_Toolbar(Get_Object(Builder, "toolbar")),
           Gtk_Tool_Item(Widget)) >
        1 then
         Hide(Widget);
      end if;
   end HideButton;

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
      Append(ErrorText, "0.5");
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
         Foreach
           (Gtk_Container(Get_Object(Builder, "toolbar")), HideButton'Access);
         Hide(Gtk_Widget(Get_Object(Builder, "entry")));
         Hide(Gtk_Widget(Get_Object(Builder, "boxpath")));
         Hide(Gtk_Widget(Get_Object(Builder, "searchfile")));
         Hide(Gtk_Widget(Get_Object(Builder, "actioninfo")));
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "filestack")), "error");
      end if;
   end SaveException;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
   begin
      SaveException(An_Exception, False);
   end On_Exception;

   procedure CreateErrorDialog(NewBuilder: Gtkada_Builder) is
      Label: constant Gtk_Label :=
        Gtk_Label(Get_Object(NewBuilder, "lblerror"));
   begin
      Builder := NewBuilder;
      Set_Label
        (Label,
         Get_Label(Label) & " from '" & Value("HOME") &
         "/.cache/hunter' directory.");
   end CreateErrorDialog;

end ErrorDialog;
