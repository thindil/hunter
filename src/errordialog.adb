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
with Ada.Directories; use Ada.Directories;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Widget; use Gtk.Widget;

package body ErrorDialog is

   Builder: Gtkada_Builder;

   procedure SaveException(An_Exception: Exception_Occurrence;
      PrintToTerminal: Boolean) is
      ErrorFile: File_Type;
      ErrorText: Unbounded_String;
   begin
      if Exists("error.log") then
         Open(ErrorFile, Append_File, "error.log");
      else
         Create(ErrorFile, Append_File, "error.log");
      end if;
      Append(ErrorText, Ada.Calendar.Formatting.Image(Clock));
      Append(ErrorText, LF);
      Append(ErrorText, "0.1");
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
         Show_All(Gtk_Widget(Get_Object(Builder, "errordialog")));
      end if;
   end SaveException;

   procedure On_Exception(An_Exception: Exception_Occurrence) is
   begin
      SaveException(An_Exception, False);
   end On_Exception;

   procedure CreateErrorDialog(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
   end CreateErrorDialog;

end ErrorDialog;
