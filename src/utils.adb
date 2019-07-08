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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Messages; use Messages;

package body Utils is

   function GetMimeType(FileName: String) return String is
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
      Arguments: constant Argument_List :=
        (new String'("-b"), new String'("--mime-type"), new String'(FileName));
      ExecutableName: constant String := FindExecutable("file");
   begin
      if ExecutableName = "" then
         return "";
      end if;
      Non_Blocking_Spawn(ProcessDesc, ExecutableName, Arguments);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      case Result is
         when 1 =>
            declare
               MimeType: constant String := Expect_Out(ProcessDesc);
            begin
               Close(ProcessDesc);
               return MimeType;
            end;
         when others =>
            null;
      end case;
      Close(ProcessDesc);
      return "";
   end GetMimeType;

   function CanBeOpened(MimeType: String) return Boolean is
      ProcessDesc: Process_Descriptor;
      Result: Expect_Match;
      ExecutableName: constant String := FindExecutable("xdg-mime");
   begin
      if ExecutableName = "" then
         return False;
      end if;
      Non_Blocking_Spawn
        (ProcessDesc, ExecutableName,
         Argument_String_To_List("query default " & MimeType).all);
      Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
      Close(ProcessDesc);
      return True;
   exception
      when Process_Died =>
         return False;
   end CanBeOpened;

   function CountFileSize(Size: File_Size) return String is
      Multiplier: Natural;
      NewSize: File_Size;
      SizeShortcuts: constant array(Natural range <>) of String(1 .. 3) :=
        ("B  ", "KiB", "MiB", "TiB", "PiB", "EiB", "ZiB", "YiB");
   begin
      NewSize := Size;
      Multiplier := 0;
      while NewSize > 1024 loop
         NewSize := NewSize / 1024;
         Multiplier := Multiplier + 1;
      end loop;
      return File_Size'Image(NewSize) & " " & SizeShortcuts(Multiplier);
   end CountFileSize;

   function FindExecutable(Name: String) return String is
      ExecutablePath: GNAT.OS_Lib.String_Access;
   begin
      if Exists(Containing_Directory(Command_Name) & "/" & Name) then
         return Containing_Directory(Command_Name) & "/" & Name;
      end if;
      ExecutablePath := Locate_Exec_On_Path(Name);
      if ExecutablePath = null then
         ShowMessage(Gettext("Could not found executable: ") & Name);
         return "";
      end if;
      return ExecutablePath.all;
   end FindExecutable;

   procedure ToggleToolButtons
     (Action: ItemActions; Finished: Boolean := False) is
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("btnsearch"), To_Unbounded_String("btnnew"),
         To_Unbounded_String("btnrename"), To_Unbounded_String("btncopy"),
         To_Unbounded_String("btncut"), To_Unbounded_String("btndelete"),
         To_Unbounded_String("btnpreferences"),
         To_Unbounded_String("btnabout"));
      CurrentButton: Unbounded_String;
   begin
      case Action is
         when CREATEFILE | CREATEDIRECTORY =>
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnbookmarks")), Finished);
            CurrentButton := To_Unbounded_String("btnnew");
         when CREATELINK =>
            CurrentButton := To_Unbounded_String("btnnew");
         when RENAME =>
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnbookmarks")), Finished);
            CurrentButton := To_Unbounded_String("btnrename");
         when COPY =>
            CurrentButton := To_Unbounded_String("btncopy");
         when MOVE =>
            CurrentButton := To_Unbounded_String("btncut");
         when DELETE =>
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnbookmarks")), Finished);
            CurrentButton := To_Unbounded_String("");
         when others =>
            return;
      end case;
      for ButtonName of ButtonsNames loop
         if ButtonName /= CurrentButton then
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, To_String(ButtonName))),
               Finished);
         end if;
      end loop;
      if Finished then
         Set_Title(Gtk_Header_Bar(Get_Object(Builder, "header")), "");
      else
         case Action is
            when CREATEFILE =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Creating empty file"));
            when CREATEDIRECTORY =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Creating new directory"));
            when CREATELINK =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Creating new link"));
            when RENAME =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Renaming file or directory"));
            when COPY =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Copying files and directories"));
            when MOVE =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Moving files and directories"));
            when DELETE =>
               Set_Title
                 (Gtk_Header_Bar(Get_Object(Builder, "header")),
                  Gettext("Deleting files and directories"));
            when others =>
               null;
         end case;
      end if;
   end ToggleToolButtons;

end Utils;
