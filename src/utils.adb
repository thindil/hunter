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
with Gtk.Label; use Gtk.Label;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with Gtkada.Builder; use Gtkada.Builder;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with LoadData; use LoadData;
with Messages; use Messages;
with Preferences; use Preferences;
with ShowItems; use ShowItems;

package body Utils is

   -- ****iv* Utils/Positive
   -- FUNCTION
   -- Max amount of items to count progress of action
   -- SOURCE
   ProgressAmount: Positive;
   -- ****

   -- ****iv* Utils/ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Positive;
   -- ****

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
      CurrentButton: Unbounded_String := To_Unbounded_String("");
   begin
      case Action is
         when CREATEFILE | CREATEDIRECTORY | RENAME | DELETE | DELETETRASH =>
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btnbookmarks")), Finished);
         when CREATELINK =>
            null;
         when COPY =>
            CurrentButton := To_Unbounded_String("btncopy");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "btntoolcancel")),
               Gettext("Stop copying files and directories [Escape]"));
         when MOVE =>
            CurrentButton := To_Unbounded_String("btncut");
            Set_Tooltip_Text
              (Gtk_Widget(Get_Object(Builder, "btntoolcancel")),
               Gettext("Stop moving files and directories [Escape]"));
         when SHOWTRASH =>
            CurrentButton := To_Unbounded_String("btndelete");
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, "btntoolrestore")),
               not Finished);
         when others =>
            return;
      end case;
      if (Action = CREATELINK or Action = COPY or Action = MOVE)
        and then (not Settings.ShowPreview) and then (not Finished) then
         Set_Position
           (Gtk_Paned(Get_Object(Builder, "filespaned")),
            Gint
              (Float
                 (Get_Allocated_Width
                    (Gtk_Widget(Get_Object(Builder, "mainwindow")))) *
               0.3));
         Show_All(Gtk_Widget(Get_Object(Builder, "boxsecond")));
      end if;
      if (Action = COPY or Action = MOVE) then
         if not Finished then
            LoadDirectory(To_String(CurrentDirectory), "fileslist2");
            Set_Label
              (Gtk_Label(Get_Object(Builder, "lblframe")),
               Gettext("Destination directory"));
            Set_Visible_Child_Name
              (Gtk_Stack(Get_Object(Builder, "infostack")), "destination");
         else
            Hide(Gtk_Widget(Get_Object(Builder, "boxpath2")));
         end if;
         Set_Visible
           (Gtk_Widget(Get_Object(Builder, "btntoolcancel")), not Finished);
      end if;
      if Action = DELETETRASH and then Finished then
         Show_All(Gtk_Widget(Get_Object(Builder, "btntoolrestore")));
         Show_All(Gtk_Widget(Get_Object(Builder, "btndelete")));
      else
         if Action /= SHOWTRASH then
            Hide(Gtk_Widget(Get_Object(Builder, "btntoolrestore")));
         end if;
         for ButtonName of ButtonsNames loop
            if ButtonName /= CurrentButton then
               Set_Visible
                 (Gtk_Widget(Get_Object(Builder, To_String(ButtonName))),
                  Finished);
            end if;
         end loop;
      end if;
      if Finished then
         Set_Title(Gtk_Header_Bar(Get_Object(Builder, "header")), "");
         Show_All(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
         if not Settings.ShowPreview then
            Set_Position
              (Gtk_Paned(Get_Object(Builder, "filespaned")),
               Get_Allocated_Width
                 (Gtk_Widget(Get_Object(Builder, "mainwindow"))));
            Hide(Gtk_Widget(Get_Object(Builder, "boxsecond")));
         else
            SetBookmarkButton;
         end if;
      else
         if Action /= SHOWTRASH then
            Hide(Gtk_Widget(Get_Object(Builder, "itemtoolbar")));
         end if;
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
            when DELETE | DELETETRASH =>
               if Settings.DeleteFiles or Action = DELETETRASH then
                  Set_Title
                    (Gtk_Header_Bar(Get_Object(Builder, "header")),
                     Gettext("Deleting files and directories"));
               else
                  Set_Title
                    (Gtk_Header_Bar(Get_Object(Builder, "header")),
                     Gettext("Moving files and directories to trash"));
               end if;
            when others =>
               null;
         end case;
      end if;
   end ToggleToolButtons;

   procedure ToggleActionButtons is
      ButtonsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("btnrename"), To_Unbounded_String("btncopy"),
         To_Unbounded_String("btncut"), To_Unbounded_String("btndelete"));
      Visible: Boolean;
   begin
      if N_Children
          (Get_Model(Gtk_Tree_View(Get_Object(Builder, "treefiles"))),
           Null_Iter) =
        0 then
         Visible := False;
      else
         Visible := True;
      end if;
      if NewAction /= SHOWTRASH then
         for ButtonName of ButtonsNames loop
            Set_Visible
              (Gtk_Widget(Get_Object(Builder, To_String(ButtonName))),
               Visible);
         end loop;
      else
         Set_Visible(Gtk_Widget(Get_Object(Builder, "btndelete")), Visible);
         Set_Visible
           (Gtk_Widget(Get_Object(Builder, "btntoolrestore")), Visible);
      end if;
   end ToggleActionButtons;

   procedure SetProgressBar(Amount: Positive) is
      ProgressBar: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Builder, "progressbar"));
   begin
      Show_All(ProgressBar);
      Set_Fraction(Gtk_Progress_Bar(ProgressBar), 0.0);
      ProgressAmount := Amount;
      ProgressIndex := 1;
   end SetProgressBar;

   procedure UpdateProgressBar is
      ProgressBar: constant Gtk_Progress_Bar :=
        Gtk_Progress_Bar(Get_Object(Builder, "progressbar"));
   begin
      Set_Fraction
        (ProgressBar, Gdouble(ProgressIndex) / Gdouble(ProgressAmount));
   end UpdateProgressBar;

end Utils;
