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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Bookmarks; use Bookmarks;
with LibMagic; use LibMagic;
with LoadData; use LoadData;
with Messages; use Messages;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Toolbars; use Toolbars;

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
      ButtonsIndexes: constant array(Positive range <>) of Positive :=
        (1, 2, 4, 5, 6, 7, 8, 12, 13);
      CurrentButtonIndex: Natural := 0;
   begin
      case Action is
         when CREATEFILE | CREATEDIRECTORY | RENAME | DELETE | DELETETRASH =>
            Set_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 0)), Finished);
         when CREATELINK =>
            null;
         when COPY =>
            CurrentButtonIndex := 6;
            Set_Tooltip_Text
              (Gtk_Widget(Get_Nth_Item(ActionToolBar, 9)),
               Gettext("Stop copying files and directories [Escape]"));
         when MOVE =>
            CurrentButtonIndex := 7;
            Set_Tooltip_Text
              (Gtk_Widget(Get_Nth_Item(ActionToolBar, 9)),
               Gettext("Stop moving files and directories [Escape]"));
         when SHOWTRASH =>
            CurrentButtonIndex := 8;
            Set_Visible
              (Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)), not Finished);
         when others =>
            return;
      end case;
      if (Action = CREATELINK or Action = COPY or Action = MOVE)
        and then (not Settings.ShowPreview) and then (not Finished) then
         Set_Position
           (FilesPaned,
            Gint(Float(Get_Allocated_Width(Gtk_Widget(Window))) * 0.3));
         Show_All(Get_Child2(FilesPaned));
      end if;
      if (Action = COPY or Action = MOVE) then
         if not Finished then
            LoadDirectory(To_String(CurrentDirectory), "fileslist2");
            Set_Markup
              (Gtk_Label
                 (Get_Label_Widget
                    (Gtk_Frame
                       (Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 1)))),
               "<b>" & Gettext("Destination directory") & "</b>");
            Set_Visible_Child_Name(InfoStack, "destination");
         else
            Hide(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
         end if;
         Set_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 9)), not Finished);
      end if;
      if Action = DELETETRASH and then Finished then
         if Gtk.List_Store.N_Children
             (-(Gtk.Tree_Model_Filter.Get_Model
                 (-(Gtk.Tree_Model_Sort.Get_Model
                     (-(Gtk.Tree_View.Get_Model(DirectoryView))))))) =
           0 then
            Hide(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)));
            Hide(Gtk_Widget(Get_Nth_Item(ActionToolBar, 8)));
         else
            Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)));
            Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 8)));
         end if;
      else
         if Action /= SHOWTRASH then
            Hide(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)));
         end if;
         for Index of ButtonsIndexes loop
            if Index /= CurrentButtonIndex then
               Set_Visible
                 (Gtk_Widget(Get_Nth_Item(ActionToolBar, Gint(Index))),
                  Finished);
            end if;
         end loop;
      end if;
      if Finished then
         Set_Title
           (Gtk_Header_Bar(Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
            "");
         Show_All(Gtk_Widget(ItemToolBar));
         if not Settings.ShowPreview then
            Set_Position(FilesPaned, Get_Allocated_Width(Gtk_Widget(Window)));
            Hide(Get_Child2(FilesPaned));
         else
            SetBookmarkButton;
         end if;
      else
         if Action /= SHOWTRASH then
            Hide(Gtk_Widget(ItemToolBar));
         end if;
         case Action is
            when CREATEFILE =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Creating empty file"));
            when CREATEDIRECTORY =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Creating new directory"));
            when CREATELINK =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Creating new link"));
            when RENAME =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Renaming file or directory"));
            when COPY =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Copying files and directories"));
               Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 2)));
               Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 1)));
            when MOVE =>
               Set_Title
                 (Gtk_Header_Bar
                    (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                  Gettext("Moving files and directories"));
               Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 2)));
               Show_All(Gtk_Widget(Get_Nth_Item(ActionToolBar, 1)));
            when DELETE | DELETETRASH =>
               if Settings.DeleteFiles or Action = DELETETRASH then
                  Set_Title
                    (Gtk_Header_Bar
                       (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                     Gettext("Deleting files and directories"));
               else
                  Set_Title
                    (Gtk_Header_Bar
                       (Get_Child(Gtk_Box(Get_Child(Gtk_Bin(Window))), 0)),
                     Gettext("Moving files and directories to trash"));
               end if;
            when others =>
               null;
         end case;
      end if;
   end ToggleToolButtons;

   procedure ToggleActionButtons is
      Visible: Boolean;
   begin
      if N_Children(Get_Model(DirectoryView), Null_Iter) = 0 then
         Visible := False;
      else
         Visible := True;
      end if;
      if NewAction /= SHOWTRASH then
         for I in 5 .. 8 loop
            Set_Visible
              (Gtk_Widget(Get_Nth_Item(ActionToolBar, Gint(I))), Visible);
         end loop;
      else
         Set_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 8)), Visible);
         Set_Visible(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)), Visible);
      end if;
   end ToggleActionButtons;

   procedure SetProgressBar(Amount: Positive) is
      ProgressBar: constant Gtk_Widget :=
        Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 3);
   begin
      Show_All(ProgressBar);
      Set_Fraction(Gtk_Progress_Bar(ProgressBar), 0.0);
      ProgressAmount := Amount;
      ProgressIndex := 1;
   end SetProgressBar;

   procedure UpdateProgressBar is
      ProgressBar: constant Gtk_Progress_Bar :=
        Gtk_Progress_Bar
          (Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 3));
   begin
      ProgressIndex := ProgressIndex + 1;
      Set_Fraction
        (ProgressBar, Gdouble(ProgressIndex) / Gdouble(ProgressAmount));
   end UpdateProgressBar;

   procedure RemoveChild(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      Destroy(Widget);
   end RemoveChild;

end Utils;
