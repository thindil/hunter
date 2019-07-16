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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Button; use Gtk.Button;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Radio_Tool_Button; use Gtk.Radio_Tool_Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Widget; use Gtk.Widget;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with MoveItems; use MoveItems;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;

package body ShowItems is

   -- ****iv* ShowItems/DesktopFile
   -- FUNCTION
   -- Name of .desktop file or name of application associated with selected
   -- file.
   -- SOURCE
   DesktopFile: Unbounded_String;
   -- ****

   -- ****if* ShowItems/FindFileName
   -- FUNCTION
   -- Find name of associated program with selected file. If found, replace
   -- .desktop file name with name of application.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content of currently selected directory
   -- Path  - Gtk_Tree_Path to selected element in Model
   -- Iter  - Gtk_Tree_Iter to selected element in Model
   -- SOURCE
   function FindFileName
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
      pragma Unreferenced(Path);
      -- ****
   begin
      if Get_String(Model, Iter, 1) = To_String(DesktopFile) then
         DesktopFile := To_Unbounded_String(Get_String(Model, Iter, 0));
         return True;
      end if;
      return False;
   end FindFileName;

   -- ****if* ShowItems/GetSelectedItems
   -- FUNCTION
   -- Add selected file or directory to SelectedItems list.
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with content of currently selected directory
   -- Path  - Gtk_Tree_Path to selected element in Model
   -- Iter  - Gtk_Tree_Iter to selected element in Model
   -- SOURCE
   procedure GetSelectedItems
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter) is
      pragma Unreferenced(Path);
      -- ****
   begin
      if CurrentDirectory = To_Unbounded_String("/") then
         CurrentDirectory := Null_Unbounded_String;
      end if;
      SelectedItems.Append
        (CurrentDirectory &
         To_Unbounded_String("/" & Get_String(Model, Iter, 0)));
   end GetSelectedItems;

   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class) is
      Amount: Natural := 0;
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
      SelectedPath: Unbounded_String;
      ObjectsNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("lblfiletype"),
         To_Unbounded_String("lblfiletype2"),
         To_Unbounded_String("btnprogram"), To_Unbounded_String("lblprogram2"),
         To_Unbounded_String("cbtnownerexecute"),
         To_Unbounded_String("cbtngroupexecute"),
         To_Unbounded_String("cbtnothersexecute"));
   begin
      if Setting or CurrentSelected = Null_Unbounded_String then
         return;
      end if;
      Setting := True;
      SelectedPath :=
        To_Unbounded_String(Full_Name(To_String(CurrentSelected)));
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblname")), To_String(SelectedPath));
      Set_Label(Gtk_Label(Get_Object(Object, "lblsize2")), "Size:");
      if Is_Symbolic_Link(To_String(CurrentSelected)) then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblname2")), Gettext("Links to:"));
      else
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblname2")), Gettext("Full path:"));
      end if;
      for Name of ObjectsNames loop
         Hide(Gtk_Widget(Get_Object(Object, To_String(Name))));
      end loop;
      if Is_Regular_File(To_String(SelectedPath)) then
         for Name of ObjectsNames loop
            Show_All(Gtk_Widget(Get_Object(Object, To_String(Name))));
         end loop;
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblsize")),
            CountFileSize(Size(To_String(SelectedPath))));
         Set_Label
           (Gtk_Label(Get_Object(Object, "lbllastmodified")),
            Ada.Calendar.Formatting.Image
              (Modification_Time(To_String(SelectedPath)), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset));
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblfiletype")),
            GetMimeType(To_String(SelectedPath)));
         if not CanBeOpened(GetMimeType(To_String(SelectedPath))) then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnprogram")), Gettext("none"));
         else
            declare
               ProcessDesc: Process_Descriptor;
               Result: Expect_Match;
               ExecutableName: constant String := FindExecutable("xdg-mime");
            begin
               if ExecutableName = "" then
                  return;
               end if;
               Non_Blocking_Spawn
                 (ProcessDesc, ExecutableName,
                  Argument_String_To_List
                    ("query default " &
                     GetMimeType(To_String(SelectedPath))).all);
               Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
               if Result = 1 then
                  DesktopFile :=
                    To_Unbounded_String(Expect_Out_Match(ProcessDesc));
                  Foreach
                    (Gtk_List_Store(Get_Object(Object, "applicationslist")),
                     FindFileName'Access);
                  if Index(DesktopFile, ".desktop") = 0 then
                     Set_Label
                       (Gtk_Button(Get_Object(Object, "btnprogram")),
                        To_String(DesktopFile));
                  else
                     Set_Label
                       (Gtk_Label(Get_Object(Object, "lblprogram")),
                        To_String(DesktopFile) & Gettext(" (not installed)"));
                  end if;
               end if;
               Close(ProcessDesc);
            end;
         end if;
      elsif Is_Directory(To_String(SelectedPath)) then
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblsize2")), Gettext("Elements:"));
         if Is_Read_Accessible_File(To_String(SelectedPath)) then
            Open(Directory, To_String(SelectedPath));
            loop
               Read(Directory, FileName, Last);
               exit when Last = 0;
               Amount := Amount + 1;
            end loop;
            Close(Directory);
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblsize")),
               Natural'Image(Amount - 2));
         else
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblsize")), Gettext("Unknown"));
         end if;
         Set_Label
           (Gtk_Label(Get_Object(Object, "lbllastmodified")),
            Ada.Calendar.Formatting.Image
              (Modification_Time(To_String(SelectedPath))));
      else
         if SelectedPath = "" then
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblname")), Gettext("Unknown"));
         end if;
         Set_Label
           (Gtk_Label(Get_Object(Object, "lblsize")), Gettext("Unknown"));
         for I in 5 .. 7 loop
            Show_All
              (Gtk_Widget(Get_Object(Object, To_String(ObjectsNames(I)))));
         end loop;
         Set_Label
           (Gtk_Label(Get_Object(Object, "lbllastmodified")),
            Gettext("Unknown"));
      end if;
      declare
         ProcessDesc: Process_Descriptor;
         Result: Expect_Match;
         FileStats: Unbounded_String;
         Tokens: Slice_Set;
         ButtonNames: constant array(3 .. 11) of Unbounded_String :=
           (To_Unbounded_String("cbtnownerread"),
            To_Unbounded_String("cbtnownerwrite"),
            To_Unbounded_String("cbtnownerexecute"),
            To_Unbounded_String("cbtngroupread"),
            To_Unbounded_String("cbtngroupwrite"),
            To_Unbounded_String("cbtngroupexecute"),
            To_Unbounded_String("cbtnothersread"),
            To_Unbounded_String("cbtnotherswrite"),
            To_Unbounded_String("cbtnothersexecute"));
         CanChange: Boolean := False;
         Button: Gtk_Toggle_Button;
         Arguments: constant Argument_List :=
           (new String'("-c""%A %U %G"),
            new String'(To_String(CurrentSelected)));
      begin
         Non_Blocking_Spawn(ProcessDesc, "stat", Arguments);
         Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
         if Result = 1 then
            FileStats := To_Unbounded_String(Expect_Out_Match(ProcessDesc));
            Create(Tokens, To_String(FileStats), " ");
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblowner")), Slice(Tokens, 2));
            Set_Label
              (Gtk_Label(Get_Object(Object, "lblgroup")),
               Slice(Tokens, 3)
                 (Slice(Tokens, 3)'First .. Slice(Tokens, 3)'Last));
            if Value("USER") = Slice(Tokens, 2) then
               CanChange := True;
            end if;
            for I in ButtonNames'Range loop
               Button :=
                 Gtk_Toggle_Button
                   (Get_Object(Object, To_String(ButtonNames(I))));
               if Slice(Tokens, 1)(I) = '-' then
                  Set_Active(Button, False);
               else
                  Set_Active(Button, True);
               end if;
               Set_Sensitive(Gtk_Widget(Button), CanChange);
            end loop;
         end if;
         Close(ProcessDesc);
      exception
         when Process_Died =>
            return;
      end;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblframe")), Gettext("Information"));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "infostack")), "info");
      if not Get_Active
          (Gtk_Radio_Tool_Button(Get_Object(Object, "btnfileinfo"))) then
         Set_Active
           (Gtk_Radio_Tool_Button(Get_Object(Object, "btnfileinfo")), True);
      end if;
      Setting := False;
   end ShowItemInfo;

   procedure PreviewItem(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Setting or (not Settings.ShowPreview) then
         return;
      end if;
      SetBookmarkButton;
      if Is_Directory(To_String(CurrentSelected)) then
         Show_All(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Show_All(Gtk_Widget(Get_Object(Object, "btnpreview")));
         Show_All(Gtk_Widget(Get_Object(Object, "btnopen")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrollimage")));
         Hide(Gtk_Widget(Get_Object(Object, "btnrun")));
         LoadDirectory(To_String(CurrentSelected), "fileslist1");
      else
         Show_All(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Clear(Gtk_Image(Get_Object(Object, "imgpreview")));
         Hide(Gtk_Widget(Get_Object(Object, "scrollimage")));
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
            Buffer: constant Gtk_Text_Buffer :=
              Get_Buffer(Gtk_Text_View(Get_Object(Object, "filetextview")));
            Iter: Gtk_Text_Iter;
         begin
            Set_Text(Buffer, "");
            Get_Start_Iter(Buffer, Iter);
            if not Is_Executable_File(To_String(CurrentSelected)) then
               Hide(Gtk_Widget(Get_Object(Builder, "btnrun")));
            end if;
            if MimeType(1 .. 4) = "text" then
               declare
                  ExecutableName: constant String :=
                    FindExecutable("highlight");
                  Success, FirstLine: Boolean;
                  File: File_Type;
                  FileLine: Unbounded_String;
                  procedure LoadFile is
                  begin
                     Open(File, In_File, To_String(CurrentSelected));
                     while not End_Of_File(File) loop
                        Insert(Buffer, Iter, Get_Line(File) & LF);
                     end loop;
                     Close(File);
                  end LoadFile;
               begin
                  if ExecutableName = "" then
                     LoadFile;
                     goto Set_UI;
                  end if;
                  Spawn
                    (ExecutableName,
                     Argument_String_To_List
                       ("--out-format=pango --force --output=" &
                        Value("HOME") & "/.cache/hunter/highlight.tmp " &
                        To_String(CurrentSelected)).all,
                     Success);
                  if not Success then
                     LoadFile;
                  else
                     Open
                       (File, In_File,
                        Value("HOME") & "/.cache/hunter/highlight.tmp");
                     FirstLine := True;
                     while not End_Of_File(File) loop
                        FileLine := To_Unbounded_String(Get_Line(File));
                        if FirstLine then
                           FileLine :=
                             Unbounded_Slice
                               (FileLine, Index(FileLine, ">") + 1,
                                Length(FileLine));
                           FirstLine := False;
                        end if;
                        exit when End_Of_File(File);
                        Insert(Buffer, Iter, To_String(FileLine) & LF);
                     end loop;
                     Close(File);
                     Delete_File
                       (Value("HOME") & "/.cache/hunter/highlight.tmp");
                  end if;
               exception
                  when others =>
                     LoadFile;
               end;
            elsif MimeType(1 .. 5) = "image" then
               declare
                  Pixbuf: Gdk_Pixbuf;
                  Error: GError;
                  Image: constant Gtk_Image :=
                    Gtk_Image(Get_Object(Object, "imgpreview"));
                  ScaleFactor: Float;
                  MaxHeight: constant Gint :=
                    Get_Allocated_Height
                      (Gtk_Widget(Get_Object(Object, "infostack"))) -
                    5;
                  MaxWidth: constant Gint :=
                    Get_Allocated_Width
                      (Gtk_Widget(Get_Object(Object, "infostack"))) -
                    5;
               begin
                  Gdk_New_From_File(Pixbuf, To_String(CurrentSelected), Error);
                  if Error /= null then
                     ShowMessage
                       (Gettext("Could not load image file: ") &
                        To_String(CurrentSelected));
                     return;
                  end if;
                  if Settings.ScaleImages then
                     if (Get_Width(Pixbuf) - MaxWidth) >
                       (Get_Height(Pixbuf) - MaxHeight) then
                        ScaleFactor :=
                          Float(MaxWidth) / Float(Get_Width(Pixbuf));
                     else
                        ScaleFactor :=
                          Float(MaxHeight) / Float(Get_Height(Pixbuf));
                     end if;
                     Pixbuf :=
                       Scale_Simple
                         (Pixbuf,
                          Gint
                            (Float'Floor
                               (Float(Get_Width(Pixbuf)) * ScaleFactor)),
                          Gint
                            (Float'Floor
                               (Float(Get_Height(Pixbuf)) * ScaleFactor)));
                  end if;
                  Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
                  Set(Image, Pixbuf);
                  Show_All(Gtk_Widget(Get_Object(Object, "scrollimage")));
               end;
            else
               Hide(Gtk_Widget(Get_Object(Object, "btnpreview")));
               if not CanBeOpened(MimeType) then
                  Hide(Gtk_Widget(Get_Object(Object, "btnopen")));
               end if;
               Set_Active
                 (Gtk_Radio_Tool_Button(Get_Object(Object, "btnfileinfo")),
                  True);
               return;
            end if;
         end;
      end if;
      <<Set_UI>>
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblframe")), Gettext("Preview"));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "infostack")), "preview");
      if Get_Active
          (Gtk_Radio_Tool_Button(Get_Object(Object, "btnfileinfo"))) then
         Setting := True;
         Set_Active
           (Gtk_Radio_Tool_Button(Get_Object(Object, "btnpreview")), True);
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "infostack")), "preview");
         Setting := False;
      end if;
   end PreviewItem;

   procedure ShowItem(Object: access Gtkada_Builder_Record'Class) is
   begin
      SelectedItems.Clear;
      Selected_Foreach
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treefiles"))),
         GetSelectedItems'Access);
      if SelectedItems.Length > 1 then
         Hide(Gtk_Widget(Get_Object(Object, "scrolltext")));
         Hide(Gtk_Widget(Get_Object(Object, "scrolllist")));
         Hide(Gtk_Widget(Get_Object(Object, "itemtoolbar")));
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblframe")), Gettext("Preview"));
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "infostack")), "preview");
         return;
      elsif SelectedItems.Length = 0 then
         PreviewItem(Object);
         return;
      end if;
      if CurrentSelected = SelectedItems(1) then
         return;
      end if;
      CurrentSelected := SelectedItems(1);
      if Setting or (not Settings.ShowPreview) then
         SetBookmarkButton;
         return;
      end if;
      if Get_Active(Gtk_Toggle_Tool_Button(Get_Object(Object, "btncut"))) then
         MoveItemsList := SelectedItems;
         return;
      end if;
      if Get_Active(Gtk_Toggle_Tool_Button(Get_Object(Object, "btncopy"))) then
         CopyItemsList := SelectedItems;
         return;
      end if;
      if NewAction = CREATELINK then
         LinkTarget := CurrentSelected;
         return;
      end if;
      Show_All(Gtk_Widget(Get_Object(Object, "itemtoolbar")));
      Set_Active
        (Gtk_Radio_Tool_Button(Get_Object(Object, "btnpreview")), True);
      PreviewItem(Object);
   end ShowItem;

   procedure SetAssociated(Object: access Gtkada_Builder_Record'Class) is
      Pid: GNAT.OS_Lib.Process_Id;
      ProgramIter: Gtk_Tree_Iter;
      ProgramModel: Gtk_Tree_Model;
      ExecutableName: constant String := FindExecutable("xdg-mime");
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeapplications"))),
         ProgramModel, ProgramIter);
      if ProgramIter /= Null_Iter then
         if ExecutableName = "" then
            return;
         end if;
         Pid :=
           Non_Blocking_Spawn
             (ExecutableName,
              Argument_String_To_List
                ("default " & Get_String(ProgramModel, ProgramIter, 1) & " " &
                 GetMimeType(To_String(CurrentSelected))).all);
         if Pid = GNAT.OS_Lib.Invalid_Pid then
            ShowMessage(Gettext("Could not set new associated file."));
         else
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnprogram")),
               Get_String(ProgramModel, ProgramIter, 0));
         end if;
         Set_Active
           (Gtk_Toggle_Button(Get_Object(Object, "btnprogram")), False);
      end if;
   end SetAssociated;

   procedure SetPermission(Object: access Gtkada_Builder_Record'Class) is
      ButtonNames: constant array(2 .. 10) of Unbounded_String :=
        (To_Unbounded_String("cbtnownerread"),
         To_Unbounded_String("cbtnownerwrite"),
         To_Unbounded_String("cbtnownerexecute"),
         To_Unbounded_String("cbtngroupread"),
         To_Unbounded_String("cbtngroupwrite"),
         To_Unbounded_String("cbtngroupexecute"),
         To_Unbounded_String("cbtnothersread"),
         To_Unbounded_String("cbtnotherswrite"),
         To_Unbounded_String("cbtnothersexecute"));
      UserPermission, GroupPermission, OthersPermission: Natural := 0;
      Success: Boolean;
      Arguments: Argument_List(1 .. 2);
   begin
      if Setting then
         return;
      end if;
      for I in ButtonNames'Range loop
         if Get_Active
             (Gtk_Toggle_Button
                (Get_Object(Object, To_String(ButtonNames(I))))) then
            case I is
               when 2 =>
                  UserPermission := UserPermission + 4;
               when 3 =>
                  UserPermission := UserPermission + 2;
               when 4 =>
                  UserPermission := UserPermission + 1;
               when 5 =>
                  GroupPermission := GroupPermission + 4;
               when 6 =>
                  GroupPermission := GroupPermission + 2;
               when 7 =>
                  GroupPermission := GroupPermission + 1;
               when 8 =>
                  OthersPermission := OthersPermission + 4;
               when 9 =>
                  OthersPermission := OthersPermission + 2;
               when 10 =>
                  OthersPermission := OthersPermission + 1;
            end case;
         end if;
      end loop;
      Arguments :=
        (new String'
           (Trim(Natural'Image(UserPermission), Both) &
            Trim(Natural'Image(GroupPermission), Both) &
            Trim(Natural'Image(OthersPermission), Both)),
         new String'(To_String(CurrentSelected)));
      Spawn(Locate_Exec_On_Path("chmod").all, Arguments, Success);
      if not Success then
         ShowMessage
           (Gettext("Could not change permissions for ") &
            To_String(CurrentSelected));
      end if;
   end SetPermission;

   procedure SetBookmarkButton is
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnaddbookmark")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnremovebookmark")));
      if not Is_Directory(To_String(CurrentSelected)) then
         return;
      end if;
      for Bookmark of BookmarksList loop
         if Bookmark.Path = CurrentSelected then
            Show_All(Gtk_Widget(Get_Object(Builder, "btnremovebookmark")));
            return;
         end if;
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "btnaddbookmark")));
   end SetBookmarkButton;

end ShowItems;
