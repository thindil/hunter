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
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Flow_Box; use Gtk.Flow_Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Menu_Button; use Gtk.Menu_Button;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Radio_Tool_Button; use Gtk.Radio_Tool_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Values; use Glib.Values;
with Pango.Enums; use Pango.Enums;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with MoveItems; use MoveItems;
with Messages; use Messages;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with SearchItems; use SearchItems;
with Toolbars; use Toolbars;
with Utils; use Utils;

package body ShowItems is

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
      SelectedItems.Append(To_Unbounded_String(Get_String(Model, Iter, 6)));
   end GetSelectedItems;

   -- ****if* ShowItems/ShowItemInfo
   -- FUNCTION
   -- Show detailed information (name, size, modification date, etc) about
   -- selected file or directory.
   -- PARAMETERS
   -- Self - Gtk_Tool_Button clicked. Unused. Can be null
   -- SOURCE
   procedure ShowItemInfo(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      Amount: Natural := 0;
      Directory: Dir_Type;
      Last: Natural;
      FileName: String(1 .. 1024);
      SelectedPath: Unbounded_String;
      InfoGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(InfoStack, "info"));
      Widgets: constant array(1 .. 7) of Gtk_Widget :=
        (Get_Child_At(InfoGrid, 0, 3), Get_Child_At(InfoGrid, 1, 3),
         Get_Child_At(InfoGrid, 0, 4), Get_Child_At(InfoGrid, 1, 4),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 3),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 3),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 3));
   begin
      if Setting or CurrentSelected = Null_Unbounded_String then
         return;
      end if;
      Setting := True;
      SelectedPath :=
        To_Unbounded_String(Full_Name(To_String(CurrentSelected)));
      Set_Label
        (Gtk_Label(Get_Child_At(InfoGrid, 1, 0)), To_String(SelectedPath));
      Set_Label(Gtk_Label(Get_Child_At(InfoGrid, 0, 1)), Gettext("Size:"));
      if Is_Symbolic_Link(To_String(CurrentSelected)) then
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 0, 0)), Gettext("Links to:"));
      else
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 0, 0)), Gettext("Full path:"));
      end if;
      for Widget of Widgets loop
         Hide(Widget);
      end loop;
      if Is_Regular_File(To_String(SelectedPath)) then
         for Widget of Widgets loop
            Show_All(Widget);
         end loop;
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 1)),
            CountFileSize(Size(To_String(SelectedPath))));
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 2)),
            Ada.Calendar.Formatting.Image
              (Modification_Time(To_String(SelectedPath)), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset));
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 3)),
            GetMimeType(To_String(SelectedPath)));
         if not CanBeOpened(GetMimeType(To_String(SelectedPath))) then
            Set_Label
              (Gtk_Button(Get_Child_At(InfoGrid, 1, 4)), Gettext("none"));
         else
            declare
               ProcessDesc: Process_Descriptor;
               Result: Expect_Match;
               ExecutableName: constant String := FindExecutable("xdg-mime");
               DesktopFile: Unbounded_String;
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
                  GetProgramName(DesktopFile);
                  if Index(DesktopFile, ".desktop") = 0 then
                     Set_Label
                       (Gtk_Button(Get_Child_At(InfoGrid, 1, 4)),
                        To_String(DesktopFile));
                  else
                     Set_Label
                       (Gtk_Button(Get_Child_At(InfoGrid, 1, 4)),
                        To_String(DesktopFile) & Gettext(" (not installed)"));
                  end if;
               end if;
               Close(ProcessDesc);
            end;
         end if;
      elsif Is_Directory(To_String(SelectedPath)) then
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 0, 1)), Gettext("Elements:"));
         if Is_Read_Accessible_File(To_String(SelectedPath)) then
            Open(Directory, To_String(SelectedPath));
            loop
               Read(Directory, FileName, Last);
               exit when Last = 0;
               Amount := Amount + 1;
            end loop;
            Close(Directory);
            Set_Label
              (Gtk_Label(Get_Child_At(InfoGrid, 1, 1)),
               Natural'Image(Amount - 2));
         else
            Set_Label
              (Gtk_Label(Get_Child_At(InfoGrid, 1, 1)), Gettext("Unknown"));
         end if;
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 2)),
            Ada.Calendar.Formatting.Image
              (Modification_Time(To_String(SelectedPath))));
      else
         if SelectedPath = "" then
            Set_Label
              (Gtk_Label(Get_Child_At(InfoGrid, 1, 0)), Gettext("Unknown"));
         end if;
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 1)), Gettext("Unknown"));
         for I in 5 .. 7 loop
            Show_All(Widgets(I));
         end loop;
         Set_Label
           (Gtk_Label(Get_Child_At(InfoGrid, 1, 2)), Gettext("Unknown"));
      end if;
      declare
         ProcessDesc: Process_Descriptor;
         Result: Expect_Match;
         FileStats: Unbounded_String;
         Tokens: Slice_Set;
         Buttons: constant array(3 .. 11) of Gtk_Widget :=
           (Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 1),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 2),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 3),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 1),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 2),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 3),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 1),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 2),
            Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 3));
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
              (Gtk_Label(Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 0)),
               Slice(Tokens, 2));
            Set_Label
              (Gtk_Label(Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 0)),
               Slice(Tokens, 3)
                 (Slice(Tokens, 3)'First .. Slice(Tokens, 3)'Last));
            if Value("USER") = Slice(Tokens, 2) then
               CanChange := True;
            end if;
            for I in Buttons'Range loop
               Button := Gtk_Toggle_Button(Buttons(I));
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
      Set_Markup
        (Gtk_Label
           (Get_Label_Widget
              (Gtk_Frame(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 1)))),
         "<b>" & Gettext("Information") & "</b>");
      Set_Visible_Child_Name(InfoStack, "info");
      if not Get_Active
          (Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 5))) then
         Set_Active(Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 5)), True);
      end if;
      Setting := False;
   end ShowItemInfo;

   -- ****if* ShowItems/RemoveTag
   -- FUNCTION
   -- Remove selected text tag from preview window.
   -- PARAMETERS
   -- Tag - Gtk_Text_Tag to remove
   -- SOURCE
   procedure RemoveTag(Tag: not null access Gtk_Text_Tag_Record'Class) is
   -- ****
   begin
      if Get_Property(GObject(Tag), Gtk.Text_Tag.Name_Property) /= "" then
         return;
      end if;
      Remove
        (Get_Tag_Table
           (Get_Buffer
              (Gtk_Text_View
                 (Get_Child
                    (Gtk_Scrolled_Window
                       (Get_Child_By_Name(InfoStack, "preview")))))),
         Tag);
   end RemoveTag;

   -- ****if* ShowItems/UpdateImage
   -- FUNCTION
   -- If scaling images in preview is enabled, scale it on resize preview of it
   -- PARAMETERS
   -- Self       - Gtk_Image which was resized. Unused.
   -- Allocation - Gtk_Allocation for size. Unused.
   -- SOURCE
   procedure UpdateImage
     (Self: access Gtk_Widget_Record'Class; Allocation: Gtk_Allocation) is
      pragma Unreferenced(Allocation);
      -- ****
      Pixbuf: Gdk_Pixbuf;
      ScaleFactor: Float;
      MaxHeight: Gint;
      MaxWidth: Gint;
      Error: GError;
   begin
      if not Settings.ScaleImages then
         return;
      end if;
      Clear(Gtk_Image(Self));
      Gdk_New_From_File(Pixbuf, To_String(CurrentSelected), Error);
      if Error /= null then
         ShowMessage
           (Gettext("Could not load image file: ") &
            To_String(CurrentSelected));
         return;
      end if;
      MaxHeight := Get_Allocated_Height(Gtk_Widget(InfoStack)) - 5;
      MaxWidth := Get_Allocated_Width(Gtk_Widget(InfoStack)) - 5;
      if (Get_Width(Pixbuf) - MaxWidth) > (Get_Height(Pixbuf) - MaxHeight) then
         ScaleFactor := Float(MaxWidth) / Float(Get_Width(Pixbuf));
      else
         ScaleFactor := Float(MaxHeight) / Float(Get_Height(Pixbuf));
      end if;
      Pixbuf :=
        Scale_Simple
          (Pixbuf, Gint(Float'Floor(Float(Get_Width(Pixbuf)) * ScaleFactor)),
           Gint(Float'Floor(Float(Get_Height(Pixbuf)) * ScaleFactor)));
      Set(Gtk_Image(Self), Pixbuf);
      Unref(Pixbuf);
   end UpdateImage;

   procedure PreviewItem(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      PreviewScroll: constant Gtk_Scrolled_Window :=
        Gtk_Scrolled_Window(Get_Child_By_Name(InfoStack, "preview"));
   begin
      if Setting or (not Settings.ShowPreview) then
         return;
      end if;
      SetBookmarkButton;
      Foreach(PreviewScroll, RemoveChild'Access);
      Show_All(Gtk_Widget(Get_Nth_Item(ItemToolBar, 1)));
      if Is_Directory(To_String(CurrentSelected)) then
         Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 0)));
         if Get_Child(PreviewScroll) /= null then
            goto Set_UI;
         end if;
         declare
            PreviewView: constant Gtk_Tree_View :=
              Gtk_Tree_View_New_With_Model
                (+(Gtk_Tree_Model_Filter_Filter_New
                    (+(Gtk_List_Store_Newv
                        ((GType_String, GType_Uint, GType_String))))));
            Area: Gtk_Cell_Area_Box;
            Renderer: constant Gtk_Cell_Renderer_Text :=
              Gtk_Cell_Renderer_Text_New;
            Renderer2: constant Gtk_Cell_Renderer_Pixbuf :=
              Gtk_Cell_Renderer_Pixbuf_New;
            Column: Gtk_Tree_View_Column;
         begin
            Set_Enable_Search(PreviewView, False);
            Set_Headers_Clickable(PreviewView, True);
            Area := Gtk_Cell_Area_Box_New;
            Pack_Start(Area, Renderer2, False);
            Add_Attribute(Area, Renderer2, "icon-name", 2);
            Pack_Start(Area, Renderer, True);
            Add_Attribute(Area, Renderer, "text", 0);
            Column := Gtk_Tree_View_Column_New_With_Area(Area);
            Set_Sort_Column_Id(Column, 0);
            Set_Title(Column, Gettext("Name"));
            if Append_Column(PreviewView, Column) /= 1 then
               return;
            end if;
            Set_Visible_Func(-(Get_Model(PreviewView)), VisibleItems'Access);
            Show_All(Gtk_Widget(Get_Nth_Item(ItemToolBar, 4)));
            Add(PreviewScroll, PreviewView);
            LoadDirectory(To_String(CurrentSelected), "fileslist1");
            Show_All(PreviewScroll);
         end;
      else
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
         begin
            if not Is_Executable_File(To_String(CurrentSelected)) then
               Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 0)));
            end if;
            if MimeType(1 .. 4) = "text" then
               declare
                  ExecutableName: constant String :=
                    FindExecutable("highlight");
                  Success, FirstLine: Boolean;
                  File: File_Type;
                  FileLine, TagText: Unbounded_String;
                  Tag: Gtk_Text_Tag;
                  StartIndex, EndIndex, StartColor: Natural;
                  Iter: Gtk_Text_Iter;
                  TextView: constant Gtk_Text_View := Gtk_Text_View_New;
                  Buffer: constant Gtk_Text_Buffer := Get_Buffer(TextView);
                  procedure LoadFile is
                  begin
                     Open(File, In_File, To_String(CurrentSelected));
                     while not End_Of_File(File) loop
                        Insert(Buffer, Iter, Get_Line(File) & LF);
                     end loop;
                     Close(File);
                  end LoadFile;
               begin
                  Set_Wrap_Mode(TextView, Wrap_Word);
                  Set_Editable(TextView, False);
                  Set_Cursor_Visible(TextView, False);
                  Get_Start_Iter(Buffer, Iter);
                  if not Settings.ColorText or ExecutableName = "" then
                     LoadFile;
                     goto Set_UI;
                  end if;
                  Set("LD_LIBRARY_PATH", To_String(Ld_Library_Path));
                  Spawn
                    (ExecutableName,
                     Argument_String_To_List
                       ("--out-format=pango --force --output=" &
                        Value("HOME") &
                        "/.cache/hunter/highlight.tmp --base16 --style=" &
                        To_String(Settings.ColorTheme) & " " &
                        To_String(CurrentSelected)).all,
                     Success);
                  Clear("LD_LIBRARY_PATH");
                  if not Success then
                     LoadFile;
                     goto Set_UI;
                  end if;
                  Foreach(Get_Tag_Table(Buffer), RemoveTag'Access);
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
                     loop
                        StartIndex := Index(FileLine, "&gt;");
                        exit when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 3, ">");
                     end loop;
                     loop
                        StartIndex := Index(FileLine, "&lt;");
                        exit when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 3, "<");
                     end loop;
                     loop
                        StartIndex := Index(FileLine, "&amp;");
                        exit when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 4, "&");
                     end loop;
                     StartIndex := 1;
                     loop
                        StartIndex := Index(FileLine, "<span", StartIndex);
                        exit when StartIndex = 0;
                        if StartIndex > 1 then
                           Insert
                             (Buffer, Iter,
                              Slice(FileLine, 1, StartIndex - 1));
                        end if;
                        EndIndex := Index(FileLine, ">", StartIndex);
                        TagText :=
                          Unbounded_Slice(FileLine, StartIndex, EndIndex);
                        Tag := Create_Tag(Buffer);
                        StartColor := Index(TagText, "foreground=");
                        if Index(TagText, "foreground=") > 0 then
                           Set_Property
                             (GObject(Tag), Gtk.Text_Tag.Foreground_Property,
                              Slice
                                (TagText, StartColor + 12, StartColor + 18));
                        elsif Index(TagText, "style=""italic""") > 0 then
                           Set_Property
                             (GObject(Tag), Gtk.Text_Tag.Style_Property,
                              Pango_Style_Italic);
                        elsif Index(TagText, "weight=""bold""") > 0 then
                           Set_Property
                             (GObject(Tag), Gtk.Text_Tag.Weight_Property,
                              Pango_Weight_Bold);
                        end if;
                        StartIndex := StartIndex + Length(TagText);
                        EndIndex := Index(FileLine, "</span>", StartIndex) - 1;
                        if EndIndex > 0 then
                           Insert_With_Tags
                             (Buffer, Iter,
                              Slice(FileLine, StartIndex, EndIndex), Tag);
                        else
                           Insert
                             (Buffer, Iter,
                              Slice(FileLine, StartIndex, Length(FileLine)));
                        end if;
                        StartIndex := 1;
                        FileLine :=
                          Unbounded_Slice
                            (FileLine, EndIndex + 8, Length(FileLine));
                     end loop;
                     Insert(Buffer, Iter, To_String(FileLine) & LF);
                  end loop;
                  Close(File);
                  Delete_File(Value("HOME") & "/.cache/hunter/highlight.tmp");
                  Add(PreviewScroll, TextView);
                  Show_All(PreviewScroll);
               end;
            elsif MimeType(1 .. 5) = "image" then
               declare
                  Pixbuf: Gdk_Pixbuf;
                  Error: GError;
                  Image: constant Gtk_Image := Gtk_Image_New;
                  ScaleFactor: Float;
                  MaxHeight: constant Gint :=
                    Get_Allocated_Height(Gtk_Widget(InfoStack)) - 5;
                  MaxWidth: constant Gint :=
                    Get_Allocated_Width(Gtk_Widget(InfoStack)) - 5;
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
                  Set(Image, Pixbuf);
                  Add(PreviewScroll, Image);
                  Show_All(PreviewScroll);
                  On_Size_Allocate(Image, UpdateImage'Access);
               end;
            else
               Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 4)));
               if not CanBeOpened(MimeType) then
                  Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 1)));
               end if;
               Set_Active
                 (Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 5)), True);
               return;
            end if;
         end;
      end if;
      <<Set_UI>>
      Set_Markup
        (Gtk_Label
           (Get_Label_Widget
              (Gtk_Frame(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 1)))),
         "<b>" & Gettext("Preview") & "</b>");
      Set_Visible_Child_Name(InfoStack, "preview");
      if Get_Active(Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 5))) then
         Setting := True;
         Set_Active(Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 4)), True);
         Set_Visible_Child_Name(InfoStack, "preview");
         Setting := False;
      end if;
   end PreviewItem;

   procedure ShowItem(Self: access Gtk_Tree_Selection_Record'Class) is
   begin
      SelectedItems.Clear;
      Selected_Foreach(Self, GetSelectedItems'Access);
      if Get_Active
          (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 7))) then
         MoveItemsList := SelectedItems;
         return;
      end if;
      if Get_Active
          (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 6))) then
         CopyItemsList := SelectedItems;
         return;
      end if;
      if SelectedItems.Length > 1 then
         Hide(ItemToolBar);
         Set_Markup
           (Gtk_Label
              (Get_Label_Widget
                 (Gtk_Frame(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 1)))),
            "<b>" & Gettext("Preview") & "</b>");
         Set_Visible_Child_Name(InfoStack, "preview");
         return;
      elsif SelectedItems.Length = 0 then
         PreviewItem(null);
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
      if NewAction = CREATELINK then
         LinkTarget := CurrentSelected;
         return;
      end if;
      Show_All(ItemToolBar);
      Set_Active(Gtk_Radio_Tool_Button(Get_Nth_Item(ItemToolBar, 4)), True);
      PreviewItem(null);
   end ShowItem;

   -- ****if* ShowItems/SetPermission
   -- FUNCTION
   -- Set selected permissions to selected file or directory
   -- PARAMETERS
   -- Self - Gtk_Check_Button which was (un)checked. Unused. Can be null.
   -- SOURCE
   procedure SetPermission(Self: access Gtk_Toggle_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      InfoGrid: constant Gtk_Grid :=
        Gtk_Grid(Get_Child_By_Name(InfoStack, "info"));
      Buttons: constant array(2 .. 10) of Gtk_Widget :=
        (Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 1),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 2),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 5)), 3),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 1),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 2),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 6)), 3),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 1),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 2),
         Get_Child(Gtk_Box(Get_Child_At(InfoGrid, 1, 7)), 3));
      UserPermission, GroupPermission, OthersPermission: Natural := 0;
      Success: Boolean;
      Arguments: Argument_List(1 .. 2);
   begin
      if Setting then
         return;
      end if;
      for I in Buttons'Range loop
         if Get_Active(Gtk_Toggle_Button(Buttons(I))) then
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

   -- ****if* ShowItems/SetDestination
   -- FUNCTION
   -- Enter subdirectory in preview for destination directory
   -- PARAMETERS
   -- Self   - Gtk_Tree_View which triggered this code
   -- Path   - Gtk_Tree_Path to item which was activated
   -- Column - Gtk_Tree_View_Column which was activated. Unused.
   -- SOURCE
   procedure SetDestination
     (Self: access Gtk_Tree_View_Record'Class; Path: Gtk_Tree_Path;
      Column: not null access Gtk_Tree_View_Column_Record'Class) is
      pragma Unreferenced(Column);
      -- ****
   begin
      CurrentSelected :=
        CurrentDirectory &
        To_Unbounded_String
          ("/" &
           Get_String(Get_Model(Self), Get_Iter(Get_Model(Self), Path), 0));
      DestinationPath := CurrentSelected;
      if Is_Directory(To_String(CurrentSelected)) then
         if not Is_Read_Accessible_File(To_String(CurrentSelected)) then
            ShowMessage(Gettext("You can't enter this directory."));
            return;
         end if;
         if CurrentDirectory = To_Unbounded_String("/") then
            CurrentDirectory := Null_Unbounded_String;
         end if;
         CurrentDirectory := CurrentSelected;
         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
         Set_Cursor(Self, Gtk_Tree_Path_New_From_String("0"), null, False);
         Grab_Focus(Gtk_Widget(Self));
      end if;
   end SetDestination;

   procedure CreateShowItemsUI is
      ProgramsButton: constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      InfoGrid: constant Gtk_Grid := Gtk_Grid_New;
      Scroll: Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      procedure AddLabel(Text: String; Left, Top: Gint) is
         Label: constant Gtk_Label := Gtk_Label_New(Text);
      begin
         Set_Halign(Label, Align_Start);
         Set_Valign(Label, Align_Start);
         Attach(InfoGrid, Label, Left, Top);
      end AddLabel;
      procedure AddBox(Top: Gint) is
         Box: constant Gtk_Vbox := Gtk_Vbox_New;
         Label: constant Gtk_Label := Gtk_Label_New("");
         procedure AddButton(Text: String) is
            CheckButton: constant Gtk_Check_Button :=
              Gtk_Check_Button_New_With_Label(Text);
         begin
            On_Toggled(Gtk_Toggle_Button(CheckButton), SetPermission'Access);
            Pack_Start(Box, CheckButton, False);
         end AddButton;
      begin
         Pack_Start(Box, Label, False);
         AddButton(Gettext("Can read"));
         AddButton(Gettext("Can write"));
         AddButton(Gettext("Can execute"));
         Attach(InfoGrid, Box, 1, Top);
      end AddBox;
   begin
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ItemToolBar, 4)), PreviewItem'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ItemToolBar, 5)), ShowItemInfo'Access);
      InfoStack := Gtk_Stack_New;
      Add_Named(InfoStack, Scroll, "preview");
      Set_Halign(InfoGrid, Align_Center);
      AddLabel(Gettext("Full path:"), 0, 0);
      AddLabel("", 1, 0);
      AddLabel(Gettext("Size:"), 0, 1);
      AddLabel("", 1, 1);
      AddLabel(Gettext("Last Modified:"), 0, 2);
      AddLabel("", 1, 2);
      AddLabel(Gettext("File type:"), 0, 3);
      AddLabel("", 1, 3);
      AddLabel(Gettext("Associated program:"), 0, 4);
      Set_Popover
        (ProgramsButton, CreateProgramsMenu(Gtk_Widget(ProgramsButton)));
      Attach(InfoGrid, ProgramsButton, 1, 4);
      AddLabel(Gettext("Owner:"), 0, 5);
      AddBox(5);
      AddLabel(Gettext("Group:"), 0, 6);
      AddBox(6);
      AddLabel(Gettext("Others:"), 0, 7);
      AddBox(7);
      Show_All(InfoGrid);
      Add_Named(InfoStack, InfoGrid, "info");
      declare
         DirectoryView: constant Gtk_Tree_View :=
           Gtk_Tree_View_New_With_Model
             (+(Gtk_Tree_Model_Sort_Sort_New_With_Model
                 (+(Gtk_Tree_Model_Filter_Filter_New
                     (+(Gtk_List_Store_Newv
                         ((GType_String, GType_Uint, GType_String,
                           GType_String, GType_Uint))))))));
         Area: Gtk_Cell_Area_Box;
         Renderer: Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
         Renderer2: constant Gtk_Cell_Renderer_Pixbuf :=
           Gtk_Cell_Renderer_Pixbuf_New;
         Column: Gtk_Tree_View_Column;
         Value: GValue;
         PreviewFrame: constant Gtk_Frame := Gtk_Frame_New;
         PreviewLabel: constant Gtk_Label := Gtk_Label_New;
         PreviewBox: constant Gtk_Vbox := Gtk_Vbox_New;
      begin
         Set_Enable_Search(DirectoryView, False);
         Set_Headers_Clickable(DirectoryView, True);
         Area := Gtk_Cell_Area_Box_New;
         Pack_Start(Area, Renderer2, False);
         Add_Attribute(Area, Renderer2, "icon-name", 2);
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 0);
         Init_Set_Int(Value, 80);
         Set_Property(Renderer, "max-width-chars", Value);
         Unset(Value);
         Init_Set_Boolean(Value, True);
         Set_Property(Renderer, "ellipsize-set", Value);
         Unset(Value);
         Init_Set_Int(Value, 1);
         Set_Property(Renderer, "ellipsize", Value);
         Unset(Value);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Sort_Column_Id(Column, 0);
         Set_Title(Column, Gettext("Name"));
         Set_Resizable(Column, True);
         Set_Expand(Column, True);
         if Append_Column(DirectoryView, Column) /= 1 then
            return;
         end if;
         Area := Gtk_Cell_Area_Box_New;
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 3);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Sort_Column_Id(Column, 4);
         Set_Title(Column, Gettext("Size"));
         Set_Resizable(Column, True);
         if Append_Column(DirectoryView, Column) /= 2 then
            return;
         end if;
         On_Row_Activated(DirectoryView, SetDestination'Access);
         Scroll := Gtk_Scrolled_Window_New;
         Add(Scroll, DirectoryView);
         Add_Named(InfoStack, Scroll, "destination");
         Set_Shadow_Type(PreviewFrame, Shadow_None);
         Set_Label_Align(PreviewFrame, 0.5, 0.5);
         Set_Markup(PreviewLabel, "<b>" & Gettext("Preview") & "</b>");
         Set_Label_Widget(PreviewFrame, PreviewLabel);
         Add(PreviewFrame, InfoStack);
         Pack_Start(PreviewBox, Gtk_Flow_Box_New, False);
         Pack_Start(PreviewBox, PreviewFrame);
         Add2(FilesPaned, PreviewBox);
      end;
   end CreateShowItemsUI;

end ShowItems;
