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
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Image; use Tcl.Tk.Ada.Image;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bookmarks; use Bookmarks;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with Utils; use Utils;

package body ShowItems is

   -- ****iv* ShowItems/PreviewFrame
   -- FUNCTION
   -- Main Ttk_Frame for preview items
   -- SOURCE
   PreviewFrame: Ttk_Frame;
   -- ****

   -- ****iv* ShowItems/PreviewXScroll
   -- FUNCTION
   -- X coordinates scrollbar for previews
   -- SOURCE
   PreviewXScroll: Ttk_Scrollbar;
   -- ****

   -- ****iv* ShowItems/PreviewYScroll
   -- FUNCTION
   -- Y coordinates scrollbar for previews
   -- SOURCE
   PreviewYScroll: Ttk_Scrollbar;
   -- ****

   -- ****iv* ShowItems/PreviewTree
   -- FUNCTION
   -- Tk_Tree_View used to show directories previews
   -- SOURCE
   PreviewTree: Ttk_Tree_View;
   -- ****

   -- ****iv* ShowItems/PreviewText
   -- FUNCTION
   -- Tk_Text used to show text files previews
   -- SOURCE
   PreviewText: Tk_Text;
   -- ****

   -- ****iv* ShowItems/PreviewCanvas
   -- FUNCTION
   -- Tk_Canvas used to show images
   -- SOURCE
   PreviewCanvas: Tk_Canvas;
   -- ****

   -- ****iv* ShowItems/InfoFrame
   -- FUNCTION
   -- Ttk_Frame for show information about the selected item
   -- SOURCE
   InfoFrame: Ttk_Frame;
   -- ****

   -- ****iv* ShowItems/ButtonNames
   -- FUNCTION
   -- Names of the permissions buttons
   -- SOURCE
   ButtonNames: constant array(1 .. 3) of Unbounded_String :=
     (To_Unbounded_String("execute"), To_Unbounded_String("read"),
      To_Unbounded_String("write"));
   -- ****

   -- ****ie* ShowItems/Hunter_Show_Items_Exception
   -- FUNCTION
   -- Exception raised when any problems with showing item preview
   -- occurs
   -- SOURCE
   Hunter_Show_Items_Exception: exception;
   -- ****

   procedure ScaleImage is
      Image: constant Tk_Photo :=
        Create("previewimage", "-file " & To_String(CurrentSelected));
      TempImage: Tk_Photo := Create("tempimage");
      FrameWidth, FrameHeight, ImageWidth, ImageHeight, StartX,
      StartY: Natural;
      ScaleMode: Unbounded_String := To_Unbounded_String("-subsample");
      Scale: Natural;
   begin
      Delete(PreviewCanvas, "all");
      ImageWidth := Natural'Value(Width(Image));
      ImageHeight := Natural'Value(Height(Image));
      Copy(Image, TempImage);
      Blank(Image);
      FrameHeight := Natural'Value(Winfo_Get(PreviewFrame, "height"));
      FrameWidth := Natural'Value(Winfo_Get(PreviewFrame, "width"));
      if ImageWidth > FrameWidth or ImageHeight > FrameHeight then
         if ImageWidth / FrameWidth > ImageHeight / FrameHeight then
            Scale := ImageWidth / FrameWidth;
         else
            Scale := ImageHeight / FrameHeight;
         end if;
         Scale := Scale + 1;
      elsif FrameWidth > ImageWidth or FrameHeight > ImageHeight then
         ScaleMode := To_Unbounded_String("-zoom");
         if FrameWidth / ImageWidth > FrameHeight / ImageHeight then
            Scale := FrameWidth / ImageWidth;
         else
            Scale := FrameHeight / ImageHeight;
         end if;
      end if;
      Copy
        (TempImage, Image,
         "-shrink " & To_String(ScaleMode) & Natural'Image(Scale));
      Delete(TempImage);
      ImageWidth := Natural'Value(Width(Image));
      ImageHeight := Natural'Value(Height(Image));
      StartX := ImageWidth / 2;
      StartY := ImageHeight / 2;
      Canvas_Create
        (PreviewCanvas, "image",
         Natural'Image(StartX) & Natural'Image(StartY) & " -image " &
         Widget_Image(Image));
      configure
        (PreviewCanvas,
         "-width " & Width(Image) & " -height " & Height(Image) &
         " -scrollregion [list " & BBox(PreviewCanvas, "all") & "]");
   end ScaleImage;

   procedure ShowPreview is
      Button: Ttk_Button;
      Label: Ttk_Label;
   begin
      Label.Interp := Get_Context;
      Label.Name := New_String(Widget_Image(PreviewFrame) & ".title");
      configure(Label, "-text {Preview}");
      Button.Interp := Get_Context;
      Button.Name :=
        New_String(".mainframe.toolbars.itemtoolbar.previewbutton");
      if Winfo_Get(Button, "ismapped") = "0" then
         Tcl.Tk.Ada.Pack.Pack
           (Button, "-before .mainframe.toolbars.itemtoolbar.infobutton");
      end if;
      Button.Name := New_String(".mainframe.toolbars.itemtoolbar.runbutton");
      if Is_Directory(To_String(CurrentSelected)) then
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         if not Is_Read_Accessible_File(To_String(CurrentSelected)) then
            ShowMessage
              ("You don't have permissions to preview this directory.");
         end if;
         LoadDirectory(To_String(CurrentSelected), True);
         Tcl.Tk.Ada.Pack.Pack_Forget(PreviewText);
         Tcl.Tk.Ada.Pack.Pack_Forget(PreviewCanvas);
         Tcl.Tk.Ada.Pack.Pack_Forget(InfoFrame);
         configure
           (PreviewYScroll,
            "-command [list " & Widget_Image(PreviewFrame) &
            ".directorytree yview]");
         configure
           (PreviewXScroll,
            "-command [list " & Widget_Image(PreviewFrame) &
            ".directorytree xview]");
         configure(PreviewTree, "-selectmode none");
         Tcl.Tk.Ada.Pack.Pack(PreviewXScroll, "-side bottom -fill x");
         Tcl.Tk.Ada.Pack.Pack(PreviewYScroll, "-side right -fill y");
         Tcl.Tk.Ada.Pack.Pack
           (PreviewTree, "-side top -fill both -expand true");
         UpdateDirectoryList(True, "preview");
      else
         if Is_Executable_File(To_String(CurrentSelected)) then
            if Winfo_Get(Button, "ismapped") = "0" then
               Tcl.Tk.Ada.Pack.Pack
                 (Button,
                  "-before .mainframe.toolbars.itemtoolbar.openwithbutton");
            end if;
         else
            Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         end if;
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
         begin
            Button.Name :=
              New_String(".mainframe.toolbars.itemtoolbar.openbutton");
            if CanBeOpened(MimeType) then
               if Winfo_Get(Button, "ismapped") = "0" then
                  Tcl.Tk.Ada.Pack.Pack
                    (Button,
                     "-before .mainframe.toolbars.itemtoolbar.openwithbutton");
               end if;
            else
               Tcl.Tk.Ada.Pack.Pack_Forget(Button);
            end if;
            if MimeType(1 .. 4) = "text" then
               declare
                  ExecutableName: constant String :=
                    FindExecutable("highlight", False);
                  Success, FirstLine: Boolean;
                  File: File_Type;
                  FileLine, TagText, TagName: Unbounded_String;
                  StartIndex, EndIndex, StartColor: Natural;
                  procedure LoadFile is
                  begin
                     Open(File, In_File, To_String(CurrentSelected));
                     while not End_Of_File(File) loop
                        FileLine := To_Unbounded_String(Get_Line(File));
                        StartIndex := 1;
                        loop
                           StartIndex := Index(FileLine, "{", StartIndex);
                           exit when StartIndex = 0;
                           Replace_Slice
                             (FileLine, StartIndex, StartIndex, "\{");
                           StartIndex := StartIndex + 2;
                        end loop;
                        StartIndex := 1;
                        loop
                           StartIndex := Index(FileLine, "}", StartIndex);
                           exit when StartIndex = 0;
                           Replace_Slice
                             (FileLine, StartIndex, StartIndex, "\}");
                           StartIndex := StartIndex + 2;
                        end loop;
                        Insert
                          (PreviewText, "end",
                           "[subst -nocommands -novariables {" &
                           To_String(FileLine) & LF & "}]");
                     end loop;
                     Close(File);
                  end LoadFile;
               begin
                  Tcl.Tk.Ada.Pack.Pack_Forget(PreviewTree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(PreviewCanvas);
                  Tcl.Tk.Ada.Pack.Pack_Forget(PreviewXScroll);
                  Tcl.Tk.Ada.Pack.Pack_Forget(InfoFrame);
                  configure
                    (PreviewYScroll,
                     "-command [list " & Widget_Image(PreviewText) &
                     " yview]");
                  Tcl.Tk.Ada.Pack.Pack(PreviewYScroll, "-side right -fill y");
                  Tcl.Tk.Ada.Pack.Pack
                    (PreviewText, "-side top -fill both -expand true");
                  configure(PreviewText, "-state normal");
                  Delete(PreviewText, "1.0", "end");
                  if not Settings.ColorText or ExecutableName = "" then
                     LoadFile;
                     goto Set_UI;
                  end if;
                  Spawn
                    (ExecutableName,
                     Argument_String_To_List
                       ("--out-format=pango --force --output=" &
                        Value("HOME") &
                        "/.cache/hunter/highlight.tmp --base16 --style=" &
                        To_String(Settings.ColorTheme) & " " &
                        To_String(CurrentSelected)).all,
                     Success);
                  if not Success then
                     LoadFile;
                     goto Set_UI;
                  end if;
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
                        StartIndex := Index(FileLine, "\", StartIndex);
                        exit when StartIndex = 0;
                        Replace_Slice(FileLine, StartIndex, StartIndex, "\\");
                        StartIndex := StartIndex + 2;
                     end loop;
                     StartIndex := 1;
                     loop
                        StartIndex := Index(FileLine, "{", StartIndex);
                        exit when StartIndex = 0;
                        Replace_Slice(FileLine, StartIndex, StartIndex, "\{");
                        StartIndex := StartIndex + 2;
                     end loop;
                     StartIndex := 1;
                     loop
                        StartIndex := Index(FileLine, "}", StartIndex);
                        exit when StartIndex = 0;
                        Replace_Slice(FileLine, StartIndex, StartIndex, "\}");
                        StartIndex := StartIndex + 2;
                     end loop;
                     StartIndex := 1;
                     loop
                        StartIndex := Index(FileLine, "<span", StartIndex);
                        exit when StartIndex = 0;
                        if StartIndex > 1 then
                           Insert
                             (PreviewText, "end",
                              "[subst -nocommands -novariables {" &
                              Slice(FileLine, 1, StartIndex - 1) & "}]");
                        end if;
                        EndIndex := Index(FileLine, ">", StartIndex);
                        TagText :=
                          Unbounded_Slice(FileLine, StartIndex, EndIndex);
                        StartColor := Index(TagText, "foreground=");
                        if Index(TagText, "foreground=") > 0 then
                           TagName :=
                             Unbounded_Slice
                               (TagText, StartColor + 12, StartColor + 18);
                           Tag_Configure
                             (PreviewText, To_String(TagName),
                              "-foreground " & To_String(TagName));
                        elsif Index(TagText, "style=""italic""") > 0 then
                           TagName := To_Unbounded_String("italictag");
                        elsif Index(TagText, "weight=""bold""") > 0 then
                           TagName := To_Unbounded_String("boldtag");
                        end if;
                        StartIndex := StartIndex + Length(TagText);
                        EndIndex := Index(FileLine, "</span>", StartIndex) - 1;
                        if EndIndex > 0 then
                           Insert
                             (PreviewText, "end",
                              "[subst -nocommands -novariables {" &
                              Slice(FileLine, StartIndex, EndIndex) &
                              "}] [list " & To_String(TagName) & "]");
                        else
                           Insert
                             (PreviewText, "end",
                              "[subst -nocommands -novariables {" &
                              Slice(FileLine, StartIndex, Length(FileLine)) &
                              "}]");
                        end if;
                        StartIndex := 1;
                        FileLine :=
                          Unbounded_Slice
                            (FileLine, EndIndex + 8, Length(FileLine));
                     end loop;
                     Insert
                       (PreviewText, "end",
                        "[subst -nocommands -novariables {" &
                        To_String(FileLine) & LF & "}]");
                  end loop;
                  Close(File);
                  Delete_File(Value("HOME") & "/.cache/hunter/highlight.tmp");
                  <<Set_UI>>
                  configure(PreviewText, "-state disabled");
               end;
            elsif MimeType(1 .. 5) = "image" then
               declare
                  Image: constant Tk_Photo :=
                    Create
                      ("previewimage", "-file " & To_String(CurrentSelected));
                  StartX, StartY, ImageWidth, ImageHeight: Natural;
               begin
                  Tcl.Tk.Ada.Pack.Pack_Forget(PreviewText);
                  Tcl.Tk.Ada.Pack.Pack_Forget(PreviewTree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(InfoFrame);
                  if Settings.ScaleImages then
                     Tcl.Tk.Ada.Pack.Pack_Forget(PreviewYScroll);
                     Tcl.Tk.Ada.Pack.Pack_Forget(PreviewXScroll);
                     ScaleImage;
                  else
                     Delete(PreviewCanvas, "all");
                     ImageWidth := Natural'Value(Width(Image));
                     ImageHeight := Natural'Value(Height(Image));
                     StartX := ImageWidth / 2;
                     StartY := ImageHeight / 2;
                     Canvas_Create
                       (PreviewCanvas, "image",
                        Natural'Image(StartX) & Natural'Image(StartY) &
                        " -image " & Widget_Image(Image));
                     configure
                       (PreviewCanvas,
                        "-width " & Width(Image) & " -height " &
                        Height(Image) & " -scrollregion [list " &
                        BBox(PreviewCanvas, "all") & "]");
                     configure
                       (PreviewYScroll,
                        "-command [list " & Widget_Image(PreviewCanvas) &
                        " yview]");
                     configure
                       (PreviewXScroll,
                        "-command [list " & Widget_Image(PreviewCanvas) &
                        " xview]");
                     Tcl.Tk.Ada.Pack.Pack
                       (PreviewXScroll, "-side bottom -fill x");
                     Tcl.Tk.Ada.Pack.Pack
                       (PreviewYScroll, "-side right -fill y");
                  end if;
                  Tcl.Tk.Ada.Pack.Pack(PreviewCanvas, "-side top");
               end;
            else
               declare
                  ActionButton: Ttk_RadioButton;
               begin
                  Button.Name :=
                    New_String
                      (".mainframe.toolbars.itemtoolbar.previewbutton");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Button);
                  ActionButton.Name :=
                    New_String(".mainframe.toolbars.itemtoolbar.infobutton");
                  ActionButton.Interp := Get_Context;
                  if Invoke(ActionButton) /= "" then
                     raise Hunter_Show_Items_Exception
                       with "Can't show file or directory info";
                  end if;
               end;
            end if;
         end;
      end if;
   end ShowPreview;

   -- ****if* ShowItems/ShowInfo
   -- FUNCTION
   -- Show information about the currently selected file or directory.
   -- SOURCE
   procedure ShowInfo is
      -- ****
      Label: Ttk_Label;
      SelectedItem: constant String := To_String(CurrentSelected);
      Button: Ttk_Button;
   begin
      Tcl.Tk.Ada.Pack.Pack_Forget(PreviewText);
      Tcl.Tk.Ada.Pack.Pack_Forget(PreviewTree);
      Tcl.Tk.Ada.Pack.Pack_Forget(PreviewCanvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(PreviewYScroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(PreviewXScroll);
      Label.Interp := Get_Context;
      Label.Name := New_String(Widget_Image(PreviewFrame) & ".title");
      configure(Label, "-text {Information}");
      Button.Interp := Label.Interp;
      Label.Name := New_String(Widget_Image(InfoFrame) & ".fullpathtext");
      if not Is_Symbolic_Link(SelectedItem) then
         configure(Label, "-text {Full path:}");
      else
         configure(Label, "-text {Links to:}");
      end if;
      Label.Name := New_String(Widget_Image(InfoFrame) & ".fullpath");
      configure(Label, "-text {" & Full_Name(SelectedItem) & "}");
      Label.Name := New_String(Widget_Image(InfoFrame) & ".sizetext");
      if Is_Directory(SelectedItem) then
         configure(Label, "-text {Elements:}");
      else
         configure(Label, "-text {Size:}");
      end if;
      Label.Name := New_String(Widget_Image(InfoFrame) & ".size");
      if Is_Directory(SelectedItem) then
         configure
           (Label,
            "-text {" & Natural'Image(Natural(SecondItemsList.Length)) & "}");
      elsif Is_Regular_File(SelectedItem) then
         configure(Label, "-text {" & CountFileSize(Size(SelectedItem)) & "}");
      else
         configure(Label, "-text {Unknown}");
      end if;
      Label.Name := New_String(Widget_Image(InfoFrame) & ".lastmodified");
      if Is_Directory(SelectedItem) or Is_Regular_File(SelectedItem) then
         configure
           (Label,
            "-text {" &
            Ada.Calendar.Formatting.Image
              (Modification_Time(SelectedItem), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset) &
            "}");
      else
         configure(Label, "-text {Unknown}");
      end if;
      Label.Name := New_String(Widget_Image(InfoFrame) & ".filetypetext");
      if Is_Directory(SelectedItem) or not Is_Regular_File(SelectedItem) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Label.Name := New_String(Widget_Image(InfoFrame) & ".filetype");
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         Tcl.Tk.Ada.Grid.Grid(Label);
         Label.Name := New_String(Widget_Image(InfoFrame) & ".filetype");
         configure
           (Label, "-text {" & GetMimeType(Full_Name(SelectedItem)) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name :=
        New_String(Widget_Image(InfoFrame) & ".associatedprogramtext");
      if not Is_Regular_File(SelectedItem) and
        not Is_Directory(SelectedItem) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Button.Name :=
           New_String(Widget_Image(InfoFrame) & ".associatedprogram");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      else
         Tcl.Tk.Ada.Grid.Grid(Label);
         Button.Name :=
           New_String(Widget_Image(InfoFrame) & ".associatedprogram");
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
                  GetMimeType(Full_Name(SelectedItem))).all);
            Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
            if Result = 1 then
               DesktopFile :=
                 To_Unbounded_String(Expect_Out_Match(ProcessDesc));
               DesktopFile :=
                 To_Unbounded_String(GetProgramName(To_String(DesktopFile)));
               if Index(DesktopFile, ".desktop") = 0 then
                  configure(Button, "-text {" & To_String(DesktopFile) & "}");
               else
                  configure
                    (Button,
                     "-text {" & To_String(DesktopFile) & " (not installed)}");
               end if;
            end if;
            Close(ProcessDesc);
         exception
            when Process_Died =>
               configure(Button, "-text {None}");
         end;
         Tcl.Tk.Ada.Grid.Grid(Button);
      end if;
      declare
         Attributes: Unbounded_String;
         Tokens: Slice_Set;
         procedure SetPermissionsButtons
           (Name, ButtonState: String; Permission: Character) is
            CheckButton: Ttk_CheckButton;
         begin
            CheckButton.Interp := Get_Context;
            for I in ButtonNames'Range loop
               CheckButton.Name :=
                 New_String
                   (Widget_Image(InfoFrame) & "." & Name & "frame." &
                    To_String(ButtonNames(I)));
               if I = 1 then
                  if Is_Directory(SelectedItem) then
                     Tcl.Tk.Ada.Pack.Pack_Forget(CheckButton);
                  else
                     Tcl.Tk.Ada.Pack.Pack
                       (CheckButton,
                        "-before " & Widget_Image(InfoFrame) & "." & Name &
                        "frame.read");
                  end if;
               end if;
               State(CheckButton, ButtonState);
            end loop;
            Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "execute", "0");
            Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "read", "0");
            Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "write", "0");
            case Permission is
               when '1' =>
                  Tcl.Ada.Tcl_SetVar
                    (CheckButton.Interp, Name & "execute", "1");
               when '2' =>
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "write", "1");
               when '3' =>
                  Tcl.Ada.Tcl_SetVar
                    (CheckButton.Interp, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "write", "1");
               when '4' =>
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "read", "1");
               when '5' =>
                  Tcl.Ada.Tcl_SetVar
                    (CheckButton.Interp, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "read", "1");
               when '6' =>
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "read", "1");
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "write", "1");
               when '7' =>
                  Tcl.Ada.Tcl_SetVar
                    (CheckButton.Interp, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "read", "1");
                  Tcl.Ada.Tcl_SetVar(CheckButton.Interp, Name & "write", "1");
               when others =>
                  null;
            end case;
         end SetPermissionsButtons;
      begin
         Tcl.Ada.Tcl_Eval
           (Label.Interp, "file attributes {" & SelectedItem & "}");
         Attributes :=
           To_Unbounded_String(Tcl.Ada.Tcl_GetResult(Label.Interp));
         Create(Tokens, To_String(Attributes), " ");
         Label.Name :=
           New_String(Widget_Image(InfoFrame) & ".groupframe.group");
         configure(Label, "-text {" & Slice(Tokens, 2) & "}");
         Label.Name :=
           New_String(Widget_Image(InfoFrame) & ".ownerframe.owner");
         configure(Label, "-text {" & Slice(Tokens, 4) & "}");
         if Value("USER") /= Slice(Tokens, 4) then
            SetPermissionsButtons
              ("owner", "disabled",
               Slice(Tokens, 6)(Slice(Tokens, 6)'Last - 2));
            SetPermissionsButtons
              ("group", "disabled",
               Slice(Tokens, 6)(Slice(Tokens, 6)'Last - 1));
            SetPermissionsButtons
              ("others", "disabled", Slice(Tokens, 6)(Slice(Tokens, 6)'Last));
         else
            SetPermissionsButtons
              ("owner", "!disabled",
               Slice(Tokens, 6)(Slice(Tokens, 6)'Last - 2));
            SetPermissionsButtons
              ("group", "!disabled",
               Slice(Tokens, 6)(Slice(Tokens, 6)'Last - 1));
            SetPermissionsButtons
              ("others", "!disabled", Slice(Tokens, 6)(Slice(Tokens, 6)'Last));
         end if;
      end;
      Tcl.Tk.Ada.Pack.Pack(InfoFrame);
   end ShowInfo;

   function Show_Preview_Or_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* ShowItems/Show_Preview_Or_Info_Command
      -- FUNCTION
      -- Show preview or information about the currently selected file or
      -- directory, depends which button was clicked
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Show_Preview_Or_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
   begin
      if Tcl.Ada.Tcl_GetVar(Interp, "previewtype") = "preview" then
         ShowPreview;
      else
         ShowInfo;
      end if;
      return TCL_OK;
   end Show_Preview_Or_Info_Command;

   function Show_Selected_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      DirectoryTree: Ttk_Tree_View;
      Tokens: Slice_Set;
      Items: Unbounded_String;
      ActionButton: Ttk_RadioButton;
   begin
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(".mainframe.paned.directoryframe.directorytree");
      SelectedItems.Clear;
      Items := To_Unbounded_String(Selection(DirectoryTree));
      if Items /= Null_Unbounded_String then
         Create(Tokens, To_String(Items), " ");
         for I in 1 .. Slice_Count(Tokens) loop
            SelectedItems.Append
              (CurrentDirectory & "/" &
               ItemsList(Positive'Value(Slice(Tokens, I))).Name);
         end loop;
      else
         SelectedItems.Append(CurrentDirectory);
      end if;
      if not Settings.ShowPreview or
        (SelectedItems(1) = CurrentSelected and
         CurrentSelected /= CurrentDirectory) then
         return TCL_OK;
      end if;
      CurrentSelected := SelectedItems(1);
      if NewAction = CREATELINK then
         return TCL_OK;
      end if;
      ActionButton.Interp := Get_Context;
      if Is_Directory(To_String(CurrentSelected)) or
        Is_Regular_File(To_String(CurrentSelected)) then
         ActionButton.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.previewbutton");
      else
         ActionButton.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.infobutton");
      end if;
      if Invoke(ActionButton) /= "" and Invoke(ActionButton) /= "0" then
         raise Hunter_Show_Items_Exception
           with "Can't show file or directory preview/info";
      end if;
      SetBookmarkButton;
      return TCL_OK;
   end Show_Selected_Command;

   function Set_Permissions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* ShowItems/Set_Permissions_Command
      -- FUNCTION
      -- Set the permissions for the selected file or directory
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Set_Permissions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      -- ****
      SelectedItem: constant String := Full_Name(To_String(CurrentSelected));
      PermissionsString: Unbounded_String;
      Permission: Natural;
      Names: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("owner"), To_Unbounded_String("group"),
         To_Unbounded_String("others"));
   begin
      if Is_Directory(SelectedItem) then
         PermissionsString := To_Unbounded_String("040");
      else
         PermissionsString := To_Unbounded_String("00");
      end if;
      for Name of Names loop
         Permission := 0;
         if Tcl.Ada.Tcl_GetVar(Interp, To_String(Name) & "execute") = "1" then
            Permission := Permission + 1;
         end if;
         if Tcl.Ada.Tcl_GetVar(Interp, To_String(Name) & "write") = "1" then
            Permission := Permission + 2;
         end if;
         if Tcl.Ada.Tcl_GetVar(Interp, To_String(Name) & "read") = "1" then
            Permission := Permission + 4;
         end if;
         Append(PermissionsString, Trim(Natural'Image(Permission), Both));
      end loop;
      Tcl.Ada.Tcl_Eval
        (Interp,
         "file attributes {" & SelectedItem & "} -permissions " &
         To_String(PermissionsString));
      return TCL_OK;
   end Set_Permissions_Command;

   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* Commands/GoToDirectory_Command
      -- FUNCTION
      -- Go to the selected directory in preview
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed. Unused
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command. Unused
      -- SOURCE
   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      -- ****
      SelectedItem: Unbounded_String;
   begin
      if Selection(PreviewTree) = "" then
         return TCL_OK;
      end if;
      SelectedItem :=
        DestinationDirectory & "/" &
        To_Unbounded_String(Set(PreviewTree, Selection(PreviewTree), "name"));
      if not Is_Directory(To_String(SelectedItem)) then
         return TCL_OK;
      end if;
      DestinationDirectory := SelectedItem;
      LoadDirectory(To_String(SelectedItem), True);
      UpdateDirectoryList(True, "preview");
      return TCL_OK;
   end GoToDirectory_Command;

   procedure CreateShowItemsUI is
      Paned: Ttk_PanedWindow;
      Label: Ttk_Label;
      Button: Ttk_Button;
      ButtonTexts: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("Can execute"), To_Unbounded_String("Can read"),
         To_Unbounded_String("Can write"));
      PathButtonsFrame: Ttk_Frame;
      pragma Unreferenced(PathButtonsFrame);
      procedure CreatePermissionsFrame(Name, Text: String; Row: Positive) is
         Frame: constant Ttk_Frame :=
           Create(".mainframe.paned.previewframe.infoframe." & Name & "frame");
         CheckButton: Ttk_CheckButton;
      begin
         Label :=
           Create
             (".mainframe.paned.previewframe.infoframe." & Name & "text",
              "-text {" & Text & ":}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row" & Positive'Image(Row));
         Label := Create(Widget_Image(Frame) & "." & Name);
         Tcl.Tk.Ada.Pack.Pack(Label);
         for I in ButtonNames'Range loop
            CheckButton :=
              Create
                (Widget_Image(Frame) & "." & To_String(ButtonNames(I)),
                 "-text {" & To_String(ButtonTexts(I)) & "} -variable " &
                 Name & To_String(ButtonNames(I)) &
                 " -command SetPermissions");
            Tcl.Tk.Ada.Pack.Pack(CheckButton);
         end loop;
         Tcl.Tk.Ada.Grid.Grid(Frame, "-column 1 -row" & Positive'Image(Row));
      end CreatePermissionsFrame;
   begin
      PreviewFrame := Create(".mainframe.paned.previewframe");
      Label := Create(Widget_Image(PreviewFrame) & ".title");
      Tcl.Tk.Ada.Pack.Pack(Label);
      PathButtonsFrame := Create(Widget_Image(PreviewFrame) & ".pathframe");
      Paned.Interp := PreviewFrame.Interp;
      Paned.Name := New_String(".mainframe.paned");
      PreviewXScroll :=
        Create
          (Widget_Image(PreviewFrame) & ".scrollx",
           "-orient horizontal -command [list " & Widget_Image(PreviewFrame) &
           ".directorytree xview]");
      PreviewYScroll :=
        Create
          (Widget_Image(PreviewFrame) & ".scrolly",
           "-orient vertical -command [list " & Widget_Image(PreviewFrame) &
           ".directorytree yview]");
      PreviewTree :=
        Create
          (Widget_Image(PreviewFrame) & ".directorytree",
           "-columns [list name] -xscrollcommand {" &
           Widget_Image(PreviewXScroll) & " set} -yscrollcommand {" &
           Widget_Image(PreviewYScroll) & " set} -selectmode none ");
      Heading
        (PreviewTree, "name",
         "-text {Name} -image {arrow-down} -command {Sort previewname}");
      Column(PreviewTree, "#0", "-stretch false -width 50");
      Bind(PreviewTree, "<Double-1>", "GoToDirectory");
      Bind(PreviewTree, "<Return>", "GoToDirectory");
      PreviewText :=
        Create
          (Widget_Image(PreviewFrame) & ".previewtext",
           "-wrap char -yscrollcommand """ & Widget_Image(PreviewYScroll) &
           " set""");
      Tag_Configure(PreviewText, "boldtag", "-font bold");
      Tag_Configure(PreviewText, "italictag", "-font italic");
      PreviewCanvas :=
        Create
          (Widget_Image(PreviewFrame) & ".previewcanvas",
           "-xscrollcommand """ & Widget_Image(PreviewXScroll) &
           " set"" -yscrollcommand """ & Widget_Image(PreviewYScroll) &
           " set""");
      InfoFrame := Create(Widget_Image(PreviewFrame) & ".infoframe");
      Label := Create(Widget_Image(InfoFrame) & ".fullpathtext");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label :=
        Create
          (Widget_Image(InfoFrame) & ".fullpath",
           "-wraplength " &
           Natural'Image(Natural'Value(Winfo_Get(Paned, "width")) / 3));
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      Label := Create(Widget_Image(InfoFrame) & ".sizetext");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 1");
      Label := Create(Widget_Image(InfoFrame) & ".size");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 1");
      Label :=
        Create
          (Widget_Image(InfoFrame) & ".lastmodifiedtext",
           "-text {Last modified:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 2");
      Label := Create(Widget_Image(InfoFrame) & ".lastmodified");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2");
      Label :=
        Create
          (Widget_Image(InfoFrame) & ".filetypetext", "-text {File type:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 3");
      Label := Create(Widget_Image(InfoFrame) & ".filetype");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 3");
      Label :=
        Create
          (Widget_Image(InfoFrame) & ".associatedprogramtext",
           "-text {Associated program:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 4");
      Button :=
        Create
          (Widget_Image(InfoFrame) & ".associatedprogram",
           "-command ToggleApplicationsMenu");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 4");
      CreatePermissionsFrame("owner", "Owner", 5);
      CreatePermissionsFrame("group", "Group", 6);
      CreatePermissionsFrame("others", "Others", 7);
      AddCommand("ShowSelected", Show_Selected_Command'Access);
      AddCommand("ShowPreviewOrInfo", Show_Preview_Or_Info_Command'Access);
      AddCommand("SetPermissions", Set_Permissions_Command'Access);
      AddCommand("GoToDirectory", GoToDirectory_Command'Access);
      Add
        (Button,
         "Select new associated program with that type of file or directory.");
      if Settings.ShowPreview then
         Add(Paned, PreviewFrame, "-weight 20");
      end if;
      CreateProgramsMenu;
   end CreateShowItemsUI;

end ShowItems;
