-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
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
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bookmarks.UI; use Bookmarks.UI;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages.UI; use Messages.UI;
with Modules; use Modules;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with ProgramsMenu.UI; use ProgramsMenu.UI;
with Toolbars; use Toolbars;
with Utils; use Utils;

package body ShowItems is

   -- ****iv* ShowItems/ShowItems.Preview_Frame
   -- FUNCTION
   -- Main Ttk_Frame for preview items
   -- SOURCE
   Preview_Frame: Ttk_Frame;
   -- ****

   -- ****if* ShowItems/ShowItems.Get_Preview_Frame
   -- FUNCTION
   -- Get the main Ttk_Frame for preview items
   -- RESULT
   -- The main Ttk_Frame for preview items
   -- SOURCE
   function Get_Preview_Frame return Ttk_Frame is
      -- ****
   begin
      return Preview_Frame;
   end Get_Preview_Frame;

   -- ****iv* ShowItems/ShowItems.Preview_X_Scroll
   -- FUNCTION
   -- X coordinates scrollbar for previews
   -- SOURCE
   Preview_X_Scroll: Ttk_Scrollbar;
   -- ****

   -- ****iv* ShowItems/ShowItems.Preview_Y_Scroll
   -- FUNCTION
   -- Y coordinates scrollbar for previews
   -- SOURCE
   Preview_Y_Scroll: Ttk_Scrollbar;
   -- ****

   -- ****iv* ShowItems/ShowItems.Preview_Tree
   -- FUNCTION
   -- Tk_Tree_View used to show directories previews
   -- SOURCE
   Preview_Tree: Ttk_Tree_View;
   -- ****

   -- ****iv* ShowItems/ShowItems.Preview_Text
   -- FUNCTION
   -- Tk_Text used to show text files previews
   -- SOURCE
   Preview_Text: Tk_Text;
   -- ****

   -- ****if* ShowItems/ShowItems.Get_Preview_Text
   -- FUNCTION
   -- Get the main Tk_Text for preview text files
   -- RESULT
   -- The main Tk_Text for preview text files
   -- SOURCE
   function Get_Preview_Text return Tk_Text is
      -- ****
   begin
      return Preview_Text;
   end Get_Preview_Text;

   -- ****iv* ShowItems/ShowItems.Preview_Canvas
   -- FUNCTION
   -- Tk_Canvas used to show images
   -- SOURCE
   Preview_Canvas: Tk_Canvas;
   -- ****

   -- ****if* ShowItems/ShowItems.Get_Preview_Canvas
   -- FUNCTION
   -- Get the Tk_Canvas for preview items
   -- RESULT
   -- The Tk_Canvas for preview items
   -- SOURCE
   function Get_Preview_Canvas return Tk_Canvas is
      -- ****
   begin
      return Preview_Canvas;
   end Get_Preview_Canvas;

   -- ****iv* ShowItems/ShowItems.Info_Frame
   -- FUNCTION
   -- Ttk_Frame for show information about the selected item
   -- SOURCE
   Info_Frame: Ttk_Frame;
   -- ****

   -- ****iv* ShowItems/ShowItems.Button_Names
   -- FUNCTION
   -- Names of the permissions buttons
   -- SOURCE
   Button_Names: constant array(1 .. 3) of Unbounded_String :=
     (1 => To_Unbounded_String(Source => "execute"),
      2 => To_Unbounded_String(Source => "read"),
      3 => To_Unbounded_String(Source => "write"));
   -- ****

   procedure Scale_Image is
      Image_To_Scale: constant Tk_Photo :=
        Create
          (pathName => "previewimage",
           options => "-file " & To_String(Source => Current_Selected));
      Temp_Image: Tk_Photo := Create(pathName => "tempimage");
      Frame_Width, Frame_Height, Image_Width, Image_Height, Start_X,
      Start_Y: Natural;
      Scale_Mode: Unbounded_String :=
        To_Unbounded_String(Source => "-subsample");
      Scale: Natural := 0;
   begin
      Delete(CanvasWidget => Get_Preview_Canvas, TagOrId => "all");
      Image_Width := Natural'Value(Width(Img => Image_To_Scale));
      Image_Height := Natural'Value(Height(Img => Image_To_Scale));
      Copy(Source => Image_To_Scale, Target => Temp_Image);
      Blank(Image => Image_To_Scale);
      Frame_Height :=
        Natural'Value(Winfo_Get(Widgt => Get_Preview_Frame, Info => "height"));
      Frame_Width :=
        Natural'Value(Winfo_Get(Widgt => Get_Preview_Frame, Info => "width"));
      if Image_Width > Frame_Width or Image_Height > Frame_Height then
         Scale :=
           (if Image_Width / Frame_Width > Image_Height / Frame_Height then
              Image_Width / Frame_Width
            else Image_Height / Frame_Height) +
           1;
      elsif Frame_Width > Image_Width or Frame_Height > Image_Height then
         Scale_Mode := To_Unbounded_String(Source => "-zoom");
         Scale :=
           (if Frame_Width / Image_Width > Frame_Height / Image_Height then
              Frame_Width / Image_Width
            else Frame_Height / Image_Height);
      end if;
      Copy
        (Source => Temp_Image, Target => Image_To_Scale,
         Options =>
           "-shrink " & To_String(Source => Scale_Mode) &
           Natural'Image(Scale));
      Delete(Img => Temp_Image);
      Image_Width := Natural'Value(Width(Img => Image_To_Scale));
      Image_Height := Natural'Value(Height(Img => Image_To_Scale));
      if Image_Height < Frame_Height then
         Image_Height := Frame_Height;
      end if;
      Start_X := Image_Width / 2;
      Start_Y := Image_Height / 2;
      Canvas_Create
        (Parent => Get_Preview_Canvas, Child_Type => "image",
         Options =>
           Natural'Image(Start_X) & Natural'Image(Start_Y) & " -image " &
           Image_To_Scale);
      configure
        (Widgt => Get_Preview_Canvas,
         options =>
           "-width " & Width(Img => Image_To_Scale) & " -height" &
           Natural'Image(Image_Height));
   end Scale_Image;

   procedure Show_Preview is
      Button: Ttk_Button :=
        Get_Widget
          (pathName => ".mainframe.toolbars.itemtoolbar.previewbutton");
      Label: constant Ttk_Label :=
        Get_Widget(pathName => Get_Preview_Frame & ".title");
      Path_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.paned.previewframe.pathframe");
      Hunter_Show_Items_Exception: exception;
   begin
      configure
        (Widgt => Label,
         options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "{Preview}") &
           "}");
      if Winfo_Get(Widgt => Button, Info => "ismapped") = "0" then
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Button,
            Options =>
              "-before .mainframe.toolbars.itemtoolbar.infobutton -side " &
              (if Settings.Toolbars_On_Top then "left" else "top"));
      end if;
      SetActionsButtons;
      Unautoscroll(Scroll => Preview_X_Scroll);
      Set(ScrollbarWidget => Preview_X_Scroll, First => "0.0", Last => "1.0");
      Unautoscroll(Scroll => Preview_Y_Scroll);
      Set(ScrollbarWidget => Preview_Y_Scroll, First => "0.0", Last => "1.0");
      if Is_Directory(Name => To_String(Source => Current_Selected)) then
         if not Is_Read_Accessible_File
             (Name => To_String(Source => Current_Selected)) then
            Show_Message
              (Message =>
                 Mc
                   (Interp => Get_Context,
                    Src_String =>
                      "{You don't have permissions to preview this directory.}"));
         end if;
         Load_Directory
           (Directory_Name => To_String(Source => Current_Selected),
            Second => True);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Preview_Tree);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Preview_X_Scroll);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Preview_Y_Scroll);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Info_Frame);
         configure
           (Widgt => Preview_Y_Scroll,
            options => "-command [list " & Preview_Tree & " yview]");
         configure
           (Widgt => Preview_X_Scroll,
            options => "-command [list " & Preview_Tree & " xview]");
         configure(Widgt => Preview_Tree, options => "-selectmode none");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Preview_X_Scroll, Options => "-side bottom -fill x");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Preview_Y_Scroll, Options => "-side right -fill y");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Preview_Tree,
            Options => "-side top -fill both -expand true");
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Path_Frame);
         Tcl_Eval(interp => Get_Context, strng => "update");
         Update_Directory_List(Clear => True, Frame_Name => "preview");
         Autoscroll(Scroll => Preview_X_Scroll);
         Autoscroll(Scroll => Preview_Y_Scroll);
      else
         Show_Preview_Block :
         declare
            Mime_Type: constant String :=
              Get_Mime_Type
                (File_Name => To_String(Source => Current_Selected));
         begin
            if Is_Text(Mime_Type => Mime_Type) then
               Show_Text_Preview_Block :
               declare
                  Executable_Name: constant String :=
                    Find_Executable
                      (Name => "highlight", Display_Message => False);
                  Success, First_Line: Boolean := True;
                  File: File_Type;
                  File_Line, Tag_Text, Tag_Name: Unbounded_String :=
                    Null_Unbounded_String;
                  Start_Index, End_Index, Start_Color: Natural := 0;
                  procedure Load_File is
                  begin
                     Open
                       (File => File, Mode => In_File,
                        Name => To_String(Source => Current_Selected));
                     Load_Simple_File_Loop :
                     while not End_Of_File(File => File) loop
                        File_Line :=
                          To_Unbounded_String
                            (Source => Get_Line(File => File));
                        Start_Index := 1;
                        Escape_Entry_Braces_Loop :
                        loop
                           Start_Index :=
                             Index
                               (Source => File_Line, Pattern => "{",
                                From => Start_Index);
                           exit Escape_Entry_Braces_Loop when Start_Index = 0;
                           Replace_Slice
                             (Source => File_Line, Low => Start_Index,
                              High => Start_Index, By => "\{");
                           Start_Index := Start_Index + 2;
                        end loop Escape_Entry_Braces_Loop;
                        Start_Index := 1;
                        Escape_Closing_Braces_Loop :
                        loop
                           Start_Index :=
                             Index
                               (Source => File_Line, Pattern => "}",
                                From => Start_Index);
                           exit Escape_Closing_Braces_Loop when Start_Index =
                             0;
                           Replace_Slice
                             (Source => File_Line, Low => Start_Index,
                              High => Start_Index, By => "\}");
                           Start_Index := Start_Index + 2;
                        end loop Escape_Closing_Braces_Loop;
                        Insert
                          (TextWidget => Get_Preview_Text, Index => "end",
                           Text =>
                             "[subst -nocommands -novariables {" &
                             To_String(Source => File_Line) & LF & "}]");
                     end loop Load_Simple_File_Loop;
                     Close(File => File);
                  end Load_File;
                  procedure Replace_Element(Element, New_Element: String) is
                  begin
                     Replace_Element_Loop :
                     loop
                        Start_Index :=
                          Index(Source => File_Line, Pattern => Element);
                        exit Replace_Element_Loop when Start_Index = 0;
                        Replace_Slice
                          (Source => File_Line, Low => Start_Index,
                           High => Start_Index + Element'Length - 1,
                           By => New_Element);
                     end loop Replace_Element_Loop;
                  end Replace_Element;
                  procedure Escape_Element(Element: String) is
                  begin
                     Start_Index := 1;
                     Escape_Element_Loop :
                     loop
                        Start_Index :=
                          Index
                            (Source => File_Line, Pattern => Element,
                             From => Start_Index);
                        exit Escape_Element_Loop when Start_Index = 0;
                        Replace_Slice
                          (Source => File_Line, Low => Start_Index,
                           High => Start_Index, By => "\" & Element);
                        Start_Index := Start_Index + 2;
                     end loop Escape_Element_Loop;
                  end Escape_Element;
               begin
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Preview_Tree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Preview_X_Scroll);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Info_Frame);
                  configure
                    (Widgt => Preview_Y_Scroll,
                     options =>
                       "-command [list " & Get_Preview_Text & " yview]");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Preview_Y_Scroll,
                     Options => "-side right -fill y");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Get_Preview_Text,
                     Options => "-side top -fill both -expand true");
                  configure
                    (Widgt => Get_Preview_Text, options => "-state normal");
                  Delete
                    (TextWidget => Get_Preview_Text, StartIndex => "1.0",
                     Indexes => "end");
                  if not Settings.Color_Text or Executable_Name = "" then
                     Load_File;
                     goto Set_UI;
                  end if;
                  Spawn
                    (Program_Name => Executable_Name,
                     Args =>
                       Argument_String_To_List
                         (Arg_String =>
                            "--out-format=pango --force --quiet --output=" &
                            Value(Name => "HOME") &
                            "/.cache/hunter/highlight.tmp --base16 --style=" &
                            To_String(Source => Settings.Color_Theme) & " " &
                            To_String(Source => Current_Selected)).all,
                     Success => Success);
                  if not Success then
                     Load_File;
                     goto Set_UI;
                  end if;
                  Open
                    (File => File, Mode => In_File,
                     Name =>
                       Value(Name => "HOME") & "/.cache/hunter/highlight.tmp");
                  First_Line := True;
                  Load_Highlight_File_Loop :
                  while not End_Of_File(File => File) loop
                     File_Line :=
                       To_Unbounded_String(Source => Get_Line(File => File));
                     if First_Line then
                        File_Line :=
                          Unbounded_Slice
                            (Source => File_Line,
                             Low =>
                               Index(Source => File_Line, Pattern => ">") + 1,
                             High => Length(Source => File_Line));
                        First_Line := False;
                     end if;
                     exit Load_Highlight_File_Loop when End_Of_File
                         (File => File);
                     Replace_Element(Element => "&gt;", New_Element => ">");
                     Replace_Element(Element => "&lt;", New_Element => "<");
                     Replace_Element(Element => "&amp;", New_Element => "&");
                     Escape_Element(Element => "\");
                     Escape_Element(Element => "{");
                     Escape_Element(Element => "}");
                     Start_Index := 1;
                     Highlight_Text_Loop :
                     loop
                        Start_Index :=
                          Index
                            (Source => File_Line, Pattern => "<span",
                             From => Start_Index);
                        exit Highlight_Text_Loop when Start_Index = 0;
                        if Start_Index > 1 then
                           Insert
                             (TextWidget => Get_Preview_Text, Index => "end",
                              Text =>
                                "[subst -nocommands -novariables {" &
                                Slice
                                  (Source => File_Line, Low => 1,
                                   High => Start_Index - 1) &
                                "}]");
                        end if;
                        End_Index :=
                          Index
                            (Source => File_Line, Pattern => ">",
                             From => Start_Index);
                        Tag_Text :=
                          Unbounded_Slice
                            (Source => File_Line, Low => Start_Index,
                             High => End_Index);
                        Start_Color :=
                          Index(Source => Tag_Text, Pattern => "foreground=");
                        if Index
                            (Source => Tag_Text, Pattern => "foreground=") >
                          0 then
                           Tag_Name :=
                             Unbounded_Slice
                               (Source => Tag_Text, Low => Start_Color + 12,
                                High => Start_Color + 18);
                           Tag_Configure
                             (TextWidget => Get_Preview_Text,
                              TagName => To_String(Source => Tag_Name),
                              Options =>
                                "-foreground " &
                                To_String(Source => Tag_Name));
                        elsif Index
                            (Source => Tag_Text,
                             Pattern => "style=""italic""") >
                          0 then
                           Tag_Name :=
                             To_Unbounded_String(Source => "italictag");
                        elsif Index
                            (Source => Tag_Text,
                             Pattern => "weight=""bold""") >
                          0 then
                           Tag_Name :=
                             To_Unbounded_String(Source => "boldtag");
                        end if;
                        Start_Index :=
                          Start_Index + Length(Source => Tag_Text);
                        End_Index :=
                          Index
                            (Source => File_Line, Pattern => "</span>",
                             From => Start_Index) -
                          1;
                        if End_Index > 0 then
                           Insert
                             (TextWidget => Get_Preview_Text, Index => "end",
                              Text => "[subst -nocommands -novariables {" &
                              Slice(Source => File_Line, Low => Start_Index, High => End_Index) &
                              "}] [list " & To_String(Source => Tag_Name) & "]");
                        else
                           Insert
                             (TextWidget => Get_Preview_Text, Index => "end",
                              Text => "[subst -nocommands -novariables {" &
                              Slice
                                (Source => File_Line, Low => Start_Index, High => Length(Source => File_Line)) &
                              "}]");
                        end if;
                        Start_Index := 1;
                        File_Line :=
                          Unbounded_Slice
                            (Source => File_Line, Low => End_Index + 8, High => Length(Source => File_Line));
                     end loop Highlight_Text_Loop;
                     Insert
                       (TextWidget => Get_Preview_Text, Index => "end",
                        Text => "[subst -nocommands -novariables {" &
                        To_String(Source => File_Line) & LF & "}]");
                  end loop Load_Highlight_File_Loop;
                  Close(File);
                  Delete_File(Value("HOME") & "/.cache/hunter/highlight.tmp");
                  <<Set_UI>>
                  configure(Get_Preview_Text, "-state disabled");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Path_Frame);
                  Tcl_Eval(Get_Context, "update");
               end Show_Text_Preview_Block;
               Autoscroll(Preview_Y_Scroll);
            elsif Mime_Type(1 .. 5) = "image" then
               declare
                  Image: constant Tk_Photo :=
                    Create
                      ("previewimage", "-file " & To_String(Current_Selected));
                  StartX, StartY, ImageWidth, ImageHeight: Natural;
               begin
                  Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Text);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Preview_Tree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Info_Frame);
                  if Settings.Scale_Images then
                     Tcl.Tk.Ada.Pack.Pack_Forget(Preview_Y_Scroll);
                     Tcl.Tk.Ada.Pack.Pack_Forget(Preview_X_Scroll);
                     Scale_Image;
                  else
                     Delete(Get_Preview_Canvas, "all");
                     ImageWidth := Natural'Value(Width(Image));
                     ImageHeight := Natural'Value(Height(Image));
                     if ImageHeight <
                       Natural'Value
                         (Winfo_Get(Get_Preview_Frame, "height")) then
                        ImageHeight :=
                          Natural'Value
                            (Winfo_Get(Get_Preview_Frame, "height"));
                     end if;
                     StartX := ImageWidth / 2;
                     StartY := ImageHeight / 2;
                     Canvas_Create
                       (Get_Preview_Canvas, "image",
                        Natural'Image(StartX) & Natural'Image(StartY) &
                        " -image " & Image);
                     configure
                       (Get_Preview_Canvas,
                        "-width " & Width(Image) & " -height" &
                        Natural'Image(ImageHeight) & " -scrollregion [list " &
                        BBox(Get_Preview_Canvas, "all") & "]");
                     configure
                       (Preview_Y_Scroll,
                        "-command [list " & Get_Preview_Canvas & " yview]");
                     configure
                       (Preview_X_Scroll,
                        "-command [list " & Get_Preview_Canvas & " xview]");
                     Tcl.Tk.Ada.Pack.Pack
                       (Preview_X_Scroll, "-side bottom -fill x");
                     Tcl.Tk.Ada.Pack.Pack
                       (Preview_Y_Scroll, "-side right -fill y");
                  end if;
                  Tcl.Tk.Ada.Pack.Pack(Get_Preview_Canvas, "-side top");
               exception
                  when Tcl_Error_Exception =>
                     declare
                        ActionButton: constant Ttk_RadioButton :=
                          Get_Widget
                            (".mainframe.toolbars.itemtoolbar.infobutton");
                     begin
                        Button.Name :=
                          New_String
                            (".mainframe.toolbars.itemtoolbar.previewbutton");
                        Tcl.Tk.Ada.Pack.Pack_Forget(Button);
                        if Invoke(ActionButton) /= "" then
                           raise Hunter_Show_Items_Exception
                             with Mc
                               (Get_Context,
                                "{Can't show file or directory info}");
                        end if;
                     end;
               end;
               Tcl.Tk.Ada.Pack.Pack_Forget(Path_Frame);
               Tcl_Eval(Get_Context, "update");
               Autoscroll(Preview_X_Scroll);
               Autoscroll(Preview_Y_Scroll);
            else
               declare
                  ActionButton: constant Ttk_RadioButton :=
                    Get_Widget(".mainframe.toolbars.itemtoolbar.infobutton");
               begin
                  Button.Name :=
                    New_String
                      (".mainframe.toolbars.itemtoolbar.previewbutton");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Button);
                  if Invoke(ActionButton) /= "" then
                     raise Hunter_Show_Items_Exception
                       with Mc
                         (Get_Context, "{Can't show file or directory info}");
                  end if;
               end;
            end if;
         end Show_Preview_Block;
      end if;
   end Show_Preview;

   -- ****if* ShowItems/ShowItems.ShowInfo
   -- FUNCTION
   -- Show information about the currently selected file or directory.
   -- SOURCE
   procedure Show_Info is
      -- ****
      Label: Ttk_Label := Get_Widget(Get_Preview_Frame & ".title");
      SelectedItem: constant String := To_String(Current_Selected);
      Button: Ttk_Button;
      MimeType: constant String := Get_Mime_Type(SelectedItem);
      DirectorySize: Natural := 0;
      PathFrame: constant Ttk_Frame :=
        Get_Widget(".mainframe.paned.previewframe.pathframe");
   begin
      Unautoscroll(Preview_X_Scroll);
      Unautoscroll(Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(PathFrame);
      Tcl_Eval(Get_Context, "update");
      Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Text);
      Tcl.Tk.Ada.Pack.Pack_Forget(Preview_Tree);
      Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(Preview_X_Scroll);
      configure(Label, "-text {" & Mc(Get_Context, "{Information}") & "}");
      Button.Interp := Label.Interp;
      if
        (MimeType'Length > 4 and
         (MimeType(1 .. 4) /= "imag" and not Is_Text(MimeType))) and
        not Is_Directory(SelectedItem) then
         Button.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.previewbutton");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
      Label.Name := New_String(Info_Frame & ".fullpathtext");
      if not Is_Symbolic_Link(SelectedItem) then
         configure(Label, "-text {" & Mc(Get_Context, "{Full path:}") & "}");
      else
         configure(Label, "-text {" & Mc(Get_Context, "{Links to:}") & "}");
      end if;
      Label.Name := New_String(Info_Frame & ".fullpath");
      configure(Label, "-text {" & Full_Name(SelectedItem) & "}");
      Label.Name := New_String(Info_Frame & ".sizetext");
      if Is_Directory(SelectedItem) then
         configure(Label, "-text {" & Mc(Get_Context, "{Elements:}") & "}");
      else
         configure(Label, "-text {" & Mc(Get_Context, "{Size:}") & "}");
      end if;
      Label.Name := New_String(Info_Frame & ".size");
      if Is_Directory(SelectedItem) then
         if Settings.Show_Hidden then
            configure
              (Label,
               "-text {" & Natural'Image(Natural(Second_Items_List.Length)) &
               "}");
         else
            Count_Directory_Size_Loop :
            for Item of Second_Items_List loop
               if not Item.Is_Hidden then
                  DirectorySize := DirectorySize + 1;
               end if;
            end loop Count_Directory_Size_Loop;
            configure(Label, "-text {" & Natural'Image(DirectorySize) & "}");
         end if;
      elsif Is_Regular_File(SelectedItem) then
         configure
           (Label, "-text {" & Count_File_Size(Size(SelectedItem)) & "}");
      else
         configure(Label, "-text {" & Mc(Get_Context, "{Unknown}") & "}");
      end if;
      Label.Name := New_String(Info_Frame & ".lastmodified");
      if Is_Directory(SelectedItem) or Is_Regular_File(SelectedItem) then
         configure
           (Label,
            "-text {" &
            Ada.Calendar.Formatting.Image
              (Modification_Time(SelectedItem), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset) &
            "}");
      else
         configure(Label, "-text {" & Mc(Get_Context, "{Unknown}") & "}");
      end if;
      Label.Name := New_String(Info_Frame & ".filetypetext");
      if Is_Directory(SelectedItem) or not Is_Regular_File(SelectedItem) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Label.Name := New_String(Info_Frame & ".filetype");
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         Tcl.Tk.Ada.Grid.Grid(Label);
         Label.Name := New_String(Info_Frame & ".filetype");
         configure
           (Label, "-text {" & Get_Mime_Type(Full_Name(SelectedItem)) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Info_Frame & ".associatedprogramtext");
      if not Is_Regular_File(SelectedItem) and
        not Is_Directory(SelectedItem) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Button.Name := New_String(Info_Frame & ".associatedprogram");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      else
         Tcl.Tk.Ada.Grid.Grid(Label);
         Button.Name := New_String(Info_Frame & ".associatedprogram");
         declare
            ProcessDesc: Process_Descriptor;
            Result: Expect_Match;
            ExecutableName: constant String := Find_Executable("xdg-mime");
            DesktopFile: Unbounded_String;
         begin
            if ExecutableName = "" then
               return;
            end if;
            Non_Blocking_Spawn
              (ProcessDesc, ExecutableName,
               Argument_String_To_List("query default " & MimeType).all);
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
                     "-text {" & To_String(DesktopFile) & " (" &
                     Mc(Get_Context, "{not installed}") & ")}");
               end if;
            end if;
            Close(ProcessDesc);
         exception
            when Process_Died =>
               configure(Button, "-text {" & Mc(Get_Context, "{None}") & "}");
         end;
         Tcl.Tk.Ada.Grid.Grid(Button);
      end if;
      declare
         Attributes: Unbounded_String;
         Tokens: Slice_Set;
         type Integer_Access is access Integer;
         Status: constant Integer_Access := new Integer;
         Arguments: constant Argument_List :=
           (new String'("-c%a %U %G"), new String'(SelectedItem));
         procedure SetPermissionsButtons
           (Name, ButtonState: String; Permission: Character) is
            CheckButton: Ttk_CheckButton;
         begin
            CheckButton.Interp := Get_Context;
            Set_Permission_Buttons_Loop :
            for I in Button_Names'Range loop
               CheckButton.Name :=
                 New_String
                   (Info_Frame & "." & Name & "frame." &
                    To_String(Button_Names(I)));
               if I = 1 then
                  if Is_Directory(SelectedItem) then
                     Tcl.Tk.Ada.Pack.Pack_Forget(CheckButton);
                  else
                     Tcl.Tk.Ada.Pack.Pack
                       (CheckButton,
                        "-before " & Info_Frame & "." & Name & "frame.read");
                  end if;
               end if;
               State(CheckButton, ButtonState);
            end loop Set_Permission_Buttons_Loop;
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
         Attributes :=
           To_Unbounded_String
             (Get_Command_Output("stat", Arguments, "", Status));
         Create(Tokens, To_String(Attributes), " ");
         Label.Name := New_String(Info_Frame & ".grouptext");
         configure(Label, "-text {" & Mc(Get_Context, "{Group}") & ":}");
         Label.Name := New_String(Info_Frame & ".group");
         configure(Label, "-text {" & Slice(Tokens, 3) & "}");
         Label.Name := New_String(Info_Frame & ".ownertext");
         configure(Label, "-text {" & Mc(Get_Context, "{Owner}") & ":}");
         Label.Name := New_String(Info_Frame & ".owner");
         configure(Label, "-text {" & Slice(Tokens, 2) & "}");
         Label.Name := New_String(Info_Frame & ".otherstext");
         configure(Label, "-text {" & Mc(Get_Context, "{Others}") & ":}");
         if Value("USER") /= Slice(Tokens, 2) then
            SetPermissionsButtons
              ("owner", "disabled",
               Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 2));
            SetPermissionsButtons
              ("group", "disabled",
               Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 1));
            SetPermissionsButtons
              ("others", "disabled", Slice(Tokens, 1)(Slice(Tokens, 1)'Last));
         else
            SetPermissionsButtons
              ("owner", "!disabled",
               Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 2));
            SetPermissionsButtons
              ("group", "!disabled",
               Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 1));
            SetPermissionsButtons
              ("others", "!disabled", Slice(Tokens, 1)(Slice(Tokens, 1)'Last));
         end if;
      end;
      Tcl.Tk.Ada.Pack.Pack(Info_Frame);
   end Show_Info;

   -- ****o* ShowItems/ShowItems.Show_Preview_Or_Info_Command
   -- FUNCTION
   -- Show preview or information about the currently selected file or
   -- directory, depends which button was clicked
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowPreviewOrInfo
   -- SOURCE
   function Show_Preview_Or_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Preview_Or_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      if Tcl.Ada.Tcl_GetVar(Interp, "previewtype") = "preview" then
         Show_Preview;
      else
         Show_Info;
      end if;
      return TCL_OK;
   end Show_Preview_Or_Info_Command;

   function Show_Selected_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      DirectoryTree: constant Ttk_Tree_View :=
        Get_Widget(".mainframe.paned.directoryframe.directorytree", Interp);
      Tokens: Slice_Set;
      Items: Unbounded_String;
      ActionButton: Ttk_RadioButton;
   begin
      Selected_Items.Clear;
      Items := To_Unbounded_String(Selection(DirectoryTree));
      if Items /= Null_Unbounded_String then
         Create(Tokens, To_String(Items), " ");
         Set_Selected_List_Loop :
         for I in 1 .. Slice_Count(Tokens) loop
            Selected_Items.Append
              (Items_List(Positive'Value(Slice(Tokens, I))).Path);
         end loop Set_Selected_List_Loop;
      else
         Selected_Items.Append(Common.Current_Directory);
      end if;
      if not Settings.Show_Preview or
        (Selected_Items(1) = Current_Selected and
         Current_Selected /= Common.Current_Directory) then
         return TCL_OK;
      end if;
      Current_Selected := Selected_Items(1);
      if New_Action = CREATELINK then
         return TCL_OK;
      end if;
      SetActionsButtons;
      ActionButton.Interp := Interp;
      if Is_Directory(To_String(Current_Selected)) or
        Is_Regular_File(To_String(Current_Selected)) then
         ActionButton.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.previewbutton");
      else
         ActionButton.Name :=
           New_String(".mainframe.toolbars.itemtoolbar.infobutton");
      end if;
      if Invoke(ActionButton) /= "" and Invoke(ActionButton) /= "0" then
         Set_Bookmark_Button;
         return TCL_OK;
      end if;
      Set_Bookmark_Button;
      return TCL_OK;
   end Show_Selected_Command;

   -- ****o* ShowItems/ShowItems.Set_Permissions_Command
   -- FUNCTION
   -- Set the permissions for the selected file or directory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetPermissions
   -- SOURCE
   function Set_Permissions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Permissions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      SelectedItem: constant String := Full_Name(To_String(Current_Selected));
      PermissionsString: Unbounded_String;
      Permission: Natural range 0 .. 7;
      Names: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String("owner"), To_Unbounded_String("group"),
         To_Unbounded_String("others"));
   begin
      if Is_Directory(SelectedItem) then
         PermissionsString := To_Unbounded_String("040");
      else
         PermissionsString := To_Unbounded_String("00");
      end if;
      Set_Permissions_Loop :
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
      end loop Set_Permissions_Loop;
      Tcl.Ada.Tcl_Eval
        (Interp,
         "file attributes {" & SelectedItem & "} -permissions " &
         To_String(PermissionsString));
      return TCL_OK;
   end Set_Permissions_Command;

   -- ****o* ShowItems/ShowItems.GoToDirectory_Command
   -- FUNCTION
   -- Go to the selected directory in preview
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToDirectory ?selecteditem?
   -- Selecteditem is full path to the currently selected file or directory
   -- SOURCE
   function GoToDirectory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToDirectory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      SelectedItem: Unbounded_String;
   begin
      if Argc = 2 then
         SelectedItem := To_Unbounded_String(CArgv.Arg(Argv, 1));
      else
         if Selection(Preview_Tree) = "" then
            return TCL_OK;
         end if;
         SelectedItem :=
           Destination_Directory & "/" &
           To_Unbounded_String
             (Set(Preview_Tree, Selection(Preview_Tree), "name"));
         if not Is_Directory(To_String(SelectedItem)) then
            return TCL_OK;
         end if;
      end if;
      Destination_Directory := SelectedItem;
      Load_Directory(To_String(SelectedItem), True);
      Update_Directory_List(True, "preview");
      Execute_Modules
        (Interp, ON_ENTER, "{" & To_String(Destination_Directory) & "}");
      return TCL_OK;
   end GoToDirectory_Command;

   procedure Create_Show_Items_Ui is
      Paned: constant Ttk_PanedWindow := Get_Widget(".mainframe.paned");
      Label: Ttk_Label;
      Button: Ttk_Button;
      ButtonTexts: constant array(1 .. 3) of Unbounded_String :=
        (To_Unbounded_String(Mc(Get_Context, "{Can execute}")),
         To_Unbounded_String(Mc(Get_Context, "{Can read}")),
         To_Unbounded_String(Mc(Get_Context, "{Can write}")));
      PathButtonsFrame: Ttk_Frame;
      pragma Unreferenced(PathButtonsFrame);
      Font: constant String :=
        (if Settings.Monospace_Font then "TkFixedFont" else "TkDefaultFont");
      procedure CreatePermissionsFrame(Name: String; Row: Positive) is
         Frame: constant Ttk_Frame :=
           Create(".mainframe.paned.previewframe.infoframe." & Name & "frame");
         CheckButton: Ttk_CheckButton;
      begin
         Label :=
           Create(".mainframe.paned.previewframe.infoframe." & Name & "text");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-column 0 -row" & Positive'Image(Row) & " -sticky w");
         Label := Create(".mainframe.paned.previewframe.infoframe." & Name);
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-column 1 -row" & Positive'Image(Row) & " -sticky w");
         Set_Permission_Buttons_Loop :
         for I in Button_Names'Range loop
            CheckButton :=
              Create
                (Frame & "." & To_String(Button_Names(I)),
                 "-text {" & To_String(ButtonTexts(I)) & "} -variable " &
                 Name & To_String(Button_Names(I)) &
                 " -command SetPermissions");
            Tcl.Tk.Ada.Pack.Pack(CheckButton, "-anchor w");
         end loop Set_Permission_Buttons_Loop;
         Tcl.Tk.Ada.Grid.Grid
           (Frame, "-column 1 -row" & Positive'Image(Row + 1));
      end CreatePermissionsFrame;
   begin
      Preview_Frame := Create(Paned & ".previewframe");
      Label := Create(Get_Preview_Frame & ".title");
      Tcl.Tk.Ada.Pack.Pack(Label);
      PathButtonsFrame := Create(Get_Preview_Frame & ".pathframe");
      Preview_X_Scroll :=
        Create
          (Get_Preview_Frame & ".scrollx",
           "-orient horizontal -command [list " & Get_Preview_Frame &
           ".directorytree xview]");
      Preview_Y_Scroll :=
        Create
          (Get_Preview_Frame & ".scrolly",
           "-orient vertical -command [list " & Get_Preview_Frame &
           ".directorytree yview]");
      Preview_Tree :=
        Create
          (Get_Preview_Frame & ".directorytree",
           "-columns [list name] -xscrollcommand {" & Preview_X_Scroll &
           " set} -yscrollcommand {" & Preview_Y_Scroll &
           " set} -selectmode none ");
      Heading
        (Preview_Tree, "name",
         "-text {" & Mc(Get_Context, "{Name}") &
         "} -image {arrow-down} -command {Sort previewname}");
      Column(Preview_Tree, "#0", "-stretch false -width 50");
      Tag_Bind(Preview_Tree, "itemrow", "<Double-1>", "GoToDirectory");
      Bind(Preview_Tree, "<Return>", "GoToDirectory");
      Preview_Text :=
        Create
          (Get_Preview_Frame & ".previewtext",
           "-wrap char -yscrollcommand {" & Preview_Y_Scroll & " set} -font " &
           Font);
      Tag_Configure(Get_Preview_Text, "boldtag", "-font bold");
      Tag_Configure(Get_Preview_Text, "italictag", "-font italic");
      Preview_Canvas :=
        Create
          (Get_Preview_Frame & ".previewcanvas",
           "-xscrollcommand {" & Preview_X_Scroll & " set} -yscrollcommand {" &
           Preview_Y_Scroll & " set}");
      Info_Frame := Create(Get_Preview_Frame & ".infoframe");
      Label := Create(Info_Frame & ".fullpathtext");
      Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w");
      Label :=
        Create
          (Info_Frame & ".fullpath",
           "-wraplength " &
           Natural'Image(Natural'Value(Winfo_Get(Paned, "width")) / 3));
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0 -sticky w");
      Label := Create(Info_Frame & ".sizetext");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 1 -sticky w");
      Label := Create(Info_Frame & ".size");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 1 -sticky w");
      Label :=
        Create
          (Info_Frame & ".lastmodifiedtext",
           "-text {" & Mc(Get_Context, "{Last modified:}") & "}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 2 -sticky w");
      Label := Create(Info_Frame & ".lastmodified");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 2 -sticky w");
      Label :=
        Create
          (Info_Frame & ".filetypetext",
           "-text {" & Mc(Get_Context, "{File type:}") & "}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 3 -sticky w");
      Label := Create(Info_Frame & ".filetype");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 3 -sticky w");
      Label :=
        Create
          (Info_Frame & ".associatedprogramtext",
           "-text {" & Mc(Get_Context, "{Associated program:}") & "}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 0 -row 4 -sticky w");
      Button :=
        Create
          (Info_Frame & ".associatedprogram",
           "-command ToggleApplicationsMenu");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 4 -sticky w");
      CreatePermissionsFrame("owner", 5);
      CreatePermissionsFrame("group", 7);
      CreatePermissionsFrame("others", 9);
      Add_Command("ShowSelected", Show_Selected_Command'Access);
      Add_Command("ShowPreviewOrInfo", Show_Preview_Or_Info_Command'Access);
      Add_Command("SetPermissions", Set_Permissions_Command'Access);
      Add_Command("GoToDirectory", GoToDirectory_Command'Access);
      Add
        (Button,
         Mc
           (Get_Context,
            "{Select new associated program with that type of file or directory.}"));
      if Settings.Show_Preview then
         Add(Paned, Get_Preview_Frame, "-weight 20");
      end if;
      CreateProgramsMenuUI;
   end Create_Show_Items_Ui;

   procedure Show_Destination is
      Paned: constant Ttk_PanedWindow := Get_Widget(".mainframe.paned");
      Frame: Ttk_Frame := Get_Widget(Get_Preview_Frame & ".pathframe");
   begin
      if not Settings.Show_Preview then
         Add(Paned, Get_Preview_Frame, "-weight 20");
      end if;
      Unautoscroll(Preview_X_Scroll);
      Unautoscroll(Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack
        (Frame, "-after " & Get_Preview_Frame & ".title -fill x");
      configure
        (Preview_X_Scroll, "-command [list " & Preview_Tree & " xview]");
      Tcl.Tk.Ada.Pack.Pack(Preview_X_Scroll, "-side bottom -fill x");
      configure
        (Preview_Y_Scroll, "-command [list " & Preview_Tree & " yview]");
      Tcl.Tk.Ada.Pack.Pack(Preview_Y_Scroll, "-side right -fill y");
      configure(Preview_Tree, "-selectmode browse");
      Tcl.Tk.Ada.Pack.Pack(Preview_Tree, "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Text);
      Tcl.Tk.Ada.Pack.Pack_Forget(Info_Frame);
      Frame.Name := New_String(Get_Preview_Frame & ".title");
      configure
        (Frame, "-text {" & Mc(Get_Context, "{Destination directory}") & "}");
      Destination_Directory := Common.Current_Directory;
      Load_Directory(To_String(Destination_Directory), True);
      Update_Directory_List(True, "preview");
      Autoscroll(Preview_X_Scroll);
      Autoscroll(Preview_Y_Scroll);
   end Show_Destination;

   procedure Show_Output is
      Frame: Ttk_Frame;
      Paned: constant Ttk_PanedWindow := Get_Widget(".mainframe.paned");
   begin
      Frame.Interp := Get_Context;
      if not Settings.Show_Preview then
         Add(Paned, Get_Preview_Frame, "-weight 20");
      end if;
      Unautoscroll(Preview_Y_Scroll);
      configure
        (Preview_Y_Scroll, "-command [list " & Get_Preview_Text & " yview]");
      Tcl.Tk.Ada.Pack.Pack(Preview_Y_Scroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack
        (Get_Preview_Text, "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack_Forget(Preview_X_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Preview_Tree);
      Tcl.Tk.Ada.Pack.Pack_Forget(Info_Frame);
      Frame.Name := New_String(Get_Preview_Frame & ".title");
      configure(Frame, "-text {" & Mc(Get_Context, "{Command output}") & "}");
      configure(Get_Preview_Text, "-state normal");
      Delete(Get_Preview_Text, "1.0", "end");
      configure(Get_Preview_Text, "-state disabled");
      Autoscroll(Preview_Y_Scroll);
   end Show_Output;

   procedure Update_Output(Text: String) is
   begin
      configure(Get_Preview_Text, "-state normal");
      Insert(Get_Preview_Text, "end", "{" & Text & "}");
      configure(Get_Preview_Text, "-state disabled");
      Tcl_Eval(Get_Context, "update");
   end Update_Output;

end ShowItems;
