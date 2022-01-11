-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****if* ShowItems/ShowItems.Get_Preview_X_Scroll
   -- FUNCTION
   -- Get the X axis scroll for previews
   -- RESULT
   -- The X axis scroll for previews
   -- SOURCE
   function Get_Preview_X_Scroll return Ttk_Scrollbar is
      -- ****
   begin
      return Preview_X_Scroll;
   end Get_Preview_X_Scroll;

   -- ****iv* ShowItems/ShowItems.Preview_Y_Scroll
   -- FUNCTION
   -- Y coordinates scrollbar for previews
   -- SOURCE
   Preview_Y_Scroll: Ttk_Scrollbar;
   -- ****

   -- ****if* ShowItems/ShowItems.Get_Preview_Y_Scroll
   -- FUNCTION
   -- Get the Y axis scroll for previews
   -- RESULT
   -- The Y axis scroll for previews
   -- SOURCE
   function Get_Preview_Y_Scroll return Ttk_Scrollbar is
      -- ****
   begin
      return Preview_Y_Scroll;
   end Get_Preview_Y_Scroll;

   -- ****iv* ShowItems/ShowItems.Preview_Tree
   -- FUNCTION
   -- Ttk_Tree_View used to show directories previews
   -- SOURCE
   Preview_Tree: Ttk_Tree_View;
   -- ****

   -- ****if* ShowItems/ShowItems.Get_Preview_Tree
   -- FUNCTION
   -- Get the Ttk_Tree_View for show directories previews
   -- RESULT
   -- The Ttk_Tree_View for show directories previews
   -- SOURCE
   function Get_Preview_Tree return Ttk_Tree_View is
      -- ****
   begin
      return Preview_Tree;
   end Get_Preview_Tree;

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

   -- ****if* ShowItems/ShowItems.Get_Info_Frame
   -- FUNCTION
   -- Get the Ttk_Frame for show information about the selected item
   -- RESULT
   -- The Ttk_Frame for show information about the selected item
   -- SOURCE
   function Get_Info_Frame return Ttk_Frame is
      -- ****
   begin
      return Info_Frame;
   end Get_Info_Frame;

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
      Unautoscroll(Scroll => Get_Preview_X_Scroll);
      Set
        (ScrollbarWidget => Get_Preview_X_Scroll, First => "0.0",
         Last => "1.0");
      Unautoscroll(Scroll => Get_Preview_Y_Scroll);
      Set
        (ScrollbarWidget => Get_Preview_Y_Scroll, First => "0.0",
         Last => "1.0");
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
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Tree);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_X_Scroll);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Y_Scroll);
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Info_Frame);
         configure
           (Widgt => Get_Preview_Y_Scroll,
            options => "-command [list " & Get_Preview_Tree & " yview]");
         configure
           (Widgt => Get_Preview_X_Scroll,
            options => "-command [list " & Get_Preview_Tree & " xview]");
         configure(Widgt => Get_Preview_Tree, options => "-selectmode none");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Get_Preview_X_Scroll, Options => "-side bottom -fill x");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Get_Preview_Y_Scroll, Options => "-side right -fill y");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Get_Preview_Tree,
            Options => "-side top -fill both -expand true");
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Path_Frame);
         Tcl_Eval(interp => Get_Context, strng => "update");
         Update_Directory_List(Clear => True, Frame_Name => "preview");
         Autoscroll(Scroll => Get_Preview_X_Scroll);
         Autoscroll(Scroll => Get_Preview_Y_Scroll);
      else
         Show_Preview_Block :
         declare
            Mime_Type: constant String :=
              Get_Mime_Type
                (File_Name => To_String(Source => Current_Selected));
            Hunter_Show_Items_Exception: exception;
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
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Tree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_X_Scroll);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Info_Frame);
                  configure
                    (Widgt => Get_Preview_Y_Scroll,
                     options =>
                       "-command [list " & Get_Preview_Text & " yview]");
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Get_Preview_Y_Scroll,
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
                     goto Set_Ui;
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
                     goto Set_Ui;
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
                              Text =>
                                "[subst -nocommands -novariables {" &
                                Slice
                                  (Source => File_Line, Low => Start_Index,
                                   High => End_Index) &
                                "}] [list " & To_String(Source => Tag_Name) &
                                "]");
                        else
                           Insert
                             (TextWidget => Get_Preview_Text, Index => "end",
                              Text =>
                                "[subst -nocommands -novariables {" &
                                Slice
                                  (Source => File_Line, Low => Start_Index,
                                   High => Length(Source => File_Line)) &
                                "}]");
                        end if;
                        Start_Index := 1;
                        File_Line :=
                          Unbounded_Slice
                            (Source => File_Line, Low => End_Index + 8,
                             High => Length(Source => File_Line));
                     end loop Highlight_Text_Loop;
                     Insert
                       (TextWidget => Get_Preview_Text, Index => "end",
                        Text =>
                          "[subst -nocommands -novariables {" &
                          To_String(Source => File_Line) & LF & "}]");
                  end loop Load_Highlight_File_Loop;
                  Close(File => File);
                  Delete_File
                    (Name =>
                       Value(Name => "HOME") & "/.cache/hunter/highlight.tmp");
                  <<Set_Ui>>
                  configure
                    (Widgt => Get_Preview_Text, options => "-state disabled");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Path_Frame);
                  Tcl_Eval(interp => Get_Context, strng => "update");
               end Show_Text_Preview_Block;
               Autoscroll(Scroll => Get_Preview_Y_Scroll);
            elsif Mime_Type(1 .. 5) = "image" then
               Show_Image_Preview_Block :
               declare
                  Preview_Image: constant Tk_Photo :=
                    Create
                      (pathName => "previewimage",
                       options =>
                         "-file " & To_String(Source => Current_Selected));
                  Start_X, Start_Y, Image_Width, Image_Height: Natural := 0;
               begin
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Tree);
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Info_Frame);
                  if Settings.Scale_Images then
                     Tcl.Tk.Ada.Pack.Pack_Forget
                       (Slave => Get_Preview_Y_Scroll);
                     Tcl.Tk.Ada.Pack.Pack_Forget
                       (Slave => Get_Preview_X_Scroll);
                     Scale_Image;
                  else
                     Delete
                       (CanvasWidget => Get_Preview_Canvas, TagOrId => "all");
                     Image_Width := Natural'Value(Width(Img => Preview_Image));
                     Image_Height :=
                       Natural'Value(Height(Img => Preview_Image));
                     if Image_Height <
                       Natural'Value
                         (Winfo_Get
                            (Widgt => Get_Preview_Frame,
                             Info => "height")) then
                        Image_Height :=
                          Natural'Value
                            (Winfo_Get
                               (Widgt => Get_Preview_Frame, Info => "height"));
                     end if;
                     Start_X := Image_Width / 2;
                     Start_Y := Image_Height / 2;
                     Canvas_Create
                       (Parent => Get_Preview_Canvas, Child_Type => "image",
                        Options =>
                          Natural'Image(Start_X) & Natural'Image(Start_Y) &
                          " -image " & Preview_Image);
                     configure
                       (Widgt => Get_Preview_Canvas,
                        options =>
                          "-width " & Width(Img => Preview_Image) &
                          " -height" & Natural'Image(Image_Height) &
                          " -scrollregion [list " &
                          BBox
                            (CanvasWidget => Get_Preview_Canvas,
                             TagOrId => "all") &
                          "]");
                     configure
                       (Widgt => Get_Preview_Y_Scroll,
                        options =>
                          "-command [list " & Get_Preview_Canvas & " yview]");
                     configure
                       (Widgt => Get_Preview_X_Scroll,
                        options =>
                          "-command [list " & Get_Preview_Canvas & " xview]");
                     Tcl.Tk.Ada.Pack.Pack
                       (Slave => Get_Preview_X_Scroll,
                        Options => "-side bottom -fill x");
                     Tcl.Tk.Ada.Pack.Pack
                       (Slave => Get_Preview_Y_Scroll,
                        Options => "-side right -fill y");
                  end if;
                  Tcl.Tk.Ada.Pack.Pack
                    (Slave => Get_Preview_Canvas, Options => "-side top");
               exception
                  when Tcl_Error_Exception =>
                     Show_Exception_Block :
                     declare
                        Action_Button: constant Ttk_RadioButton :=
                          Get_Widget
                            (pathName =>
                               ".mainframe.toolbars.itemtoolbar.infobutton");
                     begin
                        Button.Name :=
                          New_String
                            (Str =>
                               ".mainframe.toolbars.itemtoolbar.previewbutton");
                        Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
                        if Invoke(Buttn => Action_Button) /= "" then
                           raise Hunter_Show_Items_Exception
                             with Mc
                               (Interp => Get_Context,
                                Src_String =>
                                  "{Can't show file or directory info}");
                        end if;
                     end Show_Exception_Block;
               end Show_Image_Preview_Block;
               Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Path_Frame);
               Tcl_Eval(interp => Get_Context, strng => "update");
               Autoscroll(Scroll => Get_Preview_X_Scroll);
               Autoscroll(Scroll => Get_Preview_Y_Scroll);
            else
               Show_Info_Block :
               declare
                  Action_Button: constant Ttk_RadioButton :=
                    Get_Widget
                      (pathName =>
                         ".mainframe.toolbars.itemtoolbar.infobutton");
               begin
                  Button.Name :=
                    New_String
                      (Str => ".mainframe.toolbars.itemtoolbar.previewbutton");
                  Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
                  if Invoke(Buttn => Action_Button) /= "" then
                     raise Hunter_Show_Items_Exception
                       with Mc
                         (Interp => Get_Context,
                          Src_String => "{Can't show file or directory info}");
                  end if;
               end Show_Info_Block;
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
      Label: Ttk_Label := Get_Widget(pathName => Get_Preview_Frame & ".title");
      Selected_Item: constant String := To_String(Source => Current_Selected);
      Button: Ttk_Button := Get_Widget(pathName => ".");
      Mime_Type: constant String := Get_Mime_Type(File_Name => Selected_Item);
      Directory_Size: Natural := 0;
      Path_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => ".mainframe.paned.previewframe.pathframe");
   begin
      Unautoscroll(Scroll => Get_Preview_X_Scroll);
      Unautoscroll(Scroll => Get_Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Path_Frame);
      Tcl_Eval(interp => Get_Context, strng => "update");
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Tree);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_X_Scroll);
      configure
        (Widgt => Label,
         options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Information}") & "}");
      Button.Interp := Label.Interp;
      if
        (Mime_Type'Length > 4 and
         (Mime_Type(1 .. 4) /= "imag" and
          not Is_Text(Mime_Type => Mime_Type))) and
        not Is_Directory(Name => Selected_Item) then
         Button.Name :=
           New_String(Str => ".mainframe.toolbars.itemtoolbar.previewbutton");
         Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Button);
      end if;
      Label.Name := New_String(Str => Get_Info_Frame & ".fullpathtext");
      if Is_Symbolic_Link(Name => Selected_Item) then
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Mc(Interp => Get_Context, Src_String => "{Links to:}") & "}");
      else
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Mc(Interp => Get_Context, Src_String => "{Full path:}") & "}");
      end if;
      Label.Name := New_String(Str => Get_Info_Frame & ".fullpath");
      configure
        (Widgt => Label,
         options => "-text {" & Full_Name(Name => Selected_Item) & "}");
      Label.Name := New_String(Str => Get_Info_Frame & ".sizetext");
      if Is_Directory(Name => Selected_Item) then
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Mc(Interp => Get_Context, Src_String => "{Elements:}") & "}");
      else
         configure
           (Widgt => Label,
            options =>
              "-text {" & Mc(Interp => Get_Context, Src_String => "{Size:}") &
              "}");
      end if;
      Label.Name := New_String(Str => Get_Info_Frame & ".size");
      if Is_Directory(Name => Selected_Item) then
         if Settings.Show_Hidden then
            configure
              (Widgt => Label,
               options =>
                 "-text {" & Natural'Image(Natural(Second_Items_List.Length)) &
                 "}");
         else
            Count_Directory_Size_Loop :
            for Item of Second_Items_List loop
               if not Item.Is_Hidden then
                  Directory_Size := Directory_Size + 1;
               end if;
            end loop Count_Directory_Size_Loop;
            configure
              (Widgt => Label,
               options => "-text {" & Natural'Image(Directory_Size) & "}");
         end if;
      elsif Is_Regular_File(Name => Selected_Item) then
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Count_File_Size(Size => Size(Name => Selected_Item)) & "}");
      else
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Mc(Interp => Get_Context, Src_String => "{Unknown}") & "}");
      end if;
      Label.Name := New_String(Str => Get_Info_Frame & ".lastmodified");
      if Is_Directory(Name => Selected_Item) or
        Is_Regular_File(Name => Selected_Item) then
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Ada.Calendar.Formatting.Image
                (Date => Modification_Time(Name => Selected_Item),
                 Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset) &
              "}");
      else
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Mc(Interp => Get_Context, Src_String => "{Unknown}") & "}");
      end if;
      Label.Name := New_String(Str => Get_Info_Frame & ".filetypetext");
      if Is_Directory(Name => Selected_Item) or
        not Is_Regular_File(Name => Selected_Item) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
         Label.Name := New_String(Str => Get_Info_Frame & ".filetype");
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
      else
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Label.Name := New_String(Str => Get_Info_Frame & ".filetype");
         configure
           (Widgt => Label,
            options =>
              "-text {" &
              Get_Mime_Type(File_Name => Full_Name(Name => Selected_Item)) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      end if;
      Label.Name :=
        New_String(Str => Get_Info_Frame & ".associatedprogramtext");
      if not Is_Regular_File(Name => Selected_Item) and
        not Is_Directory(Name => Selected_Item) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
         Button.Name :=
           New_String(Str => Get_Info_Frame & ".associatedprogram");
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Button);
      else
         Tcl.Tk.Ada.Grid.Grid(Slave => Label);
         Button.Name :=
           New_String(Str => Get_Info_Frame & ".associatedprogram");
         Show_Assigned_Program_Block :
         declare
            Process_Desc: Process_Descriptor;
            Result: Expect_Match;
            Executable_Name: constant String :=
              Find_Executable(Name => "xdg-mime");
            Desktop_File: Unbounded_String := Null_Unbounded_String;
         begin
            if Executable_Name = "" then
               return;
            end if;
            Non_Blocking_Spawn
              (Descriptor => Process_Desc, Command => Executable_Name,
               Args =>
                 Argument_String_To_List
                   (Arg_String => "query default " & Mime_Type).all);
            Expect
              (Descriptor => Process_Desc, Result => Result, Regexp => ".+",
               Timeout => 1_000);
            if Result = 1 then
               Desktop_File :=
                 To_Unbounded_String
                   (Source =>
                      GetProgramName
                        (DesktopFile =>
                           Expect_Out_Match(Descriptor => Process_Desc)));
               if Index(Source => Desktop_File, Pattern => ".desktop") = 0 then
                  configure
                    (Widgt => Button,
                     options =>
                       "-text {" & To_String(Source => Desktop_File) & "}");
               else
                  configure
                    (Widgt => Button,
                     options =>
                       "-text {" & To_String(Source => Desktop_File) & " (" &
                       Mc(Interp => Get_Context,
                          Src_String => "{not installed}") &
                       ")}");
               end if;
            end if;
            Close(Descriptor => Process_Desc);
         exception
            when Process_Died =>
               configure
                 (Widgt => Button,
                  options =>
                    "-text {" &
                    Mc(Interp => Get_Context, Src_String => "{None}") & "}");
         end Show_Assigned_Program_Block;
         Tcl.Tk.Ada.Grid.Grid(Slave => Button);
      end if;
      Show_Permissions_Block :
      declare
         Attributes: Unbounded_String;
         Tokens: Slice_Set;
         type Integer_Access is access Integer;
         Status: constant Integer_Access := new Integer;
         Arguments: constant Argument_List :=
           (1 => new String'("-c%a %U %G"), 2 => new String'(Selected_Item));
         procedure Set_Permissions_Buttons
           (Name, Button_State: String; Permission: Character) is
            Check_Button: Ttk_CheckButton := Get_Widget(pathName => ".");
         begin
            Set_Permission_Buttons_Loop :
            for I in Button_Names'Range loop
               Check_Button.Name :=
                 New_String
                   (Str =>
                      Get_Info_Frame & "." & Name & "frame." &
                      To_String(Source => Button_Names(I)));
               if I = 1 then
                  if Is_Directory(Name => Selected_Item) then
                     Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Check_Button);
                  else
                     Tcl.Tk.Ada.Pack.Pack
                       (Slave => Check_Button,
                        Options =>
                          "-before " & Get_Info_Frame & "." & Name &
                          "frame.read");
                  end if;
               end if;
               State(Widget => Check_Button, StateSpec => Button_State);
            end loop Set_Permission_Buttons_Loop;
            Tcl.Ada.Tcl_SetVar
              (interp => Check_Button.Interp, varName => Name & "execute",
               newValue => "0");
            Tcl.Ada.Tcl_SetVar
              (interp => Check_Button.Interp, varName => Name & "read",
               newValue => "0");
            Tcl.Ada.Tcl_SetVar
              (interp => Check_Button.Interp, varName => Name & "write",
               newValue => "0");
            case Permission is
               when '1' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp,
                     varName => Name & "execute", newValue => "1");
               when '2' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "write",
                     newValue => "1");
               when '3' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp,
                     varName => Name & "execute", newValue => "1");
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "write",
                     newValue => "1");
               when '4' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "read",
                     newValue => "1");
               when '5' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp,
                     varName => Name & "execute", newValue => "1");
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "read",
                     newValue => "1");
               when '6' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "read",
                     newValue => "1");
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "write",
                     newValue => "1");
               when '7' =>
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp,
                     varName => Name & "execute", newValue => "1");
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "read",
                     newValue => "1");
                  Tcl.Ada.Tcl_SetVar
                    (interp => Check_Button.Interp, varName => Name & "write",
                     newValue => "1");
               when others =>
                  null;
            end case;
         end Set_Permissions_Buttons;
      begin
         Attributes :=
           To_Unbounded_String
             (Source =>
                Get_Command_Output
                  (Command => "stat", Arguments => Arguments, Input => "",
                   Status => Status));
         Create
           (S => Tokens, From => To_String(Source => Attributes),
            Separators => " ");
         Label.Name := New_String(Str => Get_Info_Frame & ".grouptext");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Mc(Interp => Get_Context, Src_String => "{Group}") &
              ":}");
         Label.Name := New_String(Str => Get_Info_Frame & ".group");
         configure
           (Widgt => Label,
            options => "-text {" & Slice(S => Tokens, Index => 3) & "}");
         Label.Name := New_String(Str => Get_Info_Frame & ".ownertext");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Mc(Interp => Get_Context, Src_String => "{Owner}") &
              ":}");
         Label.Name := New_String(Str => Get_Info_Frame & ".owner");
         configure
           (Widgt => Label,
            options => "-text {" & Slice(S => Tokens, Index => 2) & "}");
         Label.Name := New_String(Str => Get_Info_Frame & ".otherstext");
         configure
           (Widgt => Label,
            options =>
              "-text {" & Mc(Interp => Get_Context, Src_String => "{Others}") &
              ":}");
         if Value(Name => "USER") = Slice(S => Tokens, Index => 2) then
            Set_Permissions_Buttons
              (Name => "owner", Button_State => "!disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last - 2));
            Set_Permissions_Buttons
              (Name => "group", Button_State => "!disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last - 1));
            Set_Permissions_Buttons
              (Name => "others", Button_State => "!disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last));
         else
            Set_Permissions_Buttons
              (Name => "owner", Button_State => "disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last - 2));
            Set_Permissions_Buttons
              (Name => "group", Button_State => "disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last - 1));
            Set_Permissions_Buttons
              (Name => "others", Button_State => "disabled",
               Permission =>
                 Slice(S => Tokens, Index => 1)
                   (Slice(S => Tokens, Index => 1)'Last));
         end if;
      end Show_Permissions_Block;
      Tcl.Tk.Ada.Pack.Pack(Slave => Get_Info_Frame);
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Preview_Or_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
   begin
      if Tcl.Ada.Tcl_GetVar(interp => Interp, varName => "previewtype") =
        "preview" then
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
      Directory_Tree: constant Ttk_Tree_View :=
        Get_Widget
          (pathName => ".mainframe.paned.directoryframe.directorytree",
           Interp => Interp);
      Tokens: Slice_Set; --## rule line off IMPROPER_INITIALIZATION
      Items: Unbounded_String;
      Action_Button: Ttk_RadioButton :=
        Get_Widget(pathName => ".", Interp => Interp);
   begin
      Selected_Items.Clear;
      Items :=
        To_Unbounded_String
          (Source => Selection(TreeViewWidget => Directory_Tree));
      if Items = Null_Unbounded_String then
         Selected_Items.Append(New_Item => Common.Current_Directory);
      else
         Create
           (S => Tokens, From => To_String(Source => Items),
            Separators => " ");
         Set_Selected_List_Loop :
         for I in 1 .. Slice_Count(S => Tokens) loop
            Selected_Items.Append
              (New_Item =>
                 Items_List(Positive'Value(Slice(S => Tokens, Index => I)))
                   .Path);
         end loop Set_Selected_List_Loop;
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
      if Is_Directory(Name => To_String(Source => Current_Selected)) or
        Is_Regular_File(Name => To_String(Source => Current_Selected)) then
         Action_Button.Name :=
           New_String(Str => ".mainframe.toolbars.itemtoolbar.previewbutton");
      else
         Action_Button.Name :=
           New_String(Str => ".mainframe.toolbars.itemtoolbar.infobutton");
      end if;
      if Invoke(Buttn => Action_Button) not in "" | "0" then
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Permissions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Selected_Item: constant String :=
        Full_Name(Name => To_String(Source => Current_Selected));
      Permissions_String: Unbounded_String;
      Permission: Natural range 0 .. 7 := 0;
      Names: constant array(1 .. 3) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "owner"),
         2 => To_Unbounded_String(Source => "group"),
         3 => To_Unbounded_String(Source => "others"));
   begin
      if Is_Directory(Name => Selected_Item) then
         Permissions_String := To_Unbounded_String(Source => "040");
      else
         Permissions_String := To_Unbounded_String(Source => "00");
      end if;
      Set_Permissions_Loop :
      for Name of Names loop
         Permission := 0;
         if Tcl.Ada.Tcl_GetVar
             (interp => Interp,
              varName => To_String(Source => Name) & "execute") =
           "1" then
            Permission := Permission + 1;
         end if;
         if Tcl.Ada.Tcl_GetVar
             (interp => Interp,
              varName => To_String(Source => Name) & "write") =
           "1" then
            Permission := Permission + 2;
         end if;
         if Tcl.Ada.Tcl_GetVar
             (interp => Interp,
              varName => To_String(Source => Name) & "read") =
           "1" then
            Permission := Permission + 4;
         end if;
         Append
           (Source => Permissions_String,
            New_Item =>
              Trim(Source => Natural'Image(Permission), Side => Both));
      end loop Set_Permissions_Loop;
      Tcl.Ada.Tcl_Eval
        (interp => Interp,
         strng =>
           "file attributes {" & Selected_Item & "} -permissions " &
           To_String(Source => Permissions_String));
      return TCL_OK;
   end Set_Permissions_Command;

   -- ****o* ShowItems/ShowItems.Go_To_Directory_Command
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
   function Go_To_Directory_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Go_To_Directory_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      Selected_Item: Unbounded_String;
   begin
      if Argc = 2 then
         Selected_Item :=
           To_Unbounded_String(Source => CArgv.Arg(Argv => Argv, N => 1));
      else
         if Selection(TreeViewWidget => Get_Preview_Tree) = "" then
            return TCL_OK;
         end if;
         Selected_Item :=
           Destination_Directory & "/" &
           To_Unbounded_String
             (Source =>
                Set
                  (TreeViewWidget => Get_Preview_Tree,
                   Item => Selection(TreeViewWidget => Get_Preview_Tree),
                   Column => "name"));
         if not Is_Directory(Name => To_String(Source => Selected_Item)) then
            return TCL_OK;
         end if;
      end if;
      Destination_Directory := Selected_Item;
      Load_Directory
        (Directory_Name => To_String(Source => Selected_Item), Second => True);
      Update_Directory_List(Clear => True, Frame_Name => "preview");
      Execute_Modules
        (Interpreter => Interp, State => ON_ENTER,
         Arguments => "{" & To_String(Source => Destination_Directory) & "}");
      return TCL_OK;
   end Go_To_Directory_Command;

   procedure Create_Show_Items_Ui is
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => ".mainframe.paned");
      Label: Ttk_Label;
      Button: Ttk_Button;
      Button_Texts: constant array(1 .. 3) of Unbounded_String :=
        (1 =>
           To_Unbounded_String
             (Source =>
                Mc(Interp => Get_Context, Src_String => "{Can execute}")),
         2 =>
           To_Unbounded_String
             (Source => Mc(Interp => Get_Context, Src_String => "{Can read}")),
         3 =>
           To_Unbounded_String
             (Source =>
                Mc(Interp => Get_Context, Src_String => "{Can write}")));
      Path_Buttons_Frame: Ttk_Frame;
      pragma Unreferenced(Path_Buttons_Frame);
      Font: constant String :=
        (if Settings.Monospace_Font then "TkFixedFont" else "TkDefaultFont");
      procedure Create_Permissions_Frame(Name: String; Row: Positive) is
         Frame: constant Ttk_Frame :=
           Create
             (pathName =>
                ".mainframe.paned.previewframe.infoframe." & Name & "frame");
         Check_Button: Ttk_CheckButton := Get_Widget(pathName => ".");
      begin
         Label :=
           Create
             (pathName =>
                ".mainframe.paned.previewframe.infoframe." & Name & "text");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options => "-column 0 -row" & Positive'Image(Row) & " -sticky w");
         Label :=
           Create
             (pathName => ".mainframe.paned.previewframe.infoframe." & Name);
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options => "-column 1 -row" & Positive'Image(Row) & " -sticky w");
         Set_Permission_Buttons_Loop :
         for I in Button_Names'Range loop
            Check_Button :=
              Create
                (pathName =>
                   Frame & "." & To_String(Source => Button_Names(I)),
                 options =>
                   "-text {" & To_String(Source => Button_Texts(I)) &
                   "} -variable " & Name &
                   To_String(Source => Button_Names(I)) &
                   " -command SetPermissions");
            Tcl.Tk.Ada.Pack.Pack
              (Slave => Check_Button, Options => "-anchor w");
         end loop Set_Permission_Buttons_Loop;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Frame,
            Options => "-column 1 -row" & Positive'Image(Row + 1));
      end Create_Permissions_Frame;
   begin
      Preview_Frame := Create(pathName => Paned & ".previewframe");
      Label := Create(pathName => Get_Preview_Frame & ".title");
      Tcl.Tk.Ada.Pack.Pack(Slave => Label);
      Path_Buttons_Frame :=
        Create(pathName => Get_Preview_Frame & ".pathframe");
      Preview_X_Scroll :=
        Create
          (pathName => Get_Preview_Frame & ".scrollx",
           options =>
             "-orient horizontal -command [list " & Get_Preview_Frame &
             ".directorytree xview]");
      Preview_Y_Scroll :=
        Create
          (pathName => Get_Preview_Frame & ".scrolly",
           options =>
             "-orient vertical -command [list " & Get_Preview_Frame &
             ".directorytree yview]");
      Preview_Tree :=
        Create
          (pathName => Get_Preview_Frame & ".directorytree",
           options =>
             "-columns [list name] -xscrollcommand {" & Get_Preview_X_Scroll &
             " set} -yscrollcommand {" & Get_Preview_Y_Scroll &
             " set} -selectmode none ");
      Heading
        (TreeViewWidget => Get_Preview_Tree, Column => "name",
         Options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "{Name}") &
           "} -image {arrow-down} -command {Sort previewname}");
      Column
        (TreeViewWidget => Get_Preview_Tree, Col => "#0",
         Options => "-stretch false -width 50");
      Tag_Bind
        (TreeViewWidget => Get_Preview_Tree, TagName => "itemrow",
         Sequence => "<Double-1>", Script => "GoToDirectory");
      Bind
        (Widgt => Get_Preview_Tree, Sequence => "<Return>",
         Script => "GoToDirectory");
      Preview_Text :=
        Create
          (pathName => Get_Preview_Frame & ".previewtext",
           options =>
             "-wrap char -yscrollcommand {" & Get_Preview_Y_Scroll &
             " set} -font " & Font);
      Tag_Configure
        (TextWidget => Get_Preview_Text, TagName => "boldtag",
         Options => "-font bold");
      Tag_Configure
        (TextWidget => Get_Preview_Text, TagName => "italictag",
         Options => "-font italic");
      Preview_Canvas :=
        Create
          (pathName => Get_Preview_Frame & ".previewcanvas",
           options =>
             "-xscrollcommand {" & Get_Preview_X_Scroll &
             " set} -yscrollcommand {" & Get_Preview_Y_Scroll & " set}");
      Info_Frame := Create(pathName => Get_Preview_Frame & ".infoframe");
      Label := Create(pathName => Get_Info_Frame & ".fullpathtext");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-sticky w");
      Label :=
        Create
          (pathName => Get_Info_Frame & ".fullpath",
           options =>
             "-wraplength " &
             Natural'Image
               (Natural'Value(Winfo_Get(Widgt => Paned, Info => "width")) /
                3));
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 1 -row 0 -sticky w");
      Label := Create(pathName => Get_Info_Frame & ".sizetext");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 0 -row 1 -sticky w");
      Label := Create(pathName => Get_Info_Frame & ".size");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 1 -row 1 -sticky w");
      Label :=
        Create
          (pathName => Get_Info_Frame & ".lastmodifiedtext",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Last modified:}") &
             "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 0 -row 2 -sticky w");
      Label := Create(pathName => Get_Info_Frame & ".lastmodified");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 1 -row 2 -sticky w");
      Label :=
        Create
          (pathName => Get_Info_Frame & ".filetypetext",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{File type:}") & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 0 -row 3 -sticky w");
      Label := Create(pathName => Get_Info_Frame & ".filetype");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 1 -row 3 -sticky w");
      Label :=
        Create
          (pathName => Get_Info_Frame & ".associatedprogramtext",
           options =>
             "-text {" &
             Mc(Interp => Get_Context, Src_String => "{Associated program:}") &
             "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Label, Options => "-column 0 -row 4 -sticky w");
      Button :=
        Create
          (pathName => Get_Info_Frame & ".associatedprogram",
           options => "-command ToggleApplicationsMenu");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-column 1 -row 4 -sticky w");
      Create_Permissions_Frame(Name => "owner", Row => 5);
      Create_Permissions_Frame(Name => "group", Row => 7);
      Create_Permissions_Frame(Name => "others", Row => 9);
      Add_Command
        (Name => "ShowSelected", Ada_Command => Show_Selected_Command'Access);
      Add_Command
        (Name => "ShowPreviewOrInfo",
         Ada_Command => Show_Preview_Or_Info_Command'Access);
      Add_Command
        (Name => "SetPermissions",
         Ada_Command => Set_Permissions_Command'Access);
      Add_Command
        (Name => "GoToDirectory",
         Ada_Command => Go_To_Directory_Command'Access);
      Add
        (Widget => Button,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String =>
                "{Select new associated program with that type of file or directory.}"));
      if Settings.Show_Preview then
         Add
           (Paned => Paned, SubWindow => Get_Preview_Frame,
            Options => "-weight 20");
      end if;
      CreateProgramsMenuUI;
   end Create_Show_Items_Ui;

   procedure Show_Destination is
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => ".mainframe.paned");
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Get_Preview_Frame & ".pathframe");
   begin
      if not Settings.Show_Preview then
         Add
           (Paned => Paned, SubWindow => Get_Preview_Frame,
            Options => "-weight 20");
      end if;
      Unautoscroll(Scroll => Get_Preview_X_Scroll);
      Unautoscroll(Scroll => Get_Preview_Y_Scroll);
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Frame,
         Options => "-after " & Get_Preview_Frame & ".title -fill x");
      configure
        (Widgt => Get_Preview_X_Scroll,
         options => "-command [list " & Get_Preview_Tree & " xview]");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Preview_X_Scroll, Options => "-side bottom -fill x");
      configure
        (Widgt => Get_Preview_Y_Scroll,
         options => "-command [list " & Get_Preview_Tree & " yview]");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Preview_Y_Scroll, Options => "-side right -fill y");
      configure(Widgt => Get_Preview_Tree, options => "-selectmode browse");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Preview_Tree,
         Options => "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Text);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Info_Frame);
      Frame.Name := New_String(Str => Get_Preview_Frame & ".title");
      configure
        (Widgt => Frame,
         options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Destination directory}") &
           "}");
      Destination_Directory := Common.Current_Directory;
      Load_Directory
        (Directory_Name => To_String(Source => Destination_Directory),
         Second => True);
      Update_Directory_List(Clear => True, Frame_Name => "preview");
      Autoscroll(Scroll => Get_Preview_X_Scroll);
      Autoscroll(Scroll => Get_Preview_Y_Scroll);
   end Show_Destination;

   procedure Show_Output is
      Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Get_Preview_Frame & ".title");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => ".mainframe.paned");
   begin
      if not Settings.Show_Preview then
         Add
           (Paned => Paned, SubWindow => Get_Preview_Frame,
            Options => "-weight 20");
      end if;
      Unautoscroll(Scroll => Get_Preview_Y_Scroll);
      configure
        (Widgt => Get_Preview_Y_Scroll,
         options => "-command [list " & Get_Preview_Text & " yview]");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Preview_Y_Scroll, Options => "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Get_Preview_Text,
         Options => "-side top -fill both -expand true");
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_X_Scroll);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Canvas);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Preview_Tree);
      Tcl.Tk.Ada.Pack.Pack_Forget(Slave => Get_Info_Frame);
      configure
        (Widgt => Frame,
         options =>
           "-text {" &
           Mc(Interp => Get_Context, Src_String => "{Command output}") & "}");
      configure(Widgt => Get_Preview_Text, options => "-state normal");
      Delete
        (TextWidget => Get_Preview_Text, StartIndex => "1.0",
         Indexes => "end");
      configure(Widgt => Get_Preview_Text, options => "-state disabled");
      Autoscroll(Scroll => Get_Preview_Y_Scroll);
   end Show_Output;

   procedure Update_Output(Text_To_Append: String) is
   begin
      configure(Get_Preview_Text, "-state normal");
      Insert(Get_Preview_Text, "end", "{" & Text_To_Append & "}");
      configure(Get_Preview_Text, "-state disabled");
      Tcl_Eval(Get_Context, "update");
   end Update_Output;

end ShowItems;
