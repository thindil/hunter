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
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.String_Split;
with CHelper;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with AboutDialog;
with AboutDialog.UI;
with ActivateItems;
with Bookmarks.UI;
with Common;
with CopyItems.UI;
with CreateItems.UI;
with DeleteItems.UI;
with LoadData;
with LoadData.UI;
with MainWindow.Commands;
with Messages.UI;
with Modules;
with MoveItems.UI;
with Preferences; use Preferences;
with Preferences.UI;
with RefreshData;
with RenameItems.UI;
with SearchItems;
with ShowItems; use ShowItems;
with Toolbars;
with Trash;
with UserCommands;
with Utils;

package body MainWindow is

   procedure Create_Main_Window(Directory: String) is
      use Ada.Command_Line;
      use Tcl.Ada;
      use Tcl.Tk.Ada.Image.Photo;
      use Tcl.Tk.Ada.TtkStyle;
      use Tcl.Tk.Ada.Widgets.Menu;
      use Tcl.Tk.Ada.Widgets.Toplevel;
      use Tcl.Tk.Ada.Widgets.TtkEntry;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Widgets.TtkProgressBar;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tk.Ada.Wm;
      use Tcl.Tklib.Ada.Autoscroll;
      use AboutDialog;
      use AboutDialog.UI;
      use Bookmarks.UI;
      use DeleteItems.UI;
      use LoadData.UI;
      use Messages.UI;
      use Modules;
      use MoveItems.UI;
      use Preferences.UI;
      use RefreshData;
      use SearchItems;
      use Toolbars;
      use Trash;

      Interp: constant Tcl.Tcl_Interp := Get_Context;
      Main_Window: constant Tk_Toplevel := Get_Main_Window(Interp => Interp);
      Current_Dir: constant String := Ada.Directories.Current_Directory;
      Main_Frame: constant Ttk_Frame := Create(pathName => ".mainframe");
      Paned: constant Ttk_PanedWindow :=
        Create
          (pathName => ".mainframe.paned", options => "-orient horizontal");
      Directory_Frame: constant Ttk_Frame :=
        Create(pathName => Paned & ".directoryframe");
      Directory_X_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Directory_Frame & ".scrollx",
           options =>
             "-orient horizontal -command [list " & Directory_Frame &
             ".directorytree xview]");
      Directory_Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Directory_Frame & ".scrolly",
           options =>
             "-orient vertical -command [list " & Directory_Frame &
             ".directorytree yview]");
      Directory_Tree: constant Ttk_Tree_View :=
        Create
          (pathName => Directory_Frame & ".directorytree",
           options =>
             "-columns [list name modified size] -xscrollcommand {" &
             Directory_X_Scroll & " set} -yscrollcommand {" &
             Directory_Y_Scroll & " set}");
      Header_Label: constant Ttk_Label :=
        Create(pathName => Main_Frame & ".headerlabel");
      Icons_Names: constant array(1 .. 14) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "emblem-symbolic-link"),
         2 => To_Unbounded_String(Source => "application-x-executable"),
         3 => To_Unbounded_String(Source => "audio-x-generic"),
         4 => To_Unbounded_String(Source => "font-x-generic"),
         5 => To_Unbounded_String(Source => "image-x-generic"),
         6 => To_Unbounded_String(Source => "video-x-generic"),
         7 => To_Unbounded_String(Source => "text-x-script"),
         8 => To_Unbounded_String(Source => "text-html"),
         9 => To_Unbounded_String(Source => "package-x-generic"),
         10 => To_Unbounded_String(Source => "text-x-generic"),
         11 => To_Unbounded_String(Source => "text-x-generic-template"),
         12 => To_Unbounded_String(Source => "folder"),
         13 => To_Unbounded_String(Source => "arrow-down"),
         14 => To_Unbounded_String(Source => "arrow-up"));
      Progress_Bar: constant Ttk_ProgressBar :=
        Create
          (pathName => Main_Frame & ".progressbar",
           options => "-orient horizontal");
      Text_Frame: constant Ttk_Frame :=
        Create(pathName => Main_Frame & ".textframe");
      Text_Entry: constant Ttk_Entry :=
        Create(pathName => Text_Frame & ".textentry");
      Button: Ttk_Button := Create(pathName => Text_Frame & ".closebutton");
      Path_Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => Main_Frame & ".paned.directoryframe.pathframe");
      File_Menu: constant Tk_Menu :=
        Create(pathName => ".filemenu", options => "-tearoff false");
      Buttons_Names: constant array
        (Accelerators_Array'Range) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "actiontoolbar.quitbutton"),
         2 => To_Unbounded_String(Source => "actiontoolbar.bookmarksbutton"),
         3 => To_Unbounded_String(Source => "actiontoolbar.searchbutton"),
         4 => To_Unbounded_String(Source => "actiontoolbar.newbutton"),
         5 => To_Unbounded_String(Source => "actiontoolbar.deletebutton"),
         6 => To_Unbounded_String(Source => "actiontoolbar.aboutbutton"),
         7 => To_Unbounded_String(Source => "itemtoolbar.openbutton"),
         8 => To_Unbounded_String(Source => "actiontoolbar.selectbutton"),
         9 => To_Unbounded_String(Source => "actiontoolbar.renamebutton"),
         10 => To_Unbounded_String(Source => "actiontoolbar.copybutton"),
         11 => To_Unbounded_String(Source => "actiontoolbar.movebutton"),
         12 => To_Unbounded_String(Source => "actiontoolbar.optionsbutton"),
         13 => To_Unbounded_String(Source => "itemtoolbar.openwithbutton"),
         14 => To_Unbounded_String(Source => "itemtoolbar.infobutton"),
         15 => To_Unbounded_String(Source => "itemtoolbar.previewbutton"),
         16 => To_Unbounded_String(Source => "itemtoolbar.addbutton"),
         17 => To_Unbounded_String(Source => "itemtoolbar.deletebutton"),
         18 => To_Unbounded_String(Source => "itemtoolbar.runbutton"),
         19 => Null_Unbounded_String,
         20 => To_Unbounded_String(Source => "actiontoolbar.userbutton"));
      --## rule off POSITIONAL_ASSOCIATIONS
      pragma Unreferenced(Progress_Bar, Header_Label, File_Menu);
      --## rule on POSITIONAL_ASSOCIATIONS
   begin
      Autoscroll(Scroll => Directory_Y_Scroll);
      Autoscroll(Scroll => Directory_X_Scroll);
      MainWindow.Commands.Add_Commands;
      UserCommands.AddCommands;
      CreateSearchUI;
      Set_Directory(Directory => Containing_Directory(Name => Command_Name));
      -- Load the program Tk themes
      if Settings.Ui_Theme = To_Unbounded_String(Source => "hunter-light") then
         Tcl_EvalFile
           (interp => Interp,
            fileName => "../share/hunter/themes/light/breeze.tcl");
      elsif Settings.Ui_Theme =
        To_Unbounded_String(Source => "hunter-dark") then
         Tcl_EvalFile
           (interp => Interp,
            fileName => "../share/hunter/themes/dark/breeze-dark.tcl");
      end if;
      if Index
          (Source => Theme_Names,
           Pattern => To_String(Source => Settings.Ui_Theme)) =
        0 then
         Settings.Ui_Theme := To_Unbounded_String(Source => "hunter-light");
         Tcl_EvalFile
           (interp => Interp,
            fileName => "../share/hunter/themes/light/breeze.tcl");
      end if;
      Theme_Use(ThemeName => To_String(Source => Settings.Ui_Theme));
      -- Load translations
      Mc_Load(DirName => "../share/hunter/translations", Interp => Interp);
      Set_About_Dialog_Information(Interp => Interp);
      -- Set the program images
      Load_Images_Block :
      declare
         Program_Image: Tk_Photo :=
           Create
             (pathName => "ok",
              options =>
                "-file {../share/hunter/images/ok.svg} -format {svg -scaletoheight" &
                Natural'Image(Settings.Toolbars_Size) & "}");
         Hunter_Initialization_Error: exception;
      begin
         if Widget_Image(Win => Program_Image) /= "ok" then
            raise Hunter_Initialization_Error with "Can't load ok.svg image";
         end if;
         Load_Images_Loop :
         for Icon_Name of Icons_Names loop
            Program_Image :=
              Create
                (pathName => To_String(Source => Icon_Name),
                 options =>
                   "-file {../share/hunter/images/" &
                   To_String(Source => Icon_Name) &
                   ".svg} -format ""svg -scaletoheight [expr {[font metrics DefaultFont -linespace]}]""");
            if Widget_Image(Win => Program_Image) /=
              To_String(Source => Icon_Name) then
               raise Hunter_Initialization_Error
                 with "Can't load " & To_String(Source => Icon_Name) &
                 ".svg image";
            end if;
         end loop Load_Images_Loop;
      end Load_Images_Block;
      Wm_Set
        (Widgt => Main_Window, Action => "geometry",
         Options =>
           Trim
             (Source => Positive'Image(Settings.Window_Width), Side => Both) &
           "x" &
           Trim
             (Source => Positive'Image(Settings.Window_Height), Side => Both) &
           "+0+0");
      Set_Program_Icon_Block :
      declare
         use CHelper;

         Icon_Name: constant String :=
           (if
              Ada.Directories.Exists
                (Name =>
                   Value(Name => "APPDIR", Default => "") &
                   "/usr/share/doc/hunter")
            then Value(Name => "APPDIR", Default => "") & "/hunter-icon.png"
            else Containing_Directory(Name => Current_Dir) &
              "/others/hunter-icon.png");
         Icon: constant Tk_Photo :=
           Create
             (pathName => "logo", options => "-file """ & Icon_Name & """");
      begin
         Wm_Set
           (Widgt => Main_Window, Action => "iconphoto",
            Options => "-default " & Icon.Name);
      end Set_Program_Icon_Block;
      Add_Accelerators_Loop :
      for I in Buttons_Names'Range loop
         if Buttons_Names(I) /= Null_Unbounded_String then
            Bind_To_Main_Window
              (Interp => Interp,
               Sequence => "<" & To_String(Source => Accelerators(I)) & ">",
               Script =>
                 "{InvokeButton .mainframe.toolbars." &
                 To_String(Source => Buttons_Names(I)) & "}");
         end if;
      end loop Add_Accelerators_Loop;
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<Escape>", Script => "{HideWidget}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Main_Frame, Options => "-sticky nwse");
      Tcl.Tk.Ada.Grid.Row_Configure
        (Master => Main_Window, Slave => Main_Frame, Options => "-weight 1");
      Tcl.Tk.Ada.Grid.Column_Configure
        (Master => Main_Window, Slave => Main_Frame, Options => "-weight 1");
      CreateActionToolbar;
      Create_Bookmark_Menu(Create_New => True);
      CreateItemToolbar;
      Create_Messages_Ui;
      ActivateItems.Add_Commands;
      CreatePreferencesUI;
      Create_Delete_Ui;
      RenameItems.UI.AddCommands;
      CreateItems.UI.Add_Commands;
      CopyItems.UI.Create_Copy_Ui;
      CreateMoveUI;
      CreateTrash;
      Create_About_Ui;
      Load_Modules(Interpreter => Get_Context);
      SetToolbars;
      Add(Paned => Paned, SubWindow => Directory_Frame);
      Bind
        (Widgt => Path_Buttons_Frame, Sequence => "<Configure>",
         Script => "{ArrangePath %W %w}");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Path_Buttons_Frame, Options => "-side top -fill x");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Directory_X_Scroll, Options => "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Directory_Y_Scroll, Options => "-side right -fill y");
      Heading
        (TreeViewWidget => Directory_Tree, Column => "name",
         Options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "Name") &
           "} -image arrow-down -command {Sort name}");
      Set_Directory(Directory => Current_Dir);
      Heading
        (TreeViewWidget => Directory_Tree, Column => "modified",
         Options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "Modified") &
           "} -command {Sort modified}");
      Heading
        (TreeViewWidget => Directory_Tree, Column => "size",
         Options =>
           "-text {" & Mc(Interp => Get_Context, Src_String => "Size") &
           "} -command {Sort size}");
      if not Settings.Show_Last_Modified then
         configure
           (Widgt => Directory_Tree,
            options => "-displaycolumns [list name size]");
      end if;
      Column
        (TreeViewWidget => Directory_Tree, Col => "#0",
         Options => "-stretch false -width 50");
      Column
        (TreeViewWidget => Directory_Tree, Col => "modified",
         Options => "-stretch false -width 150");
      Column
        (TreeViewWidget => Directory_Tree, Col => "size",
         Options => "-stretch false -width 75");
      Tag_Bind
        (TreeViewWidget => Directory_Tree, TagName => "itemrow",
         Sequence => "<Double-1>", Script => "ActivateItem");
      Bind
        (Widgt => Directory_Tree, Sequence => "<Return>",
         Script => "ActivateItem");
      Bind
        (Widgt => Directory_Tree, Sequence => "<<TreeviewSelect>>",
         Script => "ShowSelected");
      Bind
        (Widgt => Directory_Tree, Sequence => "<3>",
         Script => "{ShowFileMenu %X %Y}");
      Tcl.Tk.Ada.Pack.Pack
        (Slave => Directory_Tree,
         Options => "-side top -fill both -expand true");
      if Settings.Toolbars_On_Top then
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Paned, Options => "-column 0 -row 3 -sticky nswe");
      else
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Paned, Options => "-column 1 -row 3 -sticky nswe");
      end if;
      Row_Configure
        (Master => Main_Frame, Slave => Paned, Options => "-weight 1");
      Column_Configure
        (Master => Main_Frame, Slave => Paned, Options => "-weight 1");
      configure
        (Widgt => Button,
         options =>
           "-image dialog-cancelicon -style Toolbutton -command HideWidget");
      Add
        (Widget => Button,
         Message =>
           Mc
             (Interp => Get_Context,
              Src_String => "{Hide entry without entering any changes}"));
      Tcl.Tk.Ada.Grid.Grid(Slave => Button);
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Text_Entry, Options => "-column 1 -row 0 -sticky we");
      Button :=
        Create
          (pathName => Text_Frame & ".okbutton",
           options => "-image ok -style Toolbutton");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-column 2 -row 0");
      Bind
        (Widgt => Text_Entry, Sequence => "<Return>",
         Script => "{.mainframe.textframe.okbutton invoke}");
      Column_Configure
        (Master => Text_Frame, Slave => Text_Entry, Options => "-weight 1");
      if Ada.Directories.Exists(Name => Directory) then
         Common.Current_Directory := To_Unbounded_String(Source => Directory);
      else
         Common.Current_Directory :=
           To_Unbounded_String(Source => Value(Name => "HOME"));
         if not Ada.Directories.Exists
             (Name => To_String(Source => Common.Current_Directory)) then
            Common.Current_Directory := To_Unbounded_String(Source => "/");
         end if;
      end if;
      Load_Directory(Directory_Name => To_String(Source => Common.Current_Directory));
      StartTimer(Path => To_String(Source => Common.Current_Directory));
      Update_Directory_List(Clear => True);
      Execute_Modules
        (Interpreter => Get_Context, State => ON_ENTER,
         Arguments => "{" & To_String(Source => Common.Current_Directory) & "}");
      CreateShowItemsUI;
      SashPos
        (Paned => Paned, Index => "0",
         NewPos =>
           Positive'Image(Positive(Float(Settings.Window_Width) / 2.5)));
   end Create_Main_Window;

   procedure Update_Directory_List
     (Clear: Boolean := False; Frame_Name: String := "directory") is
      use Ada.Characters.Latin_1;
      use GNAT.String_Split;
      use Tcl.Tk.Ada.Winfo;
      use Common;
      use LoadData;
      use Utils;

      Path_Command, Path_Shortcut: Unbounded_String;
      Size_String, Item_Index, Selected_Index, Path, Time_String, Shortcut,
      Tooltip_Text, Button_Label: Unbounded_String := Null_Unbounded_String;
      Directory_Tree: constant Ttk_Tree_View :=
        Get_Widget
          (pathName =>
             ".mainframe.paned." & Frame_Name & "frame.directorytree");
      Path_Buttons_Frame: constant Ttk_Frame :=
        Get_Widget
          (pathName => ".mainframe.paned." & Frame_Name & "frame.pathframe");
      Tokens: Slice_Set; --## rule line off IMPROPER_INITIALIZATION
      Path_Button: Ttk_Button := Get_Widget(pathName => ".mainframe");
      Row, Width, Column: Natural := 0;
      List: Items_Container.Vector;
   begin
      if Frame_Name = "directory" then
         List := Items_List;
         Path_Command :=
           (if New_Action not in SHOWTRASH | DELETETRASH then
              To_Unbounded_String(Source => "GoToBookmark")
            else To_Unbounded_String(Source => "GoToTrash"));
         Path_Shortcut := To_Unbounded_String(Source => "Alt");
      else
         List := Second_Items_List;
         Path_Command := To_Unbounded_String(Source => "GoToDirectory");
         Path_Shortcut := To_Unbounded_String(Source => "Control");
      end if;
      if Clear then
         Arrange_Items_Loop :
         for I in List.First_Index .. List.Last_Index loop
            if Exists
                (TreeViewWidget => Directory_Tree, Item => Positive'Image(I)) =
              "1" then
               Move
                 (TreeViewWidget => Directory_Tree, Item => Positive'Image(I),
                  Parent => "{}", Index => Natural'Image(I - 1));
            end if;
         end loop Arrange_Items_Loop;
         Delete
           (TreeViewWidget => Directory_Tree,
            ItemsList =>
              "[" & Widget_Image(Win => Directory_Tree) & " children {} ]");
         Add_Items_Loop :
         for I in List.First_Index .. List.Last_Index loop
            case List(I).Size is
               when -2 =>
                  Size_String := To_Unbounded_String(Source => "->");
               when -1 =>
                  Size_String :=
                    To_Unbounded_String
                      (Source =>
                         Mc(Interp => Get_Context, Src_String => "unknown"));
               when others =>
                  if List(I).Is_Directory then
                     if Settings.Show_Hidden then
                        Size_String :=
                          To_Unbounded_String
                            (Source =>
                               Item_Size'Image
                                 (List(I).Size +
                                  Item_Size(List(I).Hidden_Items)));
                     else
                        Size_String :=
                          To_Unbounded_String
                            (Source => Item_Size'Image(List(I).Size));
                     end if;
                  else
                     Size_String :=
                       To_Unbounded_String
                         (Source =>
                            Count_File_Size(Size => File_Size(List(I).Size)));
                  end if;
            end case;
            Set_Modified_Block :
            begin
               Time_String :=
                 To_Unbounded_String
                   (Source =>
                      Ada.Calendar.Formatting.Image(Date => List(I).Modified));
            exception
               when Ada.Calendar.Time_Error =>
                  Time_String :=
                    To_Unbounded_String
                      (Source =>
                         Mc(Interp => Get_Context, Src_String => "unknown"));
            end Set_Modified_Block;
            Item_Index :=
              To_Unbounded_String
                (Source =>
                   Insert
                     (TreeViewWidget => Directory_Tree,
                      Options =>
                        "{} end -id" & Positive'Image(I) & " -values [list {" &
                        To_String(Source => List(I).Name) & "} {" &
                        To_String(Source => Time_String) & "} {" &
                        To_String(Source => Size_String) & "}] -image {" &
                        To_String(Source => List(I).Image) &
                        "} -tags [list itemrow]"));
            if not Settings.Show_Hidden and then List(I).Is_Hidden then
               Detach
                 (TreeViewWidget => Directory_Tree,
                  ItemsList => To_String(Source => Item_Index));
            elsif Selected_Index = Null_Unbounded_String or
              Current_Selected = List(I).Path then
               Selected_Index :=
                 To_Unbounded_String(Source => Positive'Image(I));
            end if;
         end loop Add_Items_Loop;
         if Winfo_Get(Widgt => Path_Buttons_Frame, Info => "ismapped") =
           "1" then
            -- Remove old path buttons
            Create
              (S => Tokens, From => Grid_Slaves(Master => Path_Buttons_Frame),
               Separators => " ");
            if Slice(S => Tokens, Index => 1) /= "" then
               Remove_Old_Path_Buttons_Loop :
               for I in reverse 1 .. Slice_Count(S => Tokens) loop
                  Path_Button :=
                    Get_Widget(pathName => Slice(S => Tokens, Index => I));
                  case I is
                     when 1 =>
                        Shortcut := Path_Shortcut & "-r";
                     when 2 =>
                        Shortcut := Path_Shortcut & "-u";
                     when 3 .. 10 =>
                        Shortcut :=
                          Path_Shortcut & "-KP_" &
                          Slice_Number'Image(I - 2)(2);
                     when others =>
                        null;
                  end case;
                  Unbind_From_Main_Window
                    (Interp => Path_Button.Interp,
                     Sequence => "<" & To_String(Source => Shortcut) & ">");
                  Grid_Forget(Slave => Path_Button);
                  Destroy(Widgt => Path_Button);
               end loop Remove_Old_Path_Buttons_Loop;
            end if;
            -- Add new path buttons
            if Frame_Name = "directory"
              and then New_Action not in SHOWTRASH | DELETETRASH then
               Create
                 (S => Tokens, From => To_String(Source => Common.Current_Directory),
                  Separators => "/");
            else
               Create
                 (S => Tokens,
                  From => To_String(Source => DestinationDirectory),
                  Separators => "/");
            end if;
            Add_Path_Buttons_Loop :
            for I in 1 .. Slice_Count(S => Tokens) loop
               if Slice(S => Tokens, Index => I) = "" and I > 1 then
                  goto End_Of_Loop;
               end if;
               if I = 1 then
                  if New_Action = SHOWTRASH then
                     Path_Button :=
                       Create
                         (pathName => Path_Buttons_Frame & ".button1",
                          options =>
                            "-text {" &
                            Mc(Interp => Get_Context,
                               Src_String => "{Trash}") &
                            "} -command {ShowTrash}");
                     Path :=
                       To_Unbounded_String
                         (Source =>
                            Value(Name => "HOME") &
                            "/.local/share/Trash/files/");
                  else
                     Path_Button :=
                       Create
                         (pathName => Path_Buttons_Frame & ".button1",
                          options =>
                            "-text {/} -command {" &
                            To_String(Source => Path_Command) & " {/}}");
                     Path := To_Unbounded_String(Source => "/");
                  end if;
               else
                  Append
                    (Source => Path,
                     New_Item => Slice(S => Tokens, Index => I) & "/");
                  if New_Action = SHOWTRASH and I = 2 then
                     Set_Path_Button_Block :
                     declare
                        use Ada.Text_IO;

                        File_Info: File_Type;
                        File_Line: Unbounded_String := Null_Unbounded_String;
                     begin
                        Open
                          (File => File_Info, Mode => In_File,
                           Name =>
                             Value(Name => "HOME") &
                             "/.local/share/Trash/info/" &
                             Slice(S => Tokens, Index => I) & ".trashinfo");
                        Skip_Line(File => File_Info);
                        Read_File_Path_Loop :
                        for J in 1 .. 2 loop
                           File_Line :=
                             To_Unbounded_String
                               (Source => Get_Line(File => File_Info));
                           if Slice(Source => File_Line, Low => 1, High => 4) =
                             "Path" then
                              Button_Label :=
                                To_Unbounded_String
                                  (Source =>
                                     Simple_Name
                                       (Name =>
                                          Slice
                                            (Source => File_Line, Low => 6,
                                             High =>
                                               Length(Source => File_Line))));
                           end if;
                        end loop Read_File_Path_Loop;
                        Close(File => File_Info);
                     end Set_Path_Button_Block;
                  else
                     Button_Label :=
                       To_Unbounded_String
                         (Source => Slice(S => Tokens, Index => I));
                  end if;
                  if Length(Source => Button_Label) > 7 then
                     Button_Label :=
                       Unbounded_Slice
                         (Source => Button_Label, Low => 1, High => 7) &
                       "...";
                  end if;
                  Path_Button :=
                    Create
                      (pathName =>
                         Widget_Image(Win => Path_Buttons_Frame) & ".button" &
                         Trim(Source => Slice_Number'Image(I), Side => Both),
                       options =>
                         "-text {" & To_String(Source => Button_Label) &
                         "} -command {" & To_String(Source => Path_Command) &
                         " {" & To_String(Source => Path) & "}}");
               end if;
               if I + 11 > Slice_Count(S => Tokens) then
                  if I = Slice_Count(S => Tokens) then
                     Shortcut := Path_Shortcut & "-r";
                     Tooltip_Text :=
                       Mc
                         (Interp => Get_Context,
                          Src_String => "{Reload the current directory:}") &
                       LF & To_String(Source => Path) & LF & "\[" & Shortcut &
                       "\]";
                  elsif I = Slice_Count(S => Tokens) - 1 then
                     Shortcut := Path_Shortcut & "-u";
                     Tooltip_Text :=
                       Mc
                         (Interp => Get_Context,
                          Src_String => "{Go to directory:}") &
                       LF & To_String(Source => Path) & LF & "\[" & Shortcut &
                       "\]";
                  else
                     Shortcut :=
                       Path_Shortcut & "-KP_" &
                       Slice_Number'Image(Slice_Count(S => Tokens) - I - 1)(2);
                     Tooltip_Text :=
                       Mc
                         (Interp => Get_Context,
                          Src_String => "{Go to directory:}") &
                       LF & To_String(Source => Path) & LF & "\[" & Shortcut &
                       "\]";
                  end if;
                  Bind_To_Main_Window
                    (Interp => Path_Button.Interp,
                     Sequence => "<" & To_String(Source => Shortcut) & ">",
                     Script => "{" & Path_Button & " invoke}");
                  Add
                    (Widget => Path_Button,
                     Message => To_String(Source => Tooltip_Text));
               end if;
               Width :=
                 Width +
                 Positive'Value
                   (Winfo_Get(Widgt => Path_Button, Info => "reqwidth"));
               if Width >
                 Positive'Value
                   (Winfo_Get
                      (Widgt => Path_Buttons_Frame, Info => "width")) then
                  Row := Row + 1;
                  Width := 0;
                  Column := 0;
               end if;
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Path_Button,
                  Options =>
                    "-row" & Natural'Image(Row) & " -column" &
                    Natural'Image(Column));
               Column := Column + 1;
               <<End_Of_Loop>>
            end loop Add_Path_Buttons_Loop;
         end if;
      else
         Rearrange_Items_Loop :
         for I in List.First_Index .. List.Last_Index loop
            if (Settings.Show_Hidden and List(I).Is_Hidden) or
              not List(I).Is_Hidden then
               Move
                 (TreeViewWidget => Directory_Tree, Item => Positive'Image(I),
                  Parent => "{}", Index => Positive'Image(I));
               if Selected_Index = Null_Unbounded_String or
                 Current_Selected = List(I).Path then
                  Selected_Index :=
                    To_Unbounded_String(Source => Positive'Image(I));
               end if;
            end if;
         end loop Rearrange_Items_Loop;
      end if;
      if List.Is_Empty then
         Selection_Set(TreeViewWidget => Directory_Tree, Items => "{}");
      else
         if Length(Source => Selected_Index) > 0 and
           cget(Widgt => Directory_Tree, option => "-selectmode") in "browse" |
               "extended" then
            Selection_Set
              (TreeViewWidget => Directory_Tree,
               Items => "[list " & To_String(Source => Selected_Index) & "]");
            Tcl.Tk.Ada.Widgets.Focus(Widgt => Directory_Tree);
            Tcl.Tk.Ada.Widgets.TtkTreeView.Focus
              (TreeViewWidget => Directory_Tree,
               Item => To_String(Source => Selected_Index));
         else
            Selection_Set(TreeViewWidget => Directory_Tree, Items => "{}");
         end if;
      end if;
   end Update_Directory_List;

end MainWindow;
