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
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CHelper; use CHelper;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid; use Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Image.Photo; use Tcl.Tk.Ada.Image.Photo;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with AboutDialog; use AboutDialog;
with ActivateItems; use ActivateItems;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with MainWindow.Commands; use MainWindow.Commands;
with Messages; use Messages;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with RenameItems; use RenameItems;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;
with Toolbars; use Toolbars;
with Trash; use Trash;
with Utils; use Utils;

package body MainWindow is

   procedure CreateMainWindow(Directory: String) is
      MainWindow: Tk_Toplevel;
      Interp: constant Tcl.Tcl_Interp := Get_Context;
      CurrentDir: constant String := Current_Directory;
      MainFrame: constant Ttk_Frame := Create(".mainframe");
      Paned: constant Ttk_PanedWindow :=
        Create(".mainframe.paned", "-orient horizontal");
      DirectoryFrame: constant Ttk_Frame :=
        Create(Widget_Image(Paned) & ".directoryframe");
      DirectoryXScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(DirectoryFrame) & ".scrollx",
           "-orient horizontal -command [list " &
           Widget_Image(DirectoryFrame) & ".directorytree xview]");
      DirectoryYScroll: constant Ttk_Scrollbar :=
        Create
          (Widget_Image(DirectoryFrame) & ".scrolly",
           "-orient vertical -command [list " & Widget_Image(DirectoryFrame) &
           ".directorytree yview]");
      DirectoryTree: constant Ttk_Tree_View :=
        Create
          (Widget_Image(DirectoryFrame) & ".directorytree",
           "-columns [list name modified size] -xscrollcommand """ &
           Widget_Image(DirectoryXScroll) & " set"" -yscrollcommand """ &
           Widget_Image(DirectoryYScroll) & " set""");
      HeaderLabel: constant Ttk_Label := Create(".mainframe.headerlabel");
      IconName: Unbounded_String;
      Icon, Image: Tk_Photo;
      IconsNames: constant array(1 .. 15) of Unbounded_String :=
        (To_Unbounded_String("emblem-symbolic-link"),
         To_Unbounded_String("application-x-executable"),
         To_Unbounded_String("audio-x-generic"),
         To_Unbounded_String("font-x-generic"),
         To_Unbounded_String("image-x-generic"),
         To_Unbounded_String("video-x-generic"),
         To_Unbounded_String("text-x-generic"),
         To_Unbounded_String("text-html"),
         To_Unbounded_String("package-x-generic"),
         To_Unbounded_String("text-x-generic"),
         To_Unbounded_String("text-x-generic-template"),
         To_Unbounded_String("folder"), To_Unbounded_String("arrow-down"),
         To_Unbounded_String("arrow-up"), To_Unbounded_String("ok"));
      ProgressBar: constant Ttk_ProgressBar :=
        Create(".mainframe.progressbar", "-orient horizontal");
      TextFrame: constant Ttk_Frame := Create(".mainframe.textframe");
      TextEntry: constant Ttk_Entry :=
        Create(".mainframe.textframe.textentry");
      Button: Ttk_Button;
      PathButtonsFrame: constant Ttk_Frame :=
        Create(".mainframe.paned.directoryframe.pathframe");
      FileMenu: constant Tk_Menu := Create(".filemenu", "-tearoff false");
      pragma Unreferenced(Image, ProgressBar, HeaderLabel, FileMenu);
   begin
      Autoscroll(DirectoryYScroll);
      Autoscroll(DirectoryXScroll);
      AddCommands;
      CreateSearchUI;
      Set_Directory(Containing_Directory(Command_Name));
      -- Load the program Tk themes
      if Settings.UITheme = To_Unbounded_String("light") then
         Tcl_EvalFile
            (Get_Context, "../share/hunter/themes/light/breeze.tcl");
      elsif Settings.UITheme = To_Unbounded_String("dark") then
         Tcl_EvalFile
            (Get_Context, "../share/hunter/themes/dark/breeze-dark.tcl");
      end if;
      Theme_Use(To_String(Settings.UITheme));
      -- Load translations
      Mc_Load("../share/hunter/translations", Get_Context);
      -- Set the program images
      for IconName of IconsNames loop
         Image :=
           Create
             (To_String(IconName),
              "-file ""../share/hunter/images/" & To_String(IconName) &
              ".png""");
      end loop;
      if Ada.Directories.Exists
          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
         IconName :=
           To_Unbounded_String(Value("APPDIR", "") & "/hunter-icon.png");
      else
         IconName :=
           To_Unbounded_String
             (Containing_Directory(Current_Directory) &
              "/others/hunter-icon.png");
      end if;
      MainWindow := Get_Main_Window(Interp);
      Wm_Set
        (MainWindow, "geometry",
         Trim(Positive'Image(Settings.WindowWidth), Both) & "x" &
         Trim(Positive'Image(Settings.WindowHeight), Both) & "+0+0");
      Icon := Create("logo", "-file """ & To_String(IconName) & """");
      Wm_Set(MainWindow, "iconphoto", "-default " & Icon.Name);
      Bind_To_Main_Window(Interp, "<Control-q>", "{exit}");
      Bind_To_Main_Window
        (Interp, "<Alt-h>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.bookmarksbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-f>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.searchbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-n>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.newbutton}");
      Bind_To_Main_Window
        (Interp, "<Control-Delete>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.deletebutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-a>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.aboutbutton}");
      Bind_To_Main_Window(Interp, "<Escape>", "{HideWidget}");
      Bind_To_Main_Window
        (Interp, "<Alt-o>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.openbutton}");
      Bind_To_Main_Window
        (Interp, "<Control-a>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.selectbutton}");
      Bind_To_Main_Window
        (Interp, "<Control-r>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.renamebutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-c>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.copybutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-m>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.movebutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-p>",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.optionsbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-w>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.openwithbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-i>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.infobutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-v>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.previewbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-b>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.addbutton}");
      Bind_To_Main_Window
        (Interp, "<Alt-r>",
         "{InvokeButton .mainframe.toolbars.itemtoolbar.deletebutton}");
      Tcl.Tk.Ada.Pack.Pack(MainFrame, "-expand true -fill both");
      CreateActionToolbar;
      CreateBookmarkMenu(True);
      CreateItemToolbar;
      SetToolbars;
      CreateMessagesUI;
      CreateActivateUI;
      CreatePreferencesUI;
      CreateDeleteUI;
      CreateRenameUI;
      CreateCreateUI;
      CreateCopyUI;
      CreateMoveUI;
      CreateTrashUI;
      CreateAboutUI;
      Add(Paned, DirectoryFrame);
      Bind(PathButtonsFrame, "<Configure>", "{ArrangePath %W %w}");
      Tcl.Tk.Ada.Pack.Pack(PathButtonsFrame, "-side top -fill x");
      Tcl.Tk.Ada.Pack.Pack(DirectoryXScroll, "-side bottom -fill x");
      Tcl.Tk.Ada.Pack.Pack(DirectoryYScroll, "-side right -fill y");
      Heading
        (DirectoryTree, "name",
         "-text {" & Mc(Get_Context, "Name") &
         "} -image arrow-down -command {Sort name}");
      Set_Directory(CurrentDir);
      Heading
        (DirectoryTree, "modified",
         "-text {" & Mc(Get_Context, "Modified") &
         "} -command {Sort modified}");
      Heading
        (DirectoryTree, "size",
         "-text {" & Mc(Get_Context, "Size") & "} -command {Sort size}");
      if not Settings.ShowLastModified then
         configure(DirectoryTree, "-displaycolumns [list name size]");
      end if;
      Column(DirectoryTree, "#0", "-stretch false -width 50");
      Column(DirectoryTree, "modified", "-stretch false");
      Column(DirectoryTree, "size", "-stretch false");
      Bind(DirectoryTree, "<Double-1>", "ActivateItem");
      Bind(DirectoryTree, "<Return>", "ActivateItem");
      Bind(DirectoryTree, "<<TreeviewSelect>>", "ShowSelected");
      Bind(DirectoryTree, "<3>", "{ShowFileMenu %X %Y}");
      Tcl.Tk.Ada.Pack.Pack(DirectoryTree, "-side top -fill both -expand true");
      if not Settings.ToolbarsOnTop then
         Tcl.Tk.Ada.Grid.Grid(Paned, "-column 1 -row 3 -sticky nswe");
      else
         Tcl.Tk.Ada.Grid.Grid(Paned, "-column 0 -row 3 -sticky nswe");
      end if;
      Row_Configure(MainFrame, Paned, "-weight 1");
      Column_Configure(MainFrame, Paned, "-weight 1");
      Button :=
        Create
          (".mainframe.textframe.closebutton",
           "-image dialog-cancelicon -style Toolbutton -command HideWidget");
      Add
        (Button, Mc(Get_Context, "{Hide entry without entering any changes}"));
      Tcl.Tk.Ada.Grid.Grid(Button);
      Tcl.Tk.Ada.Grid.Grid(TextEntry, "-column 1 -row 0 -sticky we");
      Button :=
        Create(".mainframe.textframe.okbutton", "-image ok -style Toolbutton");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 2 -row 0");
      Bind(TextEntry, "<Return>", "{.mainframe.textframe.okbutton invoke}");
      Column_Configure(TextFrame, TextEntry, "-weight 1");
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists(To_String(CurrentDirectory)) then
            CurrentDirectory := To_Unbounded_String("/");
         end if;
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      StartTimer(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      CreateShowItemsUI;
   end CreateMainWindow;

   procedure UpdateDirectoryList
     (Clear: Boolean := False; FrameName: String := "directory") is
      SizeString, ItemIndex, SelectedIndex, Path, TimeString, PathCommand,
      PathShortcut, Shortcut, Tooltip, ButtonLabel: Unbounded_String;
      DirectoryTree: Ttk_Tree_View;
      PathButtonsFrame: Ttk_Frame;
      Tokens: Slice_Set;
      PathButton: Ttk_Button;
      Row, Width, Column: Natural := 0;
      List: Items_Container.Vector;
   begin
      DirectoryTree.Interp := Get_Context;
      DirectoryTree.Name :=
        New_String(".mainframe.paned." & FrameName & "frame.directorytree");
      PathButtonsFrame.Interp := Get_Context;
      PathButtonsFrame.Name :=
        New_String(".mainframe.paned." & FrameName & "frame.pathframe");
      if FrameName = "directory" then
         List := ItemsList;
         PathCommand := To_Unbounded_String("GoToBookmark");
         PathShortcut := To_Unbounded_String("Alt");
      else
         List := SecondItemsList;
         PathCommand := To_Unbounded_String("GoToDirectory");
         PathShortcut := To_Unbounded_String("Control");
      end if;
      if Clear then
         for I in List.First_Index .. List.Last_Index loop
            if Exists(DirectoryTree, Positive'Image(I)) = "1" then
               Move
                 (DirectoryTree, Positive'Image(I), "{}",
                  Natural'Image(I - 1));
            end if;
         end loop;
         Delete
           (DirectoryTree,
            "[" & Widget_Image(DirectoryTree) & " children {} ]");
         for I in List.First_Index .. List.Last_Index loop
            case List(I).Size is
               when -2 =>
                  SizeString := To_Unbounded_String("->");
               when -1 =>
                  SizeString := To_Unbounded_String("unknown");
               when others =>
                  if not List(I).IsDirectory then
                     SizeString :=
                       To_Unbounded_String
                         (CountFileSize(File_Size(List(I).Size)));
                  else
                     if Settings.ShowHidden then
                        SizeString :=
                          To_Unbounded_String
                            (Item_Size'Image
                               (List(I).Size +
                                Item_Size(List(I).HiddenItems)));
                     else
                        SizeString :=
                          To_Unbounded_String(Item_Size'Image(List(I).Size));
                     end if;
                  end if;
            end case;
            begin
               TimeString :=
                 To_Unbounded_String
                   (Ada.Calendar.Formatting.Image(List(I).Modified));
            exception
               when Ada.Calendar.Time_Error =>
                  TimeString := To_Unbounded_String("unknown");
            end;
            ItemIndex :=
              To_Unbounded_String
                (Insert
                   (DirectoryTree,
                    "{} end -id" & Positive'Image(I) & " -values [list {" &
                    To_String(List(I).Name) & "} {" & To_String(TimeString) &
                    "} {" & To_String(SizeString) & "}] -image {" &
                    To_String(List(I).Image) & "}"));
            if not Settings.ShowHidden and then List(I).IsHidden then
               Detach(DirectoryTree, To_String(ItemIndex));
            elsif SelectedIndex = Null_Unbounded_String or
              CurrentSelected = CurrentDirectory & "/" & List(I).Name then
               SelectedIndex := To_Unbounded_String(Positive'Image(I));
            end if;
         end loop;
         if Winfo_Get(PathButtonsFrame, "ismapped") = "1" then
            -- Remove old path buttons
            Create(Tokens, Grid_Slaves(PathButtonsFrame), " ");
            if Slice(Tokens, 1) /= "" then
               for I in reverse 1 .. Slice_Count(Tokens) loop
                  PathButton.Interp := PathButtonsFrame.Interp;
                  PathButton.Name := New_String(Slice(Tokens, I));
                  if I = 1 then
                     Shortcut := PathShortcut & "-r";
                  elsif I = 2 then
                     Shortcut := PathShortcut & "-u";
                  elsif I < 11 then
                     Shortcut :=
                       PathShortcut & "-KP_" & Slice_Number'Image(I - 2)(2);
                  end if;
                  Unbind_From_Main_Window
                    (PathButton.Interp, "<" & To_String(Shortcut) & ">");
                  Grid_Forget(PathButton);
                  Destroy(PathButton);
               end loop;
            end if;
            -- Add new path buttons
            if FrameName = "directory"
              and then NewAction not in SHOWTRASH | DELETETRASH then
               Create(Tokens, To_String(CurrentDirectory), "/");
            else
               Create(Tokens, To_String(DestinationDirectory), "/");
            end if;
            for I in 1 .. Slice_Count(Tokens) loop
               if Slice(Tokens, I) = "" and I > 1 then
                  goto End_Of_Loop;
               end if;
               if I = 1 then
                  PathButton :=
                    Create
                      (Widget_Image(PathButtonsFrame) & ".button1",
                       "-text {/} -command {" & To_String(PathCommand) &
                       " {/}}");
                  Path := To_Unbounded_String("/");
               else
                  Append(Path, Slice(Tokens, I) & "/");
                  ButtonLabel := To_Unbounded_String(Slice(Tokens, I));
                  if Length(ButtonLabel) > 7 then
                     ButtonLabel := Unbounded_Slice(ButtonLabel, 1, 7) & "...";
                  end if;
                  PathButton :=
                    Create
                      (Widget_Image(PathButtonsFrame) & ".button" &
                       Trim(Slice_Number'Image(I), Both),
                       "-text {" & To_String(ButtonLabel) & "} -command {" &
                       To_String(PathCommand) & " {" & To_String(Path) & "}}");
               end if;
               if I + 11 > Slice_Count(Tokens) then
                  if I = Slice_Count(Tokens) then
                     Shortcut := PathShortcut & "-r";
                     Tooltip :=
                       Mc(Get_Context, "{Reload the current directory:}") &
                       "\n" & To_String(Path) & "\n\[" & Shortcut & "\]";
                  elsif I = Slice_Count(Tokens) - 1 then
                     Shortcut := PathShortcut & "-u";
                     Tooltip :=
                       Mc(Get_Context, "{Go to directory:}") & "\n" &
                       To_String(Path) & "\n\[" & Shortcut & "\]";
                  else
                     Shortcut :=
                       PathShortcut & "-KP_" &
                       Slice_Number'Image(Slice_Count(Tokens) - I - 1)(2);
                     Tooltip :=
                       Mc(Get_Context, "{Go to directory:}") & "\n" &
                       To_String(Path) & "\n\[" & Shortcut & "\]";
                  end if;
                  Bind_To_Main_Window
                    (PathButton.Interp, "<" & To_String(Shortcut) & ">",
                     "{" & Widget_Image(PathButton) & " invoke}");
                  Add(PathButton, To_String(Tooltip));
               end if;
               Width :=
                 Width + Positive'Value(Winfo_Get(PathButton, "reqwidth"));
               if Width >
                 Positive'Value(Winfo_Get(PathButtonsFrame, "width")) then
                  Row := Row + 1;
                  Width := 0;
                  Column := 0;
               end if;
               Tcl.Tk.Ada.Grid.Grid
                 (PathButton,
                  "-row" & Natural'Image(Row) & " -column" &
                  Natural'Image(Column));
               Column := Column + 1;
               <<End_Of_Loop>>
            end loop;
         end if;
      else
         for I in List.First_Index .. List.Last_Index loop
            if (Settings.ShowHidden and List(I).IsHidden) or
              not List(I).IsHidden then
               Move(DirectoryTree, Positive'Image(I), "{}", Positive'Image(I));
               if SelectedIndex = Null_Unbounded_String or
                 CurrentSelected = CurrentDirectory & "/" & List(I).Name then
                  SelectedIndex := To_Unbounded_String(Positive'Image(I));
               end if;
            end if;
         end loop;
      end if;
      if not List.Is_Empty then
         if SelectedIndex /= Null_Unbounded_String and
           cget(DirectoryTree, "-selectmode") /= "none" then
            Selection_Set
              (DirectoryTree, "[list " & To_String(SelectedIndex) & "]");
            Tcl.Tk.Ada.Widgets.Focus(DirectoryTree);
            Tcl.Tk.Ada.Widgets.TtkTreeView.Focus
              (DirectoryTree, To_String(SelectedIndex));
         else
            Selection_Set(DirectoryTree, "{}");
         end if;
      end if;
   end UpdateDirectoryList;

end MainWindow;
