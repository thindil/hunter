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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Bin; use Gtk.Bin;
with Gtk.Box; use Gtk.Box;
with Gtk.Cell_Area_Box; use Gtk.Cell_Area_Box;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Flow_Box; use Gtk.Flow_Box;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Main; use Gtk.Main;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Search_Entry; use Gtk.Search_Entry;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Tool_Button; use Gtk.Tool_Button;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtkada.Intl; use Gtkada.Intl;
with Glib; use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Values; use Glib.Values;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with AboutDialog; use AboutDialog;
with ActivateItems; use ActivateItems;
with Bookmarks; use Bookmarks;
with CopyItems; use CopyItems;
with CreateItems; use CreateItems;
with DeleteItems; use DeleteItems;
with LoadData; use LoadData;
with Messages; use Messages;
with MoveItems; use MoveItems;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with SearchItems; use SearchItems;
with ShowItems; use ShowItems;
with Toolbars; use Toolbars;
with Trash; use Trash;
with Utils; use Utils;

package body MainWindow is

   FilesMenu: Gtk_Menu;

   procedure Quit(Self: access Gtk_Widget_Record'Class) is
      pragma Unreferenced(Self);
   begin
      SavePreferences;
      if Settings.ClearTrashOnExit then
         NewAction := CLEARTRASH;
         if DeleteSelected then
            Reload;
         end if;
      end if;
      Main_Quit;
   end Quit;

   function SetSelected
     (Model: Gtk_Tree_Model; Path: Gtk_Tree_Path; Iter: Gtk_Tree_Iter)
      return Boolean is
   begin
      if Get_String(Model, Iter, 0) =
        Simple_Name(To_String(CurrentSelected)) then
         Set_Cursor(DirectoryView, Path, null, False);
         return True;
      end if;
      return False;
   end SetSelected;

   procedure Reload is
      OldSelected: Unbounded_String;
   begin
      if CurrentDirectory = Null_Unbounded_String then
         CurrentDirectory := To_Unbounded_String("/");
      end if;
      if CurrentSelected /= Null_Unbounded_String then
         OldSelected := CurrentSelected;
      else
         OldSelected := To_Unbounded_String(".");
      end if;
      LoadDirectory(To_String(CurrentDirectory), "fileslist");
      ToggleActionButtons;
      if N_Children(Get_Model(DirectoryView), Null_Iter) = 0 then
         CurrentSelected := CurrentDirectory;
      else
         if Containing_Directory(To_String(OldSelected)) /=
           To_String(CurrentDirectory) then
            OldSelected := Null_Unbounded_String;
         end if;
         if OldSelected = Null_Unbounded_String
           or else not Ada.Directories.Exists(To_String(OldSelected)) then
            Set_Cursor
              (DirectoryView, Gtk_Tree_Path_New_From_String("0"), null, False);
         else
            CurrentSelected := OldSelected;
            Foreach(Get_Model(DirectoryView), SetSelected'Access);
         end if;
         Grab_Focus(DirectoryView);
      end if;
      ShowItem(Get_Selection(DirectoryView));
   end Reload;

   -- ****if* MainWindow/StartRename
   -- FUNCTION
   -- Show text entry to start renaming selected file or directory and fill it
   -- with current element name.
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was clicked
   -- SOURCE
   procedure StartRename(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      NewAction := RENAME;
      if Is_Directory(To_String(CurrentSelected)) then
         Set_Icon_Tooltip_Text
           (TextEntry, Gtk_Entry_Icon_Secondary, Gettext("Rename directory."));
      else
         Set_Icon_Tooltip_Text
           (TextEntry, Gtk_Entry_Icon_Secondary, Gettext("Rename file."));
      end if;
      ToggleToolButtons(NewAction);
      Set_Text(TextEntry, Simple_Name(To_String(CurrentSelected)));
      Show_All(TextEntry);
      Grab_Focus(TextEntry);
   end StartRename;

   -- ****if* MainWindow/ShowFiles
   -- FUNCTION
   -- Back to files listing and preview/info
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was clicked.
   -- SOURCE
   procedure ShowFiles(Self: access Gtk_Tool_Button_Record'Class) is
   -- ****
   begin
      Setting := True;
      if Get_Active
          (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 6))) then
         CopyItemsList.Clear;
         Set_Active
           (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 6)), False);
      end if;
      if Get_Active
          (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 7))) then
         MoveItemsList.Clear;
         Set_Active
           (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 7)), False);
      end if;
      Setting := False;
      Hide(Get_Child(Gtk_Box(Get_Child_By_Name(FileStack, "page0")), 3));
      ToggleToolButtons(NewAction, True);
      CloseMessage(null);
      Show_All(ActionToolBar);
      Show_All(Get_Child(Gtk_Box(Get_Child1(FilesPaned)), 0));
      Hide(Self);
      Hide(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
      CurrentSelected := Null_Unbounded_String;
      ToggleToolButtons(NewAction, True);
      ShowItem(Get_Selection(DirectoryView));
   end ShowFiles;

   -- ****if* MainWindow/ShowAbout
   -- FUNCTION
   -- Show dialog with informations about the program
   -- PARAMETERS
   -- Self - Gtk_Tool_Button which was clicked. Unused
   -- SOURCE
   procedure ShowAbout(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowAboutDialog(Window);
   end ShowAbout;

   -- ****if* MainWindow/EntryKeyPressed
   -- FUNCTION
   -- Close text entry on press Escape key
   -- PARAMETERS
   -- Self  - Currently active entry
   -- Event - Detailed informations about key pressed event (key code,
   --         modifiers, etc)
   -- RESULT
   -- This function always return False
   -- SOURCE
   function EntryKeyPressed
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean is
   -- ****
   begin
      if Event.Keyval = GDK_Escape then
         ToggleToolButtons(NewAction, True);
         if Self = Gtk_Widget(SearchEntry) then
            Set_Active
              (Gtk_Toggle_Tool_Button(Get_Nth_Item(ActionToolBar, 1)), False);
         else
            Hide(Self);
         end if;
         if NewAction = CREATELINK then
            Hide(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
            Set_Visible_Child_Name(InfoStack, "preview");
            NewAction := CREATEFILE;
         end if;
         PreviewItem(null);
      end if;
      return False;
   end EntryKeyPressed;

   -- ****if* MainWindow/WindowKeyPressed
   -- FUNCTION
   -- Close the program with keyboard shortcut
   -- PARAMETERS
   -- Self  - Main window of the program (not used)
   -- Event - Detailed informations about key pressed event (key code,
   --         modifiers, etc)
   -- RESULT
   -- This function always return False
   -- SOURCE
   function WindowKeyPressed
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean is
      pragma Unreferenced(Self);
      -- ****
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      Key: Gtk_Accel_Key;
      Found: Boolean;
   begin
      Lookup_Entry("<mainwindow>/quit", Key, Found);
      if not Found then
         return False;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         Quit(Window);
      end if;
      return False;
   end WindowKeyPressed;

   -- ****if* MainWindow/GetWindowSize
   -- FUNCTION
   -- Get the main window size at quitting of the program
   -- PARAMETERS
   -- Self  - Main window of the program which triggered this event
   -- Event - Gdk_Event which was triggered. Unused
   -- RESULT
   -- This function always return False
   -- SOURCE
   function GetWindowSize
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event) return Boolean is
      pragma Unreferenced(Event);
      -- ****
   begin
      Settings.WindowWidth := Positive(Get_Allocated_Width(Self));
      Settings.WindowHeight := Positive(Get_Allocated_Height(Self));
      return False;
   end GetWindowSize;

   MenuIndex: Positive;

   -- ****if* MainWindow/SetFilesMenu
   -- FUNCTION
   -- Set visibility of selected file or directory popup menu elements
   -- PARAMETERS
   -- Widget - Menu item which will be check
   -- SOURCE
   procedure SetFilesMenu
     (Widget: not null access Gtk.Widget.Gtk_Widget_Record'Class) is
   -- ****
   begin
      case MenuIndex is
         when 1 =>
            Set_Visible
              (Widget, Get_Visible(Gtk_Widget(Get_Nth_Item(ItemToolBar, 1))));
            if not Is_Visible(ItemToolBar) then
               Hide(Widget);
            end if;
         when 2 =>
            Set_Visible
              (Widget, Get_Visible(Gtk_Widget(Get_Nth_Item(ItemToolBar, 0))));
            if not Is_Visible(ItemToolBar) then
               Hide(Widget);
            end if;
         when 3 =>
            Set_Visible
              (Widget, Get_Visible(Gtk_Widget(Get_Nth_Item(ItemToolBar, 2))));
            if not Is_Visible(ItemToolBar) then
               Hide(Widget);
            end if;
         when 4 =>
            Set_Visible
              (Widget, Get_Visible(Gtk_Widget(Get_Nth_Item(ItemToolBar, 2))));
            if not Is_Visible(ItemToolBar) then
               Hide(Widget);
            end if;
         when 8 =>
            if Count_Selected_Rows(Get_Selection(DirectoryView)) =
              N_Children(Get_Model(DirectoryView)) then
               Set_Label(Gtk_Menu_Item(Widget), Gettext("Unselect all"));
            else
               Set_Label(Gtk_Menu_Item(Widget), Gettext("Select all"));
            end if;
         when others =>
            null;
      end case;
      MenuIndex := MenuIndex + 1;
   end SetFilesMenu;

   -- ****if* MainWindow/ShowFilesMenu
   -- FUNCTION
   -- Show menu with available actions on right click on directory listing
   -- PARAMETERS
   -- Self  - Tree view to which menu is attached. Unused
   -- Event - Detailed info about mouse press button event
   -- RESULT
   -- Return True if pressed button was right button, otherwise False
   -- SOURCE
   function ShowFilesMenu
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Button)
      return Boolean is
      pragma Unreferenced(Self);
      -- ****
   begin
      if Event.Button /= 3 then
         return False;
      end if;
      MenuIndex := 1;
      Foreach(FilesMenu, SetFilesMenu'Access);
      Popup
        (Menu => FilesMenu, Button => Event.Button,
         Activate_Time => Event.Time);
      return True;
   end ShowFilesMenu;

   -- ****if* MainWindow/ShowFile
   -- FUNCTION
   -- Show selected file to user
   -- PARAMETERS
   -- FileName - Name of file to show
   -- SOURCE
   procedure ShowFile(FileName: String) is
      -- ****
      FilesList: constant Gtk_Tree_Model_Sort :=
        -(Gtk.Tree_View.Get_Model(DirectoryView));
      FilesIter: Gtk_Tree_Iter;
   begin
      CurrentDirectory :=
        To_Unbounded_String(Containing_Directory(Current_Directory));
      if Ada.Directories.Exists
          (Value("APPDIR", "") & "/usr/share/doc/hunter") then
         CurrentDirectory :=
           To_Unbounded_String(Value("APPDIR", "") & "/usr/share/doc/hunter");
      end if;
      Reload;
      FilesIter := Get_Iter_First(FilesList);
      loop
         if Get_String(FilesList, FilesIter, 0) = FileName then
            Set_Cursor
              (DirectoryView, Get_Path(FilesList, FilesIter), null, False);
            exit;
         end if;
         Next(FilesList, FilesIter);
         exit when not Iter_Is_Valid(FilesList, FilesIter);
      end loop;
   end ShowFile;

   procedure SelectAll(Self: access Gtk_Tool_Button_Record'Class) is
      pragma Unreferenced(Self);
      Selection: constant Gtk_Tree_Selection := Get_Selection(DirectoryView);
   begin
      if Count_Selected_Rows(Selection) =
        N_Children(Get_Model(DirectoryView)) then
         Unselect_All(Selection);
         Set_Cursor
           (DirectoryView, Gtk_Tree_Path_New_From_String("0"), null, False);
         Grab_Focus(DirectoryView);
      else
         Select_All(Selection);
      end if;
   end SelectAll;

   -- ****if* MainWindow/ShowReadme
   -- FUNCTION
   -- Show README.md file when the user select it from menu
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused
   -- SOURCE
   procedure ShowReadme(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowFile("README.md");
   end ShowReadme;

   -- ****if* MainWindow/ShowChangelog
   -- FUNCTION
   -- Show CHANGELOG.md file when the user select it from menu
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused
   -- SOURCE
   procedure ShowChangelog(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowFile("CHANGELOG.md");
   end ShowChangelog;

   -- ****if* MainWindow/ShowContributing
   -- FUNCTION
   -- Show CONTRIBUTING.md file when the user select it from menu
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused
   -- SOURCE
   procedure ShowContributing(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ShowFile("CONTRIBUTING.md");
   end ShowContributing;

   -- ****if* MainWindow/SelectAllMenu
   -- FUNCTION
   -- Select or unselect all files and directories in current directory
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure SelectAllMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      SelectAll(null);
   end SelectAllMenu;

   -- ****if* MainWindow/StartRenameMenu
   -- FUNCTION
   -- Start renaming selected file or directory
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure StartRenameMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      StartRename(null);
   end StartRenameMenu;

   -- ****if* MainWindow/CopyItemMenu
   -- FUNCTION
   -- Start copying selected file or directory
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure CopyItemMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      CopyData(null);
   end CopyItemMenu;

   -- ****if* MainWindow/MoveItemMenu
   -- FUNCTION
   -- Start moving selected file or directory
   -- SOURCE
   procedure MoveItemMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
   begin
      MoveData(null);
   end MoveItemMenu;

   -- ****if* MainWindow/DeleteItemMenu
   -- FUNCTION
   -- Delete selected file or directory
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure DeleteItemMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      DeleteItem(null);
   end DeleteItemMenu;

   -- ****if* MainWindow/ActivateFileMenu
   -- FUNCTION
   -- Activate selected file or directory.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure ActivateFileMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ActivateFile
        (DirectoryView, Gtk_Tree_Path_New, Get_Column(DirectoryView, 0));
   end ActivateFileMenu;

   -- ****if* MainWindow/StartOpenWithMenu
   -- FUNCTION
   -- Start opening selected file or directory with user defined command.
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure StartOpenWithMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      StartOpenWith(null);
   end StartOpenWithMenu;

   -- ****if* MainWindow/ExecuteFileMenu
   -- FUNCTION
   -- Execute selected file
   -- PARAMETERS
   -- Self - Gtk_Menu_Item clicked. Unused.
   -- SOURCE
   procedure ExecuteFileMenu(Self: access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      ExecuteFile(null);
   end ExecuteFileMenu;

   procedure CreateMainWindow(Directory: String) is
      FilesBox: constant Gtk_Hbox := Gtk_Hbox_New;
      ProgressBar: constant Gtk_Progress_Bar := Gtk_Progress_Bar_New;
      StackBox: constant Gtk_Vbox := Gtk_Vbox_New;
      Header: constant Gtk_Header_Bar := Gtk_Header_Bar_New;
   begin
      Setting := True;
      Window := Gtk_Window_New;
      On_Key_Press_Event(Window, WindowKeyPressed'Access);
      On_Delete_Event(Window, GetWindowSize'Access);
      On_Destroy(Window, Quit'Access);
      Set_Default_Size(Window, 800, 600);
      Set_Position(Window, Win_Pos_Center);
      Accelerators := Gtk_Accel_Group_New;
      Add_Accel_Group(Window, Accelerators);
      Set_Title(Window, Gettext("Hunter"));
      if not Set_Icon_From_File(Window, "ui/hunter-icon.png") then
         raise Program_Error with "Can't set the program icon";
      end if;
      Add(Window, Gtk_Vbox_New);
      Set_Show_Close_Button(Header, True);
      Set_Has_Subtitle(Header, True);
      Pack_Start(Gtk_Box(Get_Child(Window)), Header, False);
      CreateActionToolbarUI;
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 2)), SelectAll'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 5)), StartRename'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 6)), CopyData'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 7)), MoveData'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 8)), DeleteItem'Access);
      On_Clicked
        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 9)), ShowFiles'Access);
      declare
         AboutButton: constant Gtk_Menu_Tool_Button :=
           Gtk_Menu_Tool_Button(Get_Nth_Item(ActionToolBar, 13));
         AboutMenu: constant Gtk_Menu := Gtk_Menu_New;
         MenuItem: Gtk_Menu_Item;
      begin
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Show README"));
         On_Activate(MenuItem, ShowReadme'Access);
         Append(AboutMenu, MenuItem);
         MenuItem :=
           Gtk_Menu_Item_New_With_Mnemonic(Gettext("Show list of changes"));
         On_Activate(MenuItem, ShowChangelog'Access);
         Append(AboutMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Get involved"));
         On_Activate(MenuItem, ShowContributing'Access);
         Append(AboutMenu, MenuItem);
         Show_All(AboutMenu);
         Set_Menu(AboutButton, AboutMenu);
         On_Clicked(AboutButton, ShowAbout'Access);
      end;
      CreateItemToolbarUI;
      FileStack := Gtk_Stack_New;
      Pack_End(Gtk_Box(Get_Child(Gtk_Bin(Window))), FileStack);
      FilesPaned := Gtk_Hpaned_New;
      DirectoryView :=
        Gtk_Tree_View_New_With_Model
          (+(Gtk_Tree_Model_Sort_Sort_New_With_Model
              (+(Gtk_Tree_Model_Filter_Filter_New
                  (+(Gtk_List_Store_Newv
                      ((GType_String, GType_Uint, GType_String, GType_String,
                        GType_Uint, GType_String, GType_String))))))));
      Add_Named(FileStack, StackBox, "page0");
      TextEntry := Gtk_Entry_New;
      Pack_Start(StackBox, TextEntry, False);
      SearchEntry := Gtk_Search_Entry_New;
      Pack_Start(StackBox, SearchEntry, False);
      InfoBar := Gtk_Info_Bar_New;
      Pack_Start(StackBox, InfoBar, False);
      Pack_Start(StackBox, ProgressBar, False);
      Pack_Start(FilesBox, Gtk_Vbox_New, False);
      Pack_Start(StackBox, FilesBox);
      SetToolbars;
      CreateActivateUI;
      CreateBookmarksUI;
      CreateCreateUI;
      CreateShowItemsUI;
      CreateSearchUI;
      CreateTrashUI;
      CreatePreferences(Gtk_Widget(Get_Nth_Item(ActionToolBar, 12)));
      On_Key_Press_Event(TextEntry, EntryKeyPressed'Access);
      On_Key_Press_Event(SearchEntry, EntryKeyPressed'Access);
      Add_Entry("<mainwindow>/reload", GDK_LC_r, Mod1_Mask);
      Add_Entry("<mainwindow>/goup", GDK_LC_u, Mod1_Mask);
      Add_Entry("<mainwindow>/path1", GDK_1, Mod1_Mask);
      Add_Entry("<mainwindow>/path2", GDK_2, Mod1_Mask);
      Add_Entry("<mainwindow>/path3", GDK_3, Mod1_Mask);
      Add_Entry("<mainwindow>/path4", GDK_4, Mod1_Mask);
      Add_Entry("<mainwindow>/path5", GDK_5, Mod1_Mask);
      Add_Entry("<mainwindow>/path6", GDK_6, Mod1_Mask);
      Add_Entry("<mainwindow>/path7", GDK_7, Mod1_Mask);
      Add_Entry("<mainwindow>/path8", GDK_8, Mod1_Mask);
      Add_Entry("<mainwindow>/path9", GDK_9, Mod1_Mask);
      Add_Entry("<mainwindow>/reload2", GDK_LC_r, Mod1_Mask + Shift_Mask);
      Add_Entry("<mainwindow>/goup2", GDK_LC_u, Mod1_Mask + Shift_Mask);
      Add_Entry("<mainwindow>/path12", GDK_exclam, Mod1_Mask);
      Add_Entry("<mainwindow>/path22", GDK_at, Mod1_Mask);
      Add_Entry("<mainwindow>/path32", GDK_numbersign, Mod1_Mask);
      Add_Entry("<mainwindow>/path42", GDK_dollar, Mod1_Mask);
      Add_Entry("<mainwindow>/path52", GDK_percent, Mod1_Mask);
      Add_Entry("<mainwindow>/path62", GDK_asciicircum, Mod1_Mask);
      Add_Entry("<mainwindow>/path72", GDK_ampersand, Mod1_Mask);
      Add_Entry("<mainwindow>/path82", GDK_parenleft, Mod1_Mask);
      Add_Entry("<mainwindow>/path92", GDK_parenright, Mod1_Mask);
      Add_Entry("<mainwindow>/quit", GDK_LC_q, Control_Mask);
      if Ada.Directories.Exists(Directory) then
         CurrentDirectory := To_Unbounded_String(Directory);
      else
         CurrentDirectory := To_Unbounded_String(Value("HOME"));
         if not Ada.Directories.Exists(To_String(CurrentDirectory)) then
            CurrentDirectory := To_Unbounded_String("/");
         end if;
      end if;
      Set_Default_Size
        (Window, Gint(Settings.WindowWidth), Gint(Settings.WindowHeight));
      Show_All(Window);
      Hide(SearchEntry);
      Hide(TextEntry);
      Hide(Gtk_Widget(Get_Nth_Item(ActionToolBar, 9)));
      Hide(Gtk_Widget(Get_Nth_Item(ActionToolBar, 10)));
      Hide(ProgressBar);
      Hide(InfoBar);
      declare
         FilesScroll: constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
         Area: Gtk_Cell_Area_Box;
         Renderer: Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
         Renderer2: constant Gtk_Cell_Renderer_Pixbuf :=
           Gtk_Cell_Renderer_Pixbuf_New;
         Column: Gtk_Tree_View_Column;
         Value: GValue;
         Box: constant Gtk_Vbox := Gtk_Vbox_New;
         MenuItem: Gtk_Menu_Item;
      begin
         Set_Enable_Search(DirectoryView, False);
         Set_Headers_Clickable(DirectoryView, True);
         Set_Mode(Get_Selection(DirectoryView), Selection_Multiple);
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
         Area := Gtk_Cell_Area_Box_New;
         Renderer := Gtk_Cell_Renderer_Text_New;
         Pack_Start(Area, Renderer, True);
         Add_Attribute(Area, Renderer, "text", 5);
         Column := Gtk_Tree_View_Column_New_With_Area(Area);
         Set_Sort_Column_Id(Column, 5);
         Set_Title(Column, Gettext("Modified"));
         Set_Resizable(Column, True);
         if Append_Column(DirectoryView, Column) /= 3 then
            return;
         end if;
         Set_Visible(Get_Column(DirectoryView, 2), Settings.ShowLastModified);
         On_Row_Activated(DirectoryView, ActivateFile'Access);
         On_Button_Press_Event(DirectoryView, ShowFilesMenu'Access);
         FilesMenu := Gtk_Menu_New;
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Open"));
         On_Activate(MenuItem, ActivateFileMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Run"));
         On_Activate(MenuItem, ExecuteFileMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Open with..."));
         On_Activate(MenuItem, StartOpenWithMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Rename"));
         On_Activate(MenuItem, StartRenameMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Copy"));
         On_Activate(MenuItem, CopyItemMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Move"));
         On_Activate(MenuItem, MoveItemMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Delete"));
         On_Activate(MenuItem, DeleteItemMenu'Access);
         Append(FilesMenu, MenuItem);
         MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("Select All"));
         On_Activate(MenuItem, SelectAllMenu'Access);
         Append(FilesMenu, MenuItem);
         Show_All(FilesMenu);
         Attach_To_Widget(FilesMenu, DirectoryView, null);
         On_Changed(Get_Selection(DirectoryView), ShowItem'Access);
         Add(FilesScroll, DirectoryView);
         Pack_Start(Box, Gtk_Flow_Box_New, False);
         Pack_Start(Box, FilesScroll);
         Add1(FilesPaned, Box);
         Reload;
      end;
      Pack_Start(FilesBox, FilesPaned);
      Show_All(FilesPaned);
      if Settings.ShowPreview then
         Set_Position
           (FilesPaned, Gint(Float(Get_Allocated_Width(Window)) * 0.3));
      else
         Hide(Get_Child2(FilesPaned));
         Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 4)));
         Hide(Gtk_Widget(Get_Nth_Item(ItemToolBar, 5)));
         Set_Position(FilesPaned, Get_Allocated_Width(Window));
      end if;
      Grab_Focus(DirectoryView);
      StartTimer(To_String(CurrentDirectory));
      Setting := False;
   end CreateMainWindow;

end MainWindow;
