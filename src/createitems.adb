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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;
--with ActivateItems; use ActivateItems;
--with Toolbars; use Toolbars;

package body CreateItems is

   -- ****ie* CreateItems/Hunter_Create_Exception
   -- FUNCTION
   -- Raised when any problems with creating items happen
   -- SOURCE
   Hunter_Create_Exception: exception;
   -- ****

   function Show_Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* CreateItems/Show_Create_Command
      -- FUNCTION
      -- Show text entry to enter a name of the new item
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Show_Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      -- ****
      Frame: Ttk_Frame;
      Button: Ttk_Button;
      TextEntry: Ttk_Entry;
      Paned: Ttk_PanedWindow;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      Button.Interp := Interp;
      Button.Name := New_String(".mainframe.textframe.closebutton");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button.Name := New_String(".mainframe.textframe.okbutton");
      configure(Button, "-command {Create " & CArgv.Arg(Argv, 1) & "}");
      Add
        (Button,
         "Create a new " & CArgv.Arg(Argv, 1) & " with the selected name.");
      Add
        (TextEntry,
         "Enter a name for the newly created " & CArgv.Arg(Argv, 1) & ".");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Unbind(TextEntry, "<KeyRelease>");
      Focus(TextEntry);
      Frame.Interp := Interp;
      Frame.Name := New_String(".mainframe.textframe");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-row 1 -columnspan 2 -sticky we");
      if CArgv.Arg(Argv, 1) = "file" then
         NewAction := CREATEFILE;
      elsif CArgv.Arg(Argv, 1) = "directory" then
         NewAction := CREATEDIRECTORY;
      else
         NewAction := CREATELINK;
         Frame.Name := New_String(".mainframe.paned.previewframe");
         if not Settings.ShowPreview then
            Add(Paned, Frame, "-weight 20");
         end if;
         Frame.Name := New_String(".mainframe.paned.previewframe.pathframe");
         Tcl.Tk.Ada.Pack.Pack
           (Frame, "-after .mainframe.paned.previewframe.title -fill x");
         Frame.Name := New_String(".mainframe.paned.previewframe.scrollx");
         configure
           (Frame,
            "-command [list .mainframe.paned.previewframe.directorytree yview]");
         Tcl.Tk.Ada.Pack.Pack(Frame, "-side bottom -fill x");
         Frame.Name := New_String(".mainframe.paned.previewframe.scrolly");
         configure
           (Frame,
            "-command [list .mainframe.paned.previewframe.directorytree xview]");
         Tcl.Tk.Ada.Pack.Pack(Frame, "-side right -fill y");
         Frame.Name :=
           New_String(".mainframe.paned.previewframe.directorytree");
         configure(Frame, "-selectmode browse");
         Tcl.Tk.Ada.Pack.Pack(Frame, "-side top -fill both -expand true");
         Frame.Name :=
           New_String(".mainframe.paned.previewframe.previewcanvas");
         Tcl.Tk.Ada.Pack.Pack_Forget(Frame);
         Frame.Name := New_String(".mainframe.paned.previewframe.previewtext");
         Tcl.Tk.Ada.Pack.Pack_Forget(Frame);
         Frame.Name := New_String(".mainframe.paned.previewframe.title");
         configure(Frame, "-text {Destination directory}");
         DestinationDirectory := CurrentDirectory;
         LoadDirectory(To_String(DestinationDirectory), True);
         UpdateDirectoryList(True, "preview");
      end if;
      ToggleToolButtons(NewAction);
      return TCL_OK;
   end Show_Create_Command;

   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;

      -- ****if* CreateItems/Create_Command
      -- FUNCTION
      -- Show text entry to enter a name of the new item
      -- PARAMETERS
      -- ClientData - Custom data send to the command. Unused
      -- Interp     - Tcl interpreter in which command was executed.
      -- Argc       - Number of arguments passed to the command. Unused
      -- Argv       - Values of arguments passed to the command.
      -- SOURCE
   function Create_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TextEntry: Ttk_Entry;
      NewItemName, ActionString, ActionBlocker, Destination: Unbounded_String;
      Button: Ttk_Button;
      File: File_Descriptor;
      DirectoryView: Ttk_Tree_View;
   begin
      TextEntry.Interp := Interp;
      TextEntry.Name := New_String(".mainframe.textframe.textentry");
      NewItemName := CurrentDirectory & "/" & Get(TextEntry);
      Button.Interp := Interp;
      Button.Name := New_String(".mainframe.textframe.closebutton");
      DirectoryView.Interp := Interp;
      DirectoryView.Name :=
        New_String(".mainframe.paned.previewframe.directorytree");
      if Exists(To_String(NewItemName)) or
        Is_Symbolic_Link(To_String(NewItemName)) then
         ActionString :=
           To_Unbounded_String("create " & CArgv.Arg(Argv, 1) & " with");
         if Is_Directory(To_String(NewItemName)) then
            ActionBlocker := To_Unbounded_String("directory");
         else
            ActionBlocker := To_Unbounded_String("file");
         end if;
         ShowMessage
           ("You can't " & To_String(ActionString) & " name '" &
            To_String(NewItemName) & "' because there exists " &
            To_String(ActionBlocker) & " with that name.");
         goto End_Of_Create;
      end if;
      if not Is_Write_Accessible_File
          (Containing_Directory(To_String(NewItemName))) then
         ShowMessage
           ("You don't have permissions to write to " &
            Containing_Directory(To_String(NewItemName)));
         goto End_Of_Create;
      end if;
      case NewAction is
         when CREATEDIRECTORY =>
            Create_Path(To_String(NewItemName));
         when CREATEFILE =>
            Create_Path(Containing_Directory(To_String(NewItemName)));
            File := Create_File(To_String(NewItemName), Binary);
            Close(File);
         when CREATELINK =>
            if Selection(DirectoryView)'Length > 0 then
               Destination :=
                 SecondItemsList(Positive'Value(Selection(DirectoryView)))
                   .Name;
            else
               Destination := DestinationDirectory;
            end if;
            Tcl_Eval
              (Interp,
               "file link -symbolic {" &
               To_String(CurrentDirectory & "/" & NewItemName) & "} {" &
               To_String(Destination) & "}");
         when others =>
            raise Hunter_Create_Exception with "Invalid action type";
      end case;
      if not Settings.StayInOld then
         CurrentDirectory :=
           To_Unbounded_String(Containing_Directory(To_String(NewItemName)));
      end if;
      LoadDirectory(To_String(CurrentDirectory));
      UpdateWatch(To_String(CurrentDirectory));
      UpdateDirectoryList(True);
      <<End_Of_Create>>
      if Invoke(Button) /= "" then
         raise Hunter_Create_Exception with "Can't hide create item bar";
      end if;
      ToggleToolButtons(NewAction, True);
      return TCL_OK;
   end Create_Command;

   procedure CreateCreateUI is
   begin
      AddCommand("ShowCreate", Show_Create_Command'Access);
      AddCommand("Create", Create_Command'Access);
   end CreateCreateUI;

--   -- ****iv* CreateItems/SourceDirectory
--   -- FUNCTION
--   -- Full path to the source directory of item to which link was created
--   -- SOURCE
--   SourceDirectory: Unbounded_String;
--   -- ****
--
--   -- ****if* CreateItems/CreateItem
--   -- FUNCTION
--   -- Create new or rename existing file, directory or symbolic link or hide
--   -- text entry
--   -- PARAMETERS
--   -- Self     - Text entry with name for new file/directory
--   -- Icon_Pos - Position of text entry icon which was pressed or if key
--   --            Enter was pressed, simulate pressing proper icon
--   -- SOURCE
--   procedure CreateItem
--     (Self: access Gtk_Entry_Record'Class;
--      Icon_Pos: Gtk_Entry_Icon_Position) is
--      -- ****
--      Name: constant String :=
--        To_String(CurrentDirectory) & "/" & Get_Text(Self);
--      File: File_Descriptor;
--      ActionString, ActionBlocker: Unbounded_String;
--      Success: Boolean := False;
--   begin
--      if Icon_Pos = Gtk_Entry_Icon_Primary then
--         Set_Text(Self, "");
--         Hide(Gtk_Widget(Self));
--         if NewAction = CREATELINK then
--            Hide(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
--            Set_Visible_Child_Name(InfoStack, "preview");
--            NewAction := CREATEFILE;
--         end if;
--         ToggleToolButtons(NewAction, True);
--         return;
--      end if;
--      if Get_Text(Self) = "" then
--         return;
--      end if;
--      if NewAction = GOTOPATH then
--         if not Ada.Directories.Exists(Get_Text(Self)) then
--            ShowMessage
--              (Gettext("Directory ") & Name & Gettext(" doesn't exists."));
--            return;
--         end if;
--         if not Is_Read_Accessible_File(Get_Text(Self)) then
--            ShowMessage
--              (Gettext("You don't have permissions to enter directory ") &
--               Get_Text(Self));
--            return;
--         end if;
--         CurrentDirectory := To_Unbounded_String(Get_Text(Self));
--         goto Update_UI;
--      end if;
--      if Ada.Directories.Exists(Name) or Is_Symbolic_Link(Name) then
--         case NewAction is
--            when CREATEDIRECTORY =>
--               ActionString :=
--                 To_Unbounded_String(Gettext("create directory with"));
--            when CREATEFILE =>
--               ActionString :=
--                 To_Unbounded_String(Gettext("create file with"));
--            when RENAME =>
--               ActionString := To_Unbounded_String(Gettext("rename with new"));
--            when CREATELINK =>
--               ActionString :=
--                 To_Unbounded_String(Gettext("create link with"));
--            when others =>
--               null;
--         end case;
--         if Is_Directory(Name) then
--            ActionBlocker := To_Unbounded_String(Gettext("directory"));
--         else
--            ActionBlocker := To_Unbounded_String(Gettext("file"));
--         end if;
--         ShowMessage
--           (Gettext("You can't ") & To_String(ActionString) &
--            Gettext(" name '") & Name & Gettext("' because there exists ") &
--            To_String(ActionBlocker) & Gettext(" with that name."));
--         return;
--      end if;
--      if Is_Write_Accessible_File(Containing_Directory(Name)) then
--         case NewAction is
--            when CREATEDIRECTORY =>
--               Create_Path(Name);
--            when CREATEFILE =>
--               Create_Path(Containing_Directory(Name));
--               File := Create_File(Name, Binary);
--               Close(File);
--            when RENAME =>
--               if To_String(CurrentSelected) /= Name then
--                  Rename_File(To_String(CurrentSelected), Name, Success);
--                  if not Success then
--                     ShowMessage
--                       (Gettext("Can't rename ") & To_String(CurrentSelected) &
--                        ".");
--                  end if;
--               end if;
--            when CREATELINK =>
--               declare
--                  Arguments: constant Argument_List :=
--                    (new String'("-s"), new String'(To_String(LinkTarget)),
--                     new String'
--                       (To_String(CurrentDirectory) & "/" & Get_Text(Self)));
--               begin
--                  Spawn(Locate_Exec_On_Path("ln").all, Arguments, Success);
--                  if not Success then
--                     ShowMessage(Gettext("Can't create symbolic link."));
--                  end if;
--                  Hide(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 0));
--                  Set_Visible_Child_Name(InfoStack, "preview");
--                  NewAction := CREATEFILE;
--               end;
--            when others =>
--               null;
--         end case;
--      else
--         if NewAction /= RENAME then
--            ShowMessage
--              (Gettext("You don't have permissions to write to ") &
--               Containing_Directory(Name));
--         else
--            ShowMessage
--              (Gettext("You don't have permissions to rename ") & Name);
--         end if;
--         return;
--      end if;
--      CurrentDirectory := To_Unbounded_String(Containing_Directory(Name));
--      <<Update_UI>>
--      Set_Text(Self, "");
--      Hide(Gtk_Widget(Self));
--      ToggleToolButtons(NewAction, True);
--      if Settings.StayInOld then
--         CurrentDirectory := SourceDirectory;
--      end if;
--      if Get_Visible_Child_Name(InfoStack) = "destination" then
--         LoadDirectory(To_String(CurrentDirectory), "fileslist2");
--      else
--         Reload;
--      end if;
--   end CreateItem;
--
--   -- ****if* CreateItems/IconPressed
--   -- FUNCTION
--   -- Create new file or directory when user press icon or hide text entry
--   -- PARAMETERS
--   -- Self     - Text entry with name for new file/directory
--   -- Icon_Pos - Position of text entry icon which was pressed
--   -- SOURCE
--   procedure IconPressed
--     (Self: access Gtk_Entry_Record'Class; Icon_Pos: Gtk_Entry_Icon_Position;
--      Event: Gdk_Event_Button) is
--      pragma Unreferenced(Event);
--      -- ****
--   begin
--      if NewAction /= OPENWITH then
--         CreateItem(Self, Icon_Pos);
--      else
--         OpenItemWith(Self, Icon_Pos);
--      end if;
--   end IconPressed;
--
--   -- ****if* CreateItems/CreateNew
--   -- FUNCTION
--   -- Create new file or directory when user press enter in text entry
--   -- PARAMETERS
--   -- Self - Gtk_GEntry in which Enter was pressed.
--   -- SOURCE
--   procedure CreateNew(Self: access Gtk_Entry_Record'Class) is
--   -- ****
--   begin
--      if NewAction /= OPENWITH then
--         CreateItem(Self, Gtk_Entry_Icon_Secondary);
--      else
--         OpenItemWith(Self, Gtk_Entry_Icon_Secondary);
--      end if;
--   end CreateNew;
--
--   -- ****if* CreateItems/AddNewButton
--   -- FUNCTION
--   -- Show text entry for enter new directory name after click button Add New
--   -- PARAMETERS
--   -- Self - Gtk_Tool_Button which was clicked. Unused. Can be null.
--   -- SOURCE
--   procedure AddNewButton(Self: access Gtk_Tool_Button_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--   begin
--      NewAction := CREATEDIRECTORY;
--      Set_Icon_Tooltip_Text
--        (TextEntry, Gtk_Entry_Icon_Secondary,
--         Gettext("Create new directory."));
--      ToggleToolButtons(NewAction);
--      Show_All(TextEntry);
--      Grab_Focus(TextEntry);
--   end AddNewButton;
--
--   -- ****if* CreateItems/AddNewFile
--   -- FUNCTION
--   -- Show text entry for enter new file name after selecting menu New File
--   -- PARAMETERS
--   -- Self - Gtk_Menu_Item which was selected. Unused.
--   -- SOURCE
--   procedure AddNewFile(Self: access Gtk_Menu_Item_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--   begin
--      NewAction := CREATEFILE;
--      Set_Icon_Tooltip_Text
--        (TextEntry, Gtk_Entry_Icon_Secondary,
--         Gettext("Create new empty file."));
--      ToggleToolButtons(NewAction);
--      Show_All(TextEntry);
--      Grab_Focus(TextEntry);
--   end AddNewFile;
--
--   -- ****if* CreateItems/AddNewLink
--   -- FUNCTION
--   -- Show text entry and destination for create new link after selecting menu
--   -- New Link
--   -- PARAMETERS
--   -- Self - Gtk_Menu_Item which was selected. Unused.
--   -- SOURCE
--   procedure AddNewLink(Self: access Gtk_Menu_Item_Record'Class) is
--      pragma Unreferenced(Self);
--      -- ****
--   begin
--      NewAction := CREATELINK;
--      SourceDirectory := CurrentDirectory;
--      LinkTarget := CurrentSelected;
--      Set_Icon_Tooltip_Text
--        (TextEntry, Gtk_Entry_Icon_Secondary,
--         Gettext("Create new link to selected file or directory."));
--      LoadDirectory(To_String(CurrentDirectory), "fileslist2");
--      Set_Markup
--        (Gtk_Label
--           (Get_Label_Widget
--              (Gtk_Frame(Get_Child(Gtk_Box(Get_Child2(FilesPaned)), 1)))),
--         "<b>" & Gettext("Destination directory") & "</b>");
--      Set_Visible_Child_Name(InfoStack, "destination");
--      ToggleToolButtons(NewAction);
--      Show_All(TextEntry);
--      Grab_Focus(TextEntry);
--   end AddNewLink;
--
--   procedure CreateCreateUI is
--      CreateNewMenu: constant Gtk_Menu := Gtk_Menu_New;
--      MenuItem: Gtk_Menu_Item;
--   begin
--      Set_Icon_From_Icon_Name
--        (TextEntry, Gtk_Entry_Icon_Primary, "window-close");
--      Set_Icon_Tooltip_Text(TextEntry, Gtk_Entry_Icon_Primary, "Close");
--      Set_Icon_From_Icon_Name
--        (TextEntry, Gtk_Entry_Icon_Secondary, "mail-send");
--      On_Activate(TextEntry, CreateNew'Access);
--      On_Icon_Press(TextEntry, IconPressed'Access);
--      On_Clicked
--        (Gtk_Tool_Button(Get_Nth_Item(ActionToolBar, 4)), AddNewButton'Access);
--      MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("New _File"));
--      On_Activate(MenuItem, AddNewFile'Access);
--      Append(CreateNewMenu, MenuItem);
--      MenuItem := Gtk_Menu_Item_New_With_Mnemonic(Gettext("New _Link"));
--      On_Activate(MenuItem, AddNewLink'Access);
--      Append(CreateNewMenu, MenuItem);
--      Show_All(CreateNewMenu);
--      Set_Menu
--        (Gtk_Menu_Tool_Button(Get_Nth_Item(ActionToolBar, 4)), CreateNewMenu);
--   end CreateCreateUI;

end CreateItems;
