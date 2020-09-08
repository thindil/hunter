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

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils; use Utils;

package body Trash is

   function Show_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      Directory, SubDirectory: Dir_Type;
      Last, SubLast: Natural;
      FileName, SubFileName: String(1 .. 1024);
      FileInfo: File_Type;
      Size: File_Size;
      FileLine, FullName, MimeType: Unbounded_String;
      Item: Item_Record;
      Button: Ttk_Button;
   begin
      TemporaryStop := True;
      NewAction := SHOWTRASH;
      ToggleToolButtons(SHOWTRASH);
      ItemsList.Clear;
      CurrentDirectory :=
        To_Unbounded_String(Value("HOME") & "/.local/share/Trash/files");
      Open(Directory, Value("HOME") & "/.local/share/Trash/files");
      loop
         Read(Directory, FileName, Last);
         exit when Last = 0;
         if FileName(1 .. Last) = "." or FileName(1 .. Last) = ".." then
            goto End_Of_Loop;
         end if;
         FullName :=
           To_Unbounded_String
             (Value("HOME") & "/.local/share/Trash/files/" &
              FileName(1 .. Last));
         Item.Path := FullName;
         Open
           (FileInfo, In_File,
            Value("HOME") & "/.local/share/Trash/info/" & FileName(1 .. Last) &
            ".trashinfo");
         Skip_Line(FileInfo);
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Item.Name :=
                 To_Unbounded_String
                   (Simple_Name(Slice(FileLine, 6, Length(FileLine))));
            else
               FileLine := Unbounded_Slice(FileLine, 14, Length(FileLine));
               Replace_Slice(FileLine, 11, 11, " ");
               Item.Modified := Value(To_String(FileLine));
            end if;
         end loop;
         Close(FileInfo);
         if Is_Directory(To_String(FullName)) then
            Item.IsDirectory := True;
            if FileName(1) = '.' then
               Item.IsHidden := True;
            else
               Item.IsHidden := False;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Image := To_Unbounded_String("emblem-symbolic-link");
            else
               Item.Image := To_Unbounded_String("folder");
            end if;
            if Is_Read_Accessible_File(To_String(FullName)) then
               Open(SubDirectory, To_String(FullName));
               Size := 0;
               loop
                  Read(SubDirectory, SubFileName, SubLast);
                  exit when SubLast = 0;
                  Size := Size + 1;
               end loop;
               Close(SubDirectory);
               Item.Size := Item_Size(Size - 2);
            else
               Item.Size := -1;
            end if;
         else
            Item.IsDirectory := False;
            if FileName(1) = '.' then
               Item.IsHidden := True;
            else
               Item.IsHidden := False;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Image := To_Unbounded_String("emblem-symbolic-link");
            elsif Is_Executable_File(To_String(FullName)) then
               Item.Image := To_Unbounded_String("application-x-executable");
            else
               MimeType :=
                 To_Unbounded_String(GetMimeType(To_String(FullName)));
               if Index(MimeType, "audio") > 0 then
                  Item.Image := To_Unbounded_String("audio-x-generic");
               elsif Index(MimeType, "font") > 0 then
                  Item.Image := To_Unbounded_String("font-x-generic");
               elsif Index(MimeType, "image") > 0 then
                  Item.Image := To_Unbounded_String("image-x-generic");
               elsif Index(MimeType, "video") > 0 then
                  Item.Image := To_Unbounded_String("video-x-generic");
               elsif Index(MimeType, "text/x-script") > 0 then
                  Item.Image := To_Unbounded_String("text-x-script");
               elsif MimeType = To_Unbounded_String("text/html") then
                  Item.Image := To_Unbounded_String("text-html");
               elsif Index(MimeType, "zip") > 0 or
                 Index(MimeType, "x-xz") > 0 then
                  Item.Image := To_Unbounded_String("package-x-generic");
               elsif Index(MimeType, "text") > 0 then
                  Item.Image := To_Unbounded_String("text-x-generic");
               else
                  Item.Image := To_Unbounded_String("text-x-generic-template");
               end if;
            end if;
            if not Is_Read_Accessible_File(To_String(FullName)) then
               Item.Size := -1;
               ItemsList.Append(Item);
               goto End_Of_Loop;
            end if;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Size := -2;
            elsif Is_Regular_File(To_String(FullName)) then
               Item.Size :=
                 Item_Size(Ada.Directories.Size(To_String(FullName)));
            else
               Item.Size := 0;
            end if;
         end if;
         ItemsList.Append(Item);
         <<End_Of_Loop>>
      end loop;
      Close(Directory);
      UpdateDirectoryList(True);
      if ItemsList.Length = 0 then
         Button.Interp := Interp;
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.restorebutton");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.deletebutton");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
         Button.Name :=
           New_String(".mainframe.toolbars.actiontoolbar.separator3");
         Tcl.Tk.Ada.Pack.Pack_Forget(Button);
      end if;
      Bind_To_Main_Window
        (Interp, "<" & To_String(Accelerators(19)) & ">",
         "{InvokeButton .mainframe.toolbars.actiontoolbar.restorebutton}");
      return Show_Selected_Command(ClientData, Interp, Argc, Argv);
   end Show_Trash_Command;

   -- ****o* Trash/Restore_Item_Command
   -- FUNCTION
   -- Restore the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RestoreItem
   -- SOURCE
   function Restore_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Restore_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      RestoreInfo, FileLine, Destination, ItemType: Unbounded_String;
      StartIndex: Positive;
      FileInfo: File_Type;
   begin
      for Item of SelectedItems loop
         StartIndex := Index(Item, "files");
         RestoreInfo :=
           Unbounded_Slice(Item, 1, StartIndex - 1) & "info" &
           Unbounded_Slice(Item, StartIndex + 5, Length(Item)) &
           To_Unbounded_String(".trashinfo");
         Open(FileInfo, In_File, To_String(RestoreInfo));
         Skip_Line(FileInfo);
         for I in 1 .. 2 loop
            FileLine := To_Unbounded_String(Get_Line(FileInfo));
            if Slice(FileLine, 1, 4) = "Path" then
               Destination := Unbounded_Slice(FileLine, 6, Length(FileLine));
               if Ada.Directories.Exists(To_String(Destination)) then
                  if Is_Directory(To_String(Destination)) then
                     ItemType :=
                       To_Unbounded_String(Mc(Interp, "{Directory}"));
                  else
                     ItemType := To_Unbounded_String(Mc(Interp, "{File}"));
                  end if;
                  ShowMessage
                    (Mc(Interp, "{Can't restore }") & To_String(Destination) &
                     " " & To_String(ItemType) & " " &
                     Mc(Interp, "{with that name exists.}"));
                  Close(FileInfo);
                  return Show_Trash_Command(ClientData, Interp, Argc, Argv);
               end if;
               Rename(To_String(Item), Slice(FileLine, 6, Length(FileLine)));
            end if;
         end loop;
         Close(FileInfo);
         Delete_File(To_String(RestoreInfo));
      end loop;
      return Show_Trash_Command(ClientData, Interp, Argc, Argv);
   end Restore_Item_Command;

   -- ****o* Trash/Clear_Trash_Command
   -- FUNCTION
   -- Remove everything from the Trash
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ClearTrash
   -- SOURCE
   function Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Clear_Trash_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
   begin
      NewAction := CLEARTRASH;
      ToggleToolButtons(NewAction);
      ShowMessage
        (Mc(Interp, "{Remove all files and directories from Trash?}"),
         "question");
      return TCL_OK;
   end Clear_Trash_Command;

   procedure CreateTrashUI is
   begin
      AddCommand("ShowTrash", Show_Trash_Command'Access);
      AddCommand("RestoreItems", Restore_Item_Command'Access);
      AddCommand("ClearTrash", Clear_Trash_Command'Access);
   end CreateTrashUI;

end Trash;
