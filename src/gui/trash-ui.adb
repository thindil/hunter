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

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Inotify; use Inotify;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Modules; use Modules;
with Preferences; use Preferences;
with ShowItems; use ShowItems;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Trash.UI is

   function Show_Trash_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Directory, SubDirectory: Dir_Type;
      FileName, SubFileName: String(1 .. 1_024);
      Last: Natural range 0 .. FileName'Last;
      SubLast: Natural range 0 .. SubFileName'Last;
      FileInfo: File_Type;
      Size: File_Size;
      FileLine, FullName, MimeType: Unbounded_String;
      Item: Item_Record;
      Button: Ttk_Button :=
        Get_Widget(".mainframe.toolbars.actiontoolbar.restorebutton");
   begin
      Temporary_Stop := True;
      Create_Path
        (Ada.Environment_Variables.Value("HOME") & "/.local/share/Trash/info");
      Create_Path
        (Ada.Environment_Variables.Value("HOME") &
         "/.local/share/Trash/files");
      if New_Action /= SHOWTRASH then
         New_Action := SHOWTRASH;
         Toggle_Tool_Buttons(SHOWTRASH);
      end if;
      ItemsList.Clear;
      MainWindow.Current_Directory :=
        To_Unbounded_String(Value("HOME") & "/.local/share/Trash/files");
      DestinationDirectory :=
        Delete
          (MainWindow.Current_Directory, 1,
           Length
             (To_Unbounded_String
                (Value("HOME") & "/.local/share/Trash/files")));
      Open(Directory, Value("HOME") & "/.local/share/Trash/files");
      Read_Trash_Content_Loop :
      loop
         Read(Directory, FileName, Last);
         exit Read_Trash_Content_Loop when Last = 0;
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
         Read_File_Path_Loop :
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
         end loop Read_File_Path_Loop;
         Close(FileInfo);
         Item.IsHidden := (if FileName(1) = '.' then True else False);
         if Is_Directory(To_String(FullName)) then
            Item.IsDirectory := True;
            Item.Image :=
              (if Is_Symbolic_Link(To_String(FullName)) then
                 To_Unbounded_String("emblem-symbolic-link")
               else To_Unbounded_String("folder"));
            if Is_Read_Accessible_File(To_String(FullName)) then
               Open(SubDirectory, To_String(FullName));
               Size := 0;
               Count_Directory_Size_Loop :
               loop
                  Read(SubDirectory, SubFileName, SubLast);
                  exit Count_Directory_Size_Loop when SubLast = 0;
                  Size := Size + 1;
               end loop Count_Directory_Size_Loop;
               Close(SubDirectory);
               Item.Size := Item_Size(Size - 2);
            else
               Item.Size := -1;
            end if;
         else
            Item.IsDirectory := False;
            if Is_Symbolic_Link(To_String(FullName)) then
               Item.Image := To_Unbounded_String("emblem-symbolic-link");
            elsif Is_Executable_File(To_String(FullName)) then
               Item.Image := To_Unbounded_String("application-x-executable");
            else
               MimeType :=
                 To_Unbounded_String(Get_Mime_Type(To_String(FullName)));
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
      end loop Read_Trash_Content_Loop;
      Close(Directory);
      Update_Directory_List(True);
      if ItemsList.Length = 0 then
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
      Execute_Modules
        (On_Enter, "{" & To_String(MainWindow.Current_Directory) & "}");
      return Show_Selected_Command(ClientData, Interp, Argc, Argv);
   end Show_Trash_Command;

   procedure CreateTrashUI is
   begin
      Add_Command("ShowTrash", Show_Trash_Command'Access);
   end CreateTrashUI;

end Trash.UI;
