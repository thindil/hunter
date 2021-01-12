-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with CArgv;
with Tcl; use Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Preferences; use Preferences;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body ShowItems is

   -- ****iv* ShowItemsTUI/ShowItemsTUI.PreviewWindow
   -- FUNCTION
   -- Window used for show preview/info about the selected item
   -- SOURCE
   PreviewWindow: Window;
   -- ****

   -- ****iv* ShowItemsTUI/ShowItemsTUI.PreviewPad
   -- FUNCTION
   -- Window used to store content of preview/info of the selected item
   -- SOURCE
   PreviewPad: Window;
   -- ****

   -- ****if* ShowItemsTUI/ShowItemsTUI.ShowInfo
   -- FUNCTION
   -- Show information about the currently selected file or directory.
   -- SOURCE
   procedure ShowInfo is
   -- ****
   begin
      null;
   end ShowInfo;

   procedure ShowPreview is
      Line: Line_Position := 1;
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      if Is_Directory(To_String(CurrentSelected)) then
         if not Is_Read_Accessible_File(To_String(CurrentSelected)) then
            ShowMessage
              (Mc
                 (Interpreter,
                  "{You don't have permissions to preview this directory.}"));
         end if;
         LoadDirectory(To_String(CurrentSelected), True);
         PreviewPad :=
           New_Pad
             (Line_Position(SecondItemsList.Length) + 1, (Columns / 2) - 1);
         Add(PreviewPad, 0, Columns / 4, "Name");
         for Item of SecondItemsList loop
            if not Settings.ShowHidden and Item.IsHidden then
               goto End_Of_Loop;
            end if;
            Add(PreviewPad, Line, 1, To_String(Item.Name));
            Line := Line + 1;
            <<End_Of_Loop>>
         end loop;
         Clear(PreviewWindow);
         Box(PreviewWindow, Default_Character, Default_Character);
         Refresh(PreviewWindow);
         Refresh
           (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2), Columns - 3);
      else
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
         begin
            if MimeType(1 .. 4) = "text" then
               declare
                  ExecutableName: constant String :=
                    FindExecutable("highlight", False);
                  Success, FirstLine: Boolean;
                  File: File_Type;
                  FileText, FileLine: Unbounded_String :=
                    Null_Unbounded_String;
                  LinesAmount: Line_Position := 0;
                  LineLength: Column_Position := 1;
                  TagText, TagName: Unbounded_String;
                  StartIndex, EndIndex, StartColor: Natural;
                  procedure LoadFile is
                  begin
                     Open(File, In_File, To_String(CurrentSelected));
                     while not End_Of_File(File) loop
                        FileLine := To_Unbounded_String(Get_Line(File));
                        if Length(FileLine) > Natural(LineLength) then
                           LineLength := Column_Position(Length(FileLine));
                        end if;
                        Append(FileText, FileLine & LF);
                        LinesAmount := LinesAmount + 1;
                     end loop;
                     Close(File);
                     PreviewPad := New_Pad(LinesAmount + 1, LineLength + 1);
                     Add(PreviewPad, To_String(FileText));
                     Refresh(PreviewWindow);
                     Refresh
                       (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2),
                        Columns - 3);
                  end LoadFile;
               begin
                  if not Settings.ColorText or ExecutableName = "" then
                     LoadFile;
                     return;
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
                     return;
                  end if;
                  Open
                    (File, In_File,
                     Value("HOME") & "/.cache/hunter/highlight.tmp");
                  FirstLine := True;
                  while not End_Of_File(File) loop
                     FileLine := To_Unbounded_String(Get_Line(File));
                     if Length(FileLine) > Natural(LineLength) then
                        LineLength := Column_Position(Length(FileLine));
                     end if;
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
                           Append
                             (FileText, Slice(FileLine, 1, StartIndex - 1));
                        end if;
                        EndIndex := Index(FileLine, ">", StartIndex);
                        TagText :=
                          Unbounded_Slice(FileLine, StartIndex, EndIndex);
                        StartColor := Index(TagText, "foreground=");
                        if Index(TagText, "foreground=") > 0 then
                           TagName :=
                             Unbounded_Slice
                               (TagText, StartColor + 12, StartColor + 18);
                        elsif Index(TagText, "style=""italic""") > 0 then
                           TagName := To_Unbounded_String("italictag");
                        elsif Index(TagText, "weight=""bold""") > 0 then
                           TagName := To_Unbounded_String("boldtag");
                        end if;
                        StartIndex := StartIndex + Length(TagText);
                        EndIndex := Index(FileLine, "</span>", StartIndex) - 1;
                        -- Bold, italic, color text
                        if EndIndex > 0 then
                           Append
                             (FileText, Slice(FileLine, StartIndex, EndIndex));
                        else
                           Append
                             (FileText,
                              Slice(FileLine, StartIndex, Length(FileLine)));
                        end if;
                        StartIndex := 1;
                        FileLine :=
                          Unbounded_Slice
                            (FileLine, EndIndex + 8, Length(FileLine));
                     end loop;
                     Append(FileText, FileLine & LF);
                     LinesAmount := LinesAmount + 1;
                  end loop;
                  Close(File);
                  PreviewPad := New_Pad(LinesAmount + 4, LineLength + 1);
                  Add(PreviewPad, To_String(FileText));
                  Refresh(PreviewWindow);
                  Refresh
                    (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2),
                     Columns - 3);
                  Delete_File(Value("HOME") & "/.cache/hunter/highlight.tmp");
               end;
            else
               ShowInfo;
            end if;
         end;
      end if;
   end ShowPreview;

   procedure Show_Selected is
   begin
      SelectedItems.Clear;
      if Item_Count(DirectoryList) > 0 then
         for I in 1 .. Item_Count(DirectoryList) loop
            if Value(Items(DirectoryList, I)) or
              Current(DirectoryList) = Items(DirectoryList, I) then
               SelectedItems.Append
                 (To_Unbounded_String(Name(Items(DirectoryList, I))));
            end if;
         end loop;
      else
         SelectedItems.Append(CurrentDirectory);
      end if;
      if not Settings.ShowPreview or
        (SelectedItems(1) = CurrentSelected and
         CurrentSelected /= CurrentDirectory) then
         return;
      end if;
      CurrentSelected := CurrentDirectory & "/" & Name(Current(DirectoryList));
      if NewAction = CREATELINK then
         return;
      end if;
      if Is_Directory(To_String(CurrentSelected)) or
        Is_Regular_File(To_String(CurrentSelected)) then
         ShowPreview;
      else
         ShowInfo;
      end if;
      Refresh;
   end Show_Selected;

   -- ****o* ShowItemsTUI/ShowItemsTUI.Set_Permissions_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Permissions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end Set_Permissions_Command;

   -- ****o* ShowItemsTUI/ShowItemsTUI.GoToDirectory_Command
   -- FUNCTION
   -- Go to the selected directory in preview
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GoToDirectory ?selecteditem?
   -- Selecteditem is full path to the currently selected file or directory
   -- SOURCE
   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToDirectory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end GoToDirectory_Command;

   procedure CreateShowItemsUI is
   begin
      AddCommand("SetPermissions", Set_Permissions_Command'Access);
      AddCommand("GoToDirectory", GoToDirectory_Command'Access);
      PreviewWindow := Create(Lines - 3, Columns / 2, 3, Columns / 2);
      Box(PreviewWindow, Default_Character, Default_Character);
      Refresh(PreviewWindow);
   end CreateShowItemsUI;

   procedure ShowDestination is
   begin
      null;
   end ShowDestination;

   procedure ShowOutput is
   begin
      null;
   end ShowOutput;

   procedure UpdateOutput(Text: String) is
   begin
      null;
   end UpdateOutput;

end ShowItems;
