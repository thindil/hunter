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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
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
      SelectedItem: constant String := To_String(CurrentSelected);
      DirectorySize: Natural := 0;
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PreviewWindow);
      Box(PreviewWindow, Default_Character, Default_Character);
      Refresh(PreviewWindow);
      PreviewPad := New_Pad(Lines - 2, (Columns / 2) - 2);
      Add(PreviewPad, 0, Columns / 4, "Info" & LF);
      if not Is_Symbolic_Link(SelectedItem) then
         Add(PreviewPad, "Full path: " & Full_Name(SelectedItem) & LF);
      else
         Add(PreviewPad, "Links to: " & Full_Name(SelectedItem) & LF);
      end if;
      if Is_Directory(SelectedItem) then
         Add(PreviewPad, "Elements:");
      else
         Add(PreviewPad, "Size:");
      end if;
      if Is_Directory(SelectedItem) then
         if Settings.ShowHidden then
            Add
              (PreviewPad,
               Natural'Image(Natural(SecondItemsList.Length)) & LF);
         else
            Count_Directory_Size_Loop :
            for Item of SecondItemsList loop
               if not Item.IsHidden then
                  DirectorySize := DirectorySize + 1;
               end if;
            end loop Count_Directory_Size_Loop;
            Add(PreviewPad, Natural'Image(DirectorySize) & LF);
         end if;
      elsif Is_Regular_File(SelectedItem) then
         Add(PreviewPad, CountFileSize(Size(SelectedItem)) & LF);
      else
         Add(PreviewPad, "Unknown" & LF);
      end if;
      if Is_Directory(SelectedItem) or Is_Regular_File(SelectedItem) then
         Add
           (PreviewPad,
            "Last modified: " &
            Ada.Calendar.Formatting.Image
              (Modification_Time(SelectedItem), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset) &
            LF);
      else
         Add(PreviewPad, "Last modified: Unknown" & LF);
      end if;
      if Is_Regular_File(SelectedItem) then
         Add
           (PreviewPad,
            "File type: " & GetMimeType(Full_Name(SelectedItem)) & LF);
      end if;
      Refresh
        (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2), Columns - 3);
   end ShowInfo;

   procedure ShowPreview is
      Line: Line_Position := 1;
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PreviewWindow);
      Box(PreviewWindow, Default_Character, Default_Character);
      Refresh(PreviewWindow);
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
            Add(PreviewPad, Line, 0, To_String(Item.Name));
            Line := Line + 1;
            <<End_Of_Loop>>
         end loop;
         Refresh
           (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2), Columns - 3);
      else
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
         begin
            if MimeType(1 .. 4) = "text" then
               PreviewPad := New_Pad(Lines - 2, (Columns / 2) - 1);
               declare
                  ExecutableName: constant String :=
                    FindExecutable("highlight", False);
                  Success, FirstLine: Boolean;
                  File: File_Type;
                  FileLine: Unbounded_String := Null_Unbounded_String;
                  LinesAmount: Line_Position := 0;
                  LineLength: constant Positive := (Positive(Columns) / 2) - 2;
                  TagText, TagName: Unbounded_String;
                  StartIndex, EndIndex, StartColor: Natural;
                  Colors: array(1 .. 16) of String(1 .. 6) :=
                    (others => "      ");
                  procedure LoadFile is
                     FileLine: String(1 .. LineLength);
                     Amount: Natural;
                  begin
                     Open(File, In_File, To_String(CurrentSelected));
                     while not End_Of_File(File) loop
                        Get_Line(File, FileLine, Amount);
                        Add
                          (PreviewPad,
                           FileLine(FileLine'First .. Amount) & LF);
                        FileLine := (others => ' ');
                        LinesAmount := LinesAmount + 1;
                        exit when LinesAmount = Lines - 3;
                     end loop;
                     Close(File);
                     Refresh(PreviewWindow);
                     Refresh
                       (PreviewPad, 0, 0, 4, (Columns / 2) + 1, (Lines - 2),
                        Columns - 3);
                  end LoadFile;
                  procedure ShowText(Start, EndText: Positive) is
                     StartText: Positive := Start;
                     EndPos: Positive := Start + LineLength - 1;
                  begin
                     loop
                        if EndText > EndPos then
                           begin
                              Add
                                (PreviewPad,
                                 Slice(FileLine, StartText, EndPos) & LF);
                           exception
                              when Curses_Exception =>
                                 LinesAmount := Lines - 3;
                                 exit;
                           end;
                           StartText := StartText + LineLength;
                           EndPos := EndPos + LineLength;
                           LinesAmount := LinesAmount + 1;
                           exit when LinesAmount = Lines - 3;
                        elsif StartText <= EndText then
                           begin
                              Add
                                (PreviewPad,
                                 Slice(FileLine, StartText, EndText));
                           exception
                              when Curses_Exception =>
                                 LinesAmount := Lines - 3;
                           end;
                           exit;
                        else
                           exit;
                        end if;
                     end loop;
                  end ShowText;
               begin
                  if not Settings.ColorText or ExecutableName = "" then
                     LoadFile;
                     return;
                  end if;
                  Spawn
                    (ExecutableName,
                     Argument_String_To_List
                       ("--out-format=pango --force --quiet --output=" &
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
                  Read_File_Loop :
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
                        StartIndex := Index(FileLine, "<span", StartIndex);
                        exit when StartIndex = 0;
                        if StartIndex > 1 then
                           ShowText(1, StartIndex - 1);
                           exit Read_File_Loop when LinesAmount = Lines - 3;
                        end if;
                        EndIndex := Index(FileLine, ">", StartIndex);
                        TagText :=
                          Unbounded_Slice(FileLine, StartIndex, EndIndex);
                        StartColor := Index(TagText, "foreground=");
                        if Index(TagText, "foreground=") > 0 then
                           TagName :=
                             Unbounded_Slice
                               (TagText, StartColor + 13, StartColor + 18);
                        elsif Index(TagText, "style=""italic""") > 0 then
                           TagName := To_Unbounded_String("italic");
                        elsif Index(TagText, "weight=""bold""") > 0 then
                           TagName := To_Unbounded_String("bold");
                        end if;
                        StartIndex := StartIndex + Length(TagText);
                        EndIndex := Index(FileLine, "</span>", StartIndex) - 1;
                        -- Bold, italic, color text
                        if EndIndex > 0 then
                           if TagName = To_Unbounded_String("bold") then
                              Switch_Character_Attribute
                                (PreviewPad,
                                 (Bold_Character => True, others => False));
                           elsif TagName = To_Unbounded_String("italic") then
                              Switch_Character_Attribute
                                (PreviewPad,
                                 (Dim_Character => True, others => False));
                           elsif TagName /= Null_Unbounded_String then
                              for I in Colors'Range loop
                                 if Colors(I) = "      " then
                                    Init_Color
                                      (Color_Number(I + 7),
                                       RGB_Value'Value
                                         ("16#" & Slice(TagName, 1, 2) & "#"),
                                       RGB_Value'Value
                                         ("16#" & Slice(TagName, 3, 4) & "#"),
                                       RGB_Value'Value
                                         ("16#" & Slice(TagName, 5, 6) & "#"));
                                    Colors(I) := To_String(TagName);
                                    Set_Character_Attributes
                                      (PreviewPad, Normal_Video,
                                       Color_Pair(I));
                                    exit;
                                 elsif Colors(I) = To_String(TagName) then
                                    Set_Character_Attributes
                                      (PreviewPad, Normal_Video,
                                       Color_Pair(I));
                                    exit;
                                 end if;
                              end loop;
                           end if;
                           ShowText(StartIndex, EndIndex);
                           exit Read_File_Loop when LinesAmount = Lines - 3;
                           Set_Character_Attributes(PreviewPad);
                           TagName := Null_Unbounded_String;
                        else
                           ShowText(StartIndex, Length(FileLine));
                           exit Read_File_Loop when LinesAmount = Lines - 3;
                        end if;
                        StartIndex := 1;
                        FileLine :=
                          Unbounded_Slice
                            (FileLine, EndIndex + 8, Length(FileLine));
                     end loop;
                     if Length(FileLine) > 0 then
                        ShowText(1, Length(FileLine));
                        exit Read_File_Loop when LinesAmount = Lines - 3;
                     end if;
                     begin
                        Add(PreviewPad, "" & LF);
                     exception
                        when Curses_Exception =>
                           exit Read_File_Loop;
                     end;
                     LinesAmount := LinesAmount + 1;
                     exit Read_File_Loop when LinesAmount = Lines - 3;
                  end loop Read_File_Loop;
                  Close(File);
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
