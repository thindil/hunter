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
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages; use Messages;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
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

   procedure ShowInfo is
      SelectedItem: constant String := To_String(Current_Selected);
      DirectorySize: Natural := 0;
      MimeType: constant String := GetMimeType(SelectedItem);
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
         Add(PreviewPad, "File type: " & MimeType & LF);
         declare
            ProcessDesc: Process_Descriptor;
            Result: Expect_Match;
            ExecutableName: constant String := FindExecutable("xdg-mime");
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
                  Add
                    (PreviewPad,
                     "Associated program: " & To_String(DesktopFile) & LF);
               else
                  Add
                    (PreviewPad,
                     "Associated program: " & To_String(DesktopFile) & LF);
               end if;
            end if;
            Close(ProcessDesc);
         exception
            when Process_Died =>
               Add(PreviewPad, "Associated program: None" & LF);
         end;
      end if;
      declare
         Attributes: Unbounded_String;
         Tokens: Slice_Set;
         type Integer_Access is access Integer;
         Status: constant Integer_Access := new Integer;
         Arguments: constant Argument_List :=
           (new String'("-c%a %U %G"), new String'(SelectedItem));
         procedure SetPermissionsButtons
           (Name: String; Permission: Character) is
         begin
            Tcl.Ada.Tcl_SetVar(Interpreter, Name & "execute", "0");
            Tcl.Ada.Tcl_SetVar(Interpreter, Name & "read", "0");
            Tcl.Ada.Tcl_SetVar(Interpreter, Name & "write", "0");
            case Permission is
               when '1' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "execute", "1");
               when '2' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "write", "1");
               when '3' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "write", "1");
               when '4' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "read", "1");
               when '5' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "read", "1");
               when '6' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "read", "1");
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "write", "1");
               when '7' =>
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "execute", "1");
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "read", "1");
                  Tcl.Ada.Tcl_SetVar(Interpreter, Name & "write", "1");
               when others =>
                  null;
            end case;
            if not Is_Directory(SelectedItem) then
               if Tcl.Ada.Tcl_GetVar(Interpreter, Name & "execute") = "1" then
                  Add(PreviewPad, "    Can execute" & LF);
               else
                  Add(PreviewPad, "    Can't execute" & LF);
               end if;
            end if;
            if Tcl.Ada.Tcl_GetVar(Interpreter, Name & "write") = "1" then
               Add(PreviewPad, "    Can write" & LF);
            else
               Add(PreviewPad, "    Can't write" & LF);
            end if;
            if Tcl.Ada.Tcl_GetVar(Interpreter, Name & "read") = "1" then
               Add(PreviewPad, "    Can read" & LF);
            else
               Add(PreviewPad, "    Can't read" & LF);
            end if;
         end SetPermissionsButtons;
      begin
         Attributes :=
           To_Unbounded_String
             (Get_Command_Output("stat", Arguments, "", Status));
         Create(Tokens, To_String(Attributes), " ");
         Add(PreviewPad, "Owner: " & Slice(Tokens, 2) & LF);
         SetPermissionsButtons
           ("owner", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 2));
         Add(PreviewPad, "Group: " & Slice(Tokens, 3) & LF);
         SetPermissionsButtons
           ("group", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 1));
         Add(PreviewPad, "Others" & LF);
         SetPermissionsButtons
           ("others", Slice(Tokens, 1)(Slice(Tokens, 1)'Last));
      end;
      Refresh
        (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2), Columns - 3);
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
      if Is_Directory(To_String(Current_Selected)) then
         if not Is_Read_Accessible_File(To_String(Current_Selected)) then
            ShowMessage
              (Mc
                 (Interpreter,
                  "{You don't have permissions to preview this directory.}"));
            return;
         end if;
         LoadDirectory(To_String(Current_Selected), True);
         PreviewPad :=
           New_Pad
             (Line_Position(SecondItemsList.Length) + 1, (Columns / 2) - 1);
         Add(PreviewPad, 0, Columns / 4, "Name");
         Load_Preview_Directory_Loop :
         for Item of SecondItemsList loop
            if not Settings.ShowHidden and Item.IsHidden then
               goto End_Of_Loop;
            end if;
            Add(PreviewPad, Line, 0, To_String(Item.Name));
            Line := Line + 1;
            <<End_Of_Loop>>
         end loop Load_Preview_Directory_Loop;
         Refresh
           (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2), Columns - 3);
      else
         declare
            MimeType: constant String :=
              GetMimeType(To_String(Current_Selected));
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
                     Open(File, In_File, To_String(Current_Selected));
                     Load_Text_File_View_Loop :
                     while not End_Of_File(File) loop
                        Get_Line(File, FileLine, Amount);
                        Add
                          (PreviewPad,
                           FileLine(FileLine'First .. Amount) & LF);
                        FileLine := (others => ' ');
                        LinesAmount := LinesAmount + 1;
                        exit Load_Text_File_View_Loop when LinesAmount =
                          Lines - 3;
                     end loop Load_Text_File_View_Loop;
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
                     Print_Text_Loop :
                     loop
                        if EndText > EndPos then
                           begin
                              Add
                                (PreviewPad,
                                 Slice(FileLine, StartText, EndPos) & LF);
                           exception
                              when Curses_Exception =>
                                 LinesAmount := Lines - 3;
                                 exit Print_Text_Loop;
                           end;
                           StartText := StartText + LineLength;
                           EndPos := EndPos + LineLength;
                           LinesAmount := LinesAmount + 1;
                           exit Print_Text_Loop when LinesAmount = Lines - 3;
                        elsif StartText <= EndText then
                           begin
                              Add
                                (PreviewPad,
                                 Slice(FileLine, StartText, EndText));
                           exception
                              when Curses_Exception =>
                                 LinesAmount := Lines - 3;
                           end;
                           exit Print_Text_Loop;
                        else
                           exit Print_Text_Loop;
                        end if;
                     end loop Print_Text_Loop;
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
                        To_String(Current_Selected)).all,
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
                     exit Read_File_Loop when End_Of_File(File);
                     Replace_Element_Loop :
                     loop
                        StartIndex := Index(FileLine, "&gt;");
                        exit Replace_Element_Loop when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 3, ">");
                     end loop Replace_Element_Loop;
                     Replace_Element_2_Loop :
                     loop
                        StartIndex := Index(FileLine, "&lt;");
                        exit Replace_Element_2_Loop when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 3, "<");
                     end loop Replace_Element_2_Loop;
                     Replace_Element_3_Loop :
                     loop
                        StartIndex := Index(FileLine, "&amp;");
                        exit Replace_Element_3_Loop when StartIndex = 0;
                        Replace_Slice
                          (FileLine, StartIndex, StartIndex + 4, "&");
                     end loop Replace_Element_3_Loop;
                     StartIndex := 1;
                     Highlight_Text_Loop :
                     loop
                        StartIndex := Index(FileLine, "<span", StartIndex);
                        exit Highlight_Text_Loop when StartIndex = 0;
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
                              Set_Colors_Loop :
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
                                    exit Set_Colors_Loop;
                                 elsif Colors(I) = To_String(TagName) then
                                    Set_Character_Attributes
                                      (PreviewPad, Normal_Video,
                                       Color_Pair(I));
                                    exit Set_Colors_Loop;
                                 end if;
                              end loop Set_Colors_Loop;
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
                     end loop Highlight_Text_Loop;
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
                    (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2),
                     Columns - 3);
                  Delete_File(Value("HOME") & "/.cache/hunter/highlight.tmp");
               end;
            else
               ShowInfo;
            end if;
         end;
      end if;
   end ShowPreview;

   -- ****iv* ShowItemsTUI/ShowItemsTUI.Path
   -- FUNCTION
   -- Path buttons menu for destination directory
   -- SOURCE
   Path: Menu;
   -- ****

   -- ****iv* ShowItemsTUI/ShowItemsTUI.PathButtons
   -- FUNCTION
   -- Path buttons window for destination directory
   -- SOURCE
   PathButtons: Window;
   -- ****

   -- ****iv* ShowItems/Buttons_Visible
   -- FUNCTION
   -- If True, destination path buttons are visible
   -- SOURCE
   Buttons_Visible: Boolean := False;
   -- ****

   procedure Show_Selected is
   begin
      if Buttons_Visible then
         Post(Path, False);
         Refresh(PathButtons);
         Buttons_Visible := False;
      end if;
      SelectedItems.Clear;
      if Item_Count(DirectoryList) > 0 then
         Update_Selected_Items_Loop :
         for I in 1 .. Item_Count(DirectoryList) loop
            if Value(Items(DirectoryList, I)) or
              Current(DirectoryList) = Items(DirectoryList, I) then
               SelectedItems.Append
                 (To_Unbounded_String(Description(Items(DirectoryList, I))));
            end if;
         end loop Update_Selected_Items_Loop;
      else
         SelectedItems.Append(MainWindow.Current_Directory);
      end if;
      if not Settings.ShowPreview or
        (SelectedItems(1) = Current_Selected and
         Current_Selected /= MainWindow.Current_Directory) then
         return;
      end if;
      Current_Selected :=
        MainWindow.Current_Directory & "/" &
        Description(Current(DirectoryList));
      if New_Action = CREATELINK then
         return;
      end if;
      if Is_Directory(To_String(Current_Selected)) or
        Is_Regular_File(To_String(Current_Selected)) then
         ShowPreview;
      else
         ShowInfo;
      end if;
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Permissions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function GoToDirectory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      return TCL_OK;
   end GoToDirectory_Command;

   procedure CreateShowItemsUI is
   begin
      AddCommand("SetPermissions", Set_Permissions_Command'Access);
      AddCommand("GoToDirectory", GoToDirectory_Command'Access);
      PathButtons := Create(1, Columns / 2, 1, 0);
      PreviewWindow := Create(Lines - 2, Columns / 2, 2, Columns / 2);
      Box(PreviewWindow, Default_Character, Default_Character);
      Refresh(PreviewWindow);
   end CreateShowItemsUI;

   procedure ShowDestination is
      Index: Positive;
      Path_Items: Item_Array_Access;
      Tokens: Slice_Set;
      Menu_Items: constant Item_Array_Access :=
        new Item_Array
          (SecondItemsList.First_Index .. SecondItemsList.Last_Index + 1);
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PathButtons);
      Clear(PreviewWindow);
      Box(PreviewWindow, Default_Character, Default_Character);
      if not Is_Read_Accessible_File(To_String(DestinationDirectory)) then
         ShowMessage
           (Mc
              (Interpreter,
               "{You don't have permissions to preview this directory.}"));
         return;
      end if;
      DestinationDirectory :=
        To_Unbounded_String
          (Normalize_Pathname(To_String(DestinationDirectory)));
      Index := Ada.Strings.Unbounded.Count(DestinationDirectory, "/") + 1;
      if DestinationDirectory /= To_Unbounded_String("/") then
         Path_Items := new Item_Array(1 .. Index + 1);
         Path_Items.all(1) := New_Item("/");
         Create(Tokens, To_String(DestinationDirectory), "/");
         for I in 2 .. Slice_Count(Tokens) loop
            Path_Items.all(Positive(I)) := New_Item(Slice(Tokens, I));
         end loop;
         Path_Items.all(Index + 1) := Null_Item;
      else
         Path_Items := new Item_Array(1 .. 2);
         Path_Items.all(1) := New_Item("/");
         Path_Items.all(2) := Null_Item;
         Index := 1;
      end if;
      Path := New_Menu(Path_Items);
      Set_Format(Path, 1, 5);
      Set_Mark(Path, "");
      Set_Window(Path, PathButtons);
      Set_Sub_Window
        (Path, Derived_Window(PathButtons, 1, (Columns / 2) - 2, 0, 1));
      Post(Path);
      Buttons_Visible := True;
      Set_Current(Path, Path_Items.all(Index));
      Move_Window(PathButtons, 1, (Columns / 2));
      Index := SecondItemsList.First_Index;
      Load_Destination_View_Loop :
      for I in SecondItemsList.First_Index .. SecondItemsList.Last_Index loop
         if not Settings.ShowHidden and SecondItemsList(I).IsHidden then
            goto End_Of_Loop;
         end if;
         Menu_Items.all(Index) := New_Item(To_String(SecondItemsList(I).Name));
         Index := Index + 1;
         <<End_Of_Loop>>
      end loop Load_Destination_View_Loop;
      if Index > SecondItemsList.First_Index then
         Fill_Empty_Entries_Loop :
         for I in Index .. Menu_Items'Last loop
            Menu_Items.all(I) := Null_Item;
         end loop Fill_Empty_Entries_Loop;
         DestinationList := New_Menu(Menu_Items);
         Set_Format(DestinationList, Lines - 5, 1);
         Set_Mark(DestinationList, "");
         Set_Window(DestinationList, PreviewWindow);
         Set_Sub_Window
           (DestinationList,
            Derived_Window(PreviewWindow, Lines - 5, (Columns / 2) - 2, 2, 1));
         Post(DestinationList);
         Set_Current(DestinationList, Menu_Items.all(1));
      end if;
      Refresh(PathButtons);
      Refresh(PreviewWindow);
   end ShowDestination;

   procedure ShowOutput is
   begin
      null;
   end ShowOutput;

   procedure UpdateOutput(Text: String) is
   begin
      null;
   end UpdateOutput;

   procedure Destination_Keys(Key: Key_Code) is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(DestinationList, M_Up_Item);
         when KEY_DOWN =>
            Result := Driver(DestinationList, M_Down_Item);
         when Key_Home =>
            Result := Driver(DestinationList, M_First_Item);
         when Key_End =>
            Result := Driver(DestinationList, M_Last_Item);
         when KEY_NPAGE =>
            Result := Driver(DestinationList, M_ScrollUp_Page);
         when KEY_PPAGE =>
            Result := Driver(DestinationList, M_ScrollDown_Page);
         when 10 =>
            DestinationDirectory :=
              DestinationDirectory & "/" & Name(Current(DestinationList));
            LoadDirectory(To_String(DestinationDirectory), True);
            ShowDestination;
            return;
         when others =>
            return;
      end case;
      if Result = Menu_Ok then
         Refresh(PreviewWindow);
      end if;
   end Destination_Keys;

   function Destination_Path_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when KEY_LEFT =>
            Result := Driver(Path, M_Previous_Item);
         when KEY_RIGHT =>
            Result := Driver(Path, M_Next_Item);
         when Key_Home =>
            Result := Driver(Path, M_First_Item);
         when Key_End =>
            Result := Driver(Path, M_Last_Item);
         when 10 =>
            DestinationDirectory := To_Unbounded_String("/");
            Update_Destination_Directory_Loop :
            for I in 2 .. Get_Index(Current(Path)) loop
               Append(DestinationDirectory, Name(Items(Path, I)));
               if I < Get_Index(Current(Path)) then
                  Append(DestinationDirectory, "/");
               end if;
            end loop Update_Destination_Directory_Loop;
            LoadDirectory(To_String(DestinationDirectory), True);
            ShowDestination;
            return DESTINATION_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(PathButtons);
      end if;
      return DESTINATION_PATH;
   end Destination_Path_Keys;

end ShowItems;
