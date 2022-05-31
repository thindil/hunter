-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData; use LoadData;
with LoadData.UI; use LoadData.UI;
with Messages.UI; use Messages.UI;
with Preferences; use Preferences;
with ProgramsMenu; use ProgramsMenu;
with ProgramsMenu.UI; use ProgramsMenu.UI;
with Utils; use Utils;

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

   -- ****iv* ShowItemsTUI/ShowItemsTUI.FormWindow
   -- FUNCTION
   -- Window used to store content of the permissions of the selected item
   -- SOURCE
   FormWindow: Window;
   -- ****

   -- ****if* ShowItemsTUI/ShowItemsTUI.Get_Item_Status
   -- FUNCTION
   -- Get information about permissions, owner and group for the selected
   -- file or directory
   -- PARAMETERS
   -- File_Name - Name of the file or directory which information will be get
   -- Args      - The list of arguments which will be passed to stat
   -- RESULT
   -- Unbounded_String with information about permissions, owner and group of
   -- the selected file or directory
   -- SOURCE
   function Get_Item_Status(File_Name, Args: String) return Unbounded_String is
      -- ****
      type Integer_Access is access Integer;
      Status: constant Integer_Access := new Integer;
      Arguments: constant Argument_List :=
        (new String'(Args), new String'(File_Name));
   begin
      return
        To_Unbounded_String(Get_Command_Output("stat", Arguments, "", Status));
   end Get_Item_Status;

   procedure ShowInfo is
      SelectedItem: constant String := To_String(Current_Selected);
      DirectorySize: Natural := 0;
      MimeType: constant String := Get_Mime_Type(SelectedItem);
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PreviewWindow);
      if Ui_Location = PREVIEW then
         Box(PreviewWindow, Default_Character, Default_Character);
      end if;
      Refresh(PreviewWindow);
      PreviewPad := New_Pad(Lines - 2, (Columns / 2) - 2);
      Add(PreviewPad, 0, Columns / 4, Mc(Interpreter, "Info") & LF);
      if not Is_Symbolic_Link(SelectedItem) then
         Add
           (PreviewPad,
            Mc(Interpreter, "{Full path:}") & " " & Full_Name(SelectedItem) &
            LF);
      else
         Add
           (PreviewPad,
            Mc(Interpreter, "{Links to:}") & " " & Full_Name(SelectedItem) &
            LF);
      end if;
      if Is_Directory(SelectedItem) then
         Add(PreviewPad, Mc(Interpreter, "Elements:"));
      else
         Add(PreviewPad, Mc(Interpreter, "Size:"));
      end if;
      if Is_Directory(SelectedItem) then
         if Settings.Show_Hidden then
            Add
              (PreviewPad,
               Natural'Image(Natural(Second_Items_List.Length)) & LF);
         else
            Count_Directory_Size_Loop :
            for Item of Second_Items_List loop
               if not Item.Is_Hidden then
                  DirectorySize := DirectorySize + 1;
               end if;
            end loop Count_Directory_Size_Loop;
            Add(PreviewPad, Natural'Image(DirectorySize) & LF);
         end if;
      elsif Is_Regular_File(SelectedItem) then
         Add(PreviewPad, Count_File_Size(Size(SelectedItem)) & LF);
      else
         Add(PreviewPad, Mc(Interpreter, "Unknown") & LF);
      end if;
      if Is_Directory(SelectedItem) or Is_Regular_File(SelectedItem) then
         Add
           (PreviewPad,
            Mc(Interpreter, "{Last modified:}") & " " &
            Ada.Calendar.Formatting.Image
              (Modification_Time(SelectedItem), False,
               Ada.Calendar.Time_Zones.UTC_Time_Offset) &
            LF);
      else
         Add
           (PreviewPad,
            Mc(Interpreter, "{Last modified:}") & " " &
            Mc(Interpreter, "Unknown") & LF);
      end if;
      if Is_Regular_File(SelectedItem) then
         Add
           (PreviewPad, Mc(Interpreter, "{File type:}") & " " & MimeType & LF);
      end if;
      declare
         Attributes: Unbounded_String;
         Tokens: Slice_Set;
         Permissions_Fields: constant Field_Array_Access :=
           (if Is_Directory(SelectedItem) then new Field_Array(1 .. 12)
            else new Field_Array(1 .. 15));
         FormHeight: Line_Position;
         FormLength: Column_Position;
         FieldOptions: Field_Option_Set;
         UnusedResult: Forms.Driver_Result := Unknown_Request;
         procedure SetPermissionsButtons
           (Name: String; Permission: Character; FieldNumber: Positive) is
            NewFieldNumber: Positive := FieldNumber;
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
                  Set_Buffer
                    (Permissions_Fields.all(NewFieldNumber), 0,
                     Mc(Interpreter, "{Can execute}"));
               else
                  Set_Buffer
                    (Permissions_Fields.all(NewFieldNumber), 0,
                     Mc(Interpreter, "{Can't execute}"));
               end if;
               NewFieldNumber := NewFieldNumber + 1;
            end if;
            if Tcl.Ada.Tcl_GetVar(Interpreter, Name & "write") = "1" then
               Set_Buffer
                 (Permissions_Fields.all(NewFieldNumber), 0,
                  Mc(Interpreter, "{Can write}"));
            else
               Set_Buffer
                 (Permissions_Fields.all(NewFieldNumber), 0,
                  Mc(Interpreter, "{Can't write}"));
            end if;
            NewFieldNumber := NewFieldNumber + 1;
            if Tcl.Ada.Tcl_GetVar(Interpreter, Name & "read") = "1" then
               Set_Buffer
                 (Permissions_Fields.all(NewFieldNumber), 0,
                  Mc(Interpreter, "{Can read}"));
            else
               Set_Buffer
                 (Permissions_Fields.all(NewFieldNumber), 0,
                  Mc(Interpreter, "{Can't read}"));
            end if;
         end SetPermissionsButtons;
      begin
         Attributes := Get_Item_Status(SelectedItem, "-c%a %U %G");
         Create(Tokens, To_String(Attributes), " ");
         Permissions_Fields.all(1) := New_Field(1, 30, 0, 0, 0, 0);
         FieldOptions := Get_Options(Permissions_Fields.all(1));
         FieldOptions.Active := False;
         Set_Options(Permissions_Fields.all(1), FieldOptions);
         Permissions_Fields.all(2) := New_Field(1, 20, 0, 20, 0, 0);
         if Is_Regular_File(SelectedItem) or Is_Directory(SelectedItem) then
            Set_Buffer
              (Permissions_Fields.all(1), 0,
               Mc(Interpreter, "{Associated program:}") & " ");
            declare
               ProcessDesc: Process_Descriptor;
               Result: Expect_Match;
               ExecutableName: constant String := Find_Executable("xdg-mime");
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
                    To_Unbounded_String
                      (Get_Program_Name(To_String(DesktopFile)));
                  if Index(DesktopFile, ".desktop") = 0 then
                     Set_Buffer
                       (Permissions_Fields.all(2), 0, To_String(DesktopFile));
                  else
                     Set_Buffer
                       (Permissions_Fields.all(2), 0, To_String(DesktopFile));
                  end if;
               end if;
               Close(ProcessDesc);
            exception
               when Process_Died =>
                  Set_Buffer
                    (Permissions_Fields.all(2), 0, Mc(Interpreter, "None"));
            end;
         end if;
         Permissions_Fields.all(3) := New_Field(1, 30, 2, 0, 0, 0);
         Set_Buffer
           (Permissions_Fields.all(3), 0,
            Mc(Interpreter, "{Owner}") & ": " & Slice(Tokens, 2));
         FieldOptions := Get_Options(Permissions_Fields.all(3));
         FieldOptions.Active := False;
         Set_Options(Permissions_Fields.all(3), FieldOptions);
         Permissions_Fields.all(4) := New_Field(1, 30, 3, 4, 0, 0);
         Permissions_Fields.all(5) := New_Field(1, 30, 4, 4, 0, 0);
         if Is_Directory(SelectedItem) then
            Permissions_Fields.all(6) := New_Field(1, 30, 5, 0, 0, 0);
            Set_Buffer
              (Permissions_Fields.all(6), 0,
               Mc(Interpreter, "{Group}") & ": " & Slice(Tokens, 3));
            FieldOptions := Get_Options(Permissions_Fields.all(6));
            FieldOptions.Active := False;
            Set_Options(Permissions_Fields.all(6), FieldOptions);
            Permissions_Fields.all(7) := New_Field(1, 30, 6, 4, 0, 0);
            Permissions_Fields.all(8) := New_Field(1, 30, 7, 4, 0, 0);
            Permissions_Fields.all(9) := New_Field(1, 30, 8, 0, 0, 0);
            Set_Buffer
              (Permissions_Fields.all(9), 0, Mc(Interpreter, "Others") & ":");
            FieldOptions := Get_Options(Permissions_Fields.all(9));
            FieldOptions.Active := False;
            Set_Options(Permissions_Fields.all(9), FieldOptions);
            Permissions_Fields.all(10) := New_Field(1, 30, 9, 4, 0, 0);
            Permissions_Fields.all(11) := New_Field(1, 30, 10, 4, 0, 0);
            Permissions_Fields.all(12) := Null_Field;
            SetPermissionsButtons
              ("owner", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 2), 4);
            SetPermissionsButtons
              ("group", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 1), 7);
            SetPermissionsButtons
              ("others", Slice(Tokens, 1)(Slice(Tokens, 1)'Last), 10);
         else
            Permissions_Fields.all(6) := New_Field(1, 30, 5, 4, 0, 0);
            SetPermissionsButtons
              ("owner", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 2), 4);
            Permissions_Fields.all(7) := New_Field(1, 30, 6, 0, 0, 0);
            Set_Buffer
              (Permissions_Fields.all(7), 0,
               Mc(Interpreter, "{Group}") & ": " & Slice(Tokens, 3));
            FieldOptions := Get_Options(Permissions_Fields.all(7));
            FieldOptions.Active := False;
            Set_Options(Permissions_Fields.all(7), FieldOptions);
            Permissions_Fields.all(8) := New_Field(1, 30, 7, 4, 0, 0);
            Permissions_Fields.all(9) := New_Field(1, 30, 8, 4, 0, 0);
            Permissions_Fields.all(10) := New_Field(1, 30, 9, 4, 0, 0);
            Permissions_Fields.all(11) := New_Field(1, 30, 10, 0, 0, 0);
            Set_Buffer
              (Permissions_Fields.all(11), 0, Mc(Interpreter, "Others") & ":");
            FieldOptions := Get_Options(Permissions_Fields.all(11));
            FieldOptions.Active := False;
            Set_Options(Permissions_Fields.all(11), FieldOptions);
            Permissions_Fields.all(12) := New_Field(1, 30, 11, 4, 0, 0);
            Permissions_Fields.all(13) := New_Field(1, 30, 12, 4, 0, 0);
            Permissions_Fields.all(14) := New_Field(1, 30, 13, 4, 0, 0);
            Permissions_Fields.all(15) := Null_Field;
            SetPermissionsButtons
              ("group", Slice(Tokens, 1)(Slice(Tokens, 1)'Last - 1), 8);
            SetPermissionsButtons
              ("others", Slice(Tokens, 1)(Slice(Tokens, 1)'Last), 12);
         end if;
         Info_Form := New_Form(Permissions_Fields);
         Set_Current(Info_Form, Permissions_Fields(2));
         Set_Options(Info_Form, (others => False));
         Scale(Info_Form, FormHeight, FormLength);
         FormWindow :=
           (if Is_Directory(SelectedItem) then
              Create(FormHeight + 2, FormLength + 2, 7, (Columns / 2) + 1)
            else Create(FormHeight + 2, FormLength + 2, 9, (Columns / 2) + 1));
         Set_Window(Info_Form, FormWindow);
         Set_Sub_Window
           (Info_Form,
            Derived_Window(FormWindow, FormHeight, FormLength, 1, 1));
         Post(Info_Form);
         UnusedResult := Driver(Info_Form, REQ_END_LINE);
      end;
      Refresh
        (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2), Columns - 3);
      Refresh(FormWindow);
   end ShowInfo;

   -- ****iv* ShowItemsTUI/ShowItemsTUI.Current_Line
   -- FUNCTION
   -- The starting line for showing the selected file preview
   -- SOURCE
   Start_Line: Positive := 1;
   -- ****

   procedure Show_Preview(Reset_Preview: Boolean := True) is
      Line: Line_Position := 1;
      Current_Line: Natural := 0;
   begin
      if Reset_Preview then
         Start_Line := 1;
      end if;
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      if Info_Form /= Null_Form then
         Post(Info_Form, False);
         Delete(Info_Form);
      end if;
      Clear_Preview_Window;
      if Is_Directory(To_String(Current_Selected)) then
         if not Is_Read_Accessible_File(To_String(Current_Selected)) then
            Show_Message
              (Mc
                 (Interpreter,
                  "{You don't have permissions to preview this directory.}"));
            return;
         end if;
         Load_Directory(To_String(Current_Selected), True);
         PreviewPad :=
           New_Pad
             (Line_Position(Second_Items_List.Length) + 1, (Columns / 2) - 1);
         Add(PreviewPad, 0, 10, Mc(Interpreter, "Name"));
         Load_Preview_Directory_Loop :
         for Item of Second_Items_List loop
            if not Settings.Show_Hidden and Item.Is_Hidden then
               goto End_Of_Loop;
            end if;
            Current_Line := Current_Line + 1;
            if Current_Line < Start_Line then
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
              Get_Mime_Type(To_String(Current_Selected));
         begin
            if Is_Text(MimeType) then
               PreviewPad := New_Pad(Lines - 2, (Columns / 2) - 1);
               declare
                  ExecutableName: constant String :=
                    Find_Executable("highlight", False);
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
                        Current_Line := Current_Line + 1;
                        if Current_Line < Start_Line then
                           goto End_Of_Load_Text_File_View_Loop;
                        end if;
                        Add
                          (PreviewPad,
                           FileLine(FileLine'First .. Amount) & LF);
                        FileLine := (others => ' ');
                        LinesAmount := LinesAmount + 1;
                        exit Load_Text_File_View_Loop when LinesAmount =
                          Lines - 3;
                        <<End_Of_Load_Text_File_View_Loop>>
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
                  if not Settings.Color_Text or ExecutableName = "" then
                     LoadFile;
                     return;
                  end if;
                  if not Ada.Directories.Exists
                      (Value("HOME") & "/.cache/hunter/highlight.tmp") or
                    Ui_Location /= PREVIEW then
                     Spawn
                       (ExecutableName,
                        Argument_String_To_List
                          ("--out-format=pango --force --quiet --output=" &
                           Value("HOME") &
                           "/.cache/hunter/highlight.tmp --base16 --style=" &
                           To_String(Settings.Color_Theme) & " " &
                           To_String(Current_Selected)).all,
                        Success);
                     if not Success then
                        LoadFile;
                        return;
                     end if;
                  end if;
                  Open
                    (File, In_File,
                     Value("HOME") & "/.cache/hunter/highlight.tmp");
                  FirstLine := True;
                  Read_File_Loop :
                  while not End_Of_File(File) loop
                     FileLine := To_Unbounded_String(Get_Line(File));
                     Current_Line := Current_Line + 1;
                     if Current_Line < Start_Line then
                        goto End_Of_Read_File_Loop;
                     end if;
                     if FirstLine then
                        FileLine :=
                          Unbounded_Slice
                            (FileLine, Index(FileLine, ">") + 1,
                             Length(FileLine));
                        FirstLine := False;
                        StartIndex := Index(FileLine, "<span");
                        EndIndex := Index(FileLine, "</span");
                        if EndIndex > 0
                          and then
                          (StartIndex = 0 or StartIndex > EndIndex) then
                           Delete(FileLine, EndIndex, EndIndex + 6);
                        end if;
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
                     <<End_Of_Read_File_Loop>>
                  end loop Read_File_Loop;
                  Close(File);
                  Refresh
                    (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2),
                     Columns - 3);
                  if Ui_Location /= PREVIEW then
                     Delete_File
                       (Value("HOME") & "/.cache/hunter/highlight.tmp");
                  end if;
               end;
            else
               ShowInfo;
            end if;
         end;
      end if;
   end Show_Preview;

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

   -- ****iv* ShowItemsTUI/ShowItemsTUI.Buttons_Visible
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
      if Ui_Location /= DIRECTORY_VIEW then
         return;
      end if;
      Selected_Items.Clear;
      if Item_Count(Directory_List) > 0 then
         Update_Selected_Items_Loop :
         for I in 1 .. Item_Count(Directory_List) loop
            if Value(Items(Directory_List, I)) or
              Current(Directory_List) = Items(Directory_List, I) then
               Selected_Items.Append
                 (To_Unbounded_String(Description(Items(Directory_List, I))));
            end if;
         end loop Update_Selected_Items_Loop;
      else
         Selected_Items.Append(Common.Current_Directory);
      end if;
      if not Settings.Show_Preview or
        (Selected_Items(1) = Current_Selected and
         Current_Selected /= Common.Current_Directory) then
         return;
      end if;
      Current_Selected :=
        To_Unbounded_String(Description(Current(Directory_List)));
      if New_Action = CREATELINK then
         return;
      end if;
      if Is_Directory(To_String(Current_Selected)) or
        Is_Regular_File(To_String(Current_Selected)) then
         Show_Preview;
      else
         ShowInfo;
      end if;
   end Show_Selected;

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
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      Destination_Directory := To_Unbounded_String(CArgv.Arg(Argv, 1));
      Load_Directory(CArgv.Arg(Argv, 1), True);
      ShowDestination;
      return TCL_OK;
   end GoToDirectory_Command;

   procedure CreateShowItemsUI is
   begin
      PathButtons := Create(1, Columns / 2, 1, 0);
      PreviewWindow := Create(Lines - 2, Columns / 2, 2, Columns / 2);
      Refresh(PreviewWindow);
      Add_Command("GoToDirectory", GoToDirectory_Command'Access);
   end CreateShowItemsUI;

   procedure ShowDestination is
      Index: Positive;
      Path_Items: Item_Array_Access;
      Tokens: Slice_Set;
      Menu_Items: constant Item_Array_Access :=
        new Item_Array
          (Second_Items_List.First_Index .. Second_Items_List.Last_Index + 1);
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PathButtons);
      Clear(PreviewWindow);
      if Ui_Location = DESTINATION_VIEW then
         Box(PreviewWindow, Default_Character, Default_Character);
      end if;
      if not Is_Read_Accessible_File(To_String(Destination_Directory)) then
         Show_Message
           (Mc
              (Interpreter,
               "{You don't have permissions to preview this directory.}"));
         return;
      end if;
      Destination_Directory :=
        To_Unbounded_String
          (Normalize_Pathname(To_String(Destination_Directory)));
      Index := Ada.Strings.Unbounded.Count(Destination_Directory, "/") + 1;
      if Destination_Directory /= To_Unbounded_String("/") then
         Path_Items := new Item_Array(1 .. Index + 1);
         Path_Items.all(1) := New_Item("/");
         Create(Tokens, To_String(Destination_Directory), "/");
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
      Index := Second_Items_List.First_Index;
      Load_Destination_View_Loop :
      for I in
        Second_Items_List.First_Index .. Second_Items_List.Last_Index loop
         if not Settings.Show_Hidden and Second_Items_List(I).Is_Hidden then
            goto End_Of_Loop;
         end if;
         Menu_Items.all(Index) :=
           New_Item(To_String(Second_Items_List(I).Name));
         Index := Index + 1;
         <<End_Of_Loop>>
      end loop Load_Destination_View_Loop;
      if Index > Second_Items_List.First_Index then
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

   procedure Show_Output is
   begin
      if PreviewPad /= Null_Window then
         Delete(PreviewPad);
      end if;
      Clear(PreviewWindow);
      Refresh(PreviewWindow);
      PreviewPad := New_Pad(Lines - 2, (Columns / 2) - 2);
      Refresh
        (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2), Columns - 3);
   end Show_Output;

   procedure Update_Output(Text_To_Append: String) is
   begin
      Add(PreviewPad, Text_To_Append);
      Refresh
        (PreviewPad, 0, 0, 3, (Columns / 2) + 1, (Lines - 2), Columns - 3);
   end Update_Output;

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
         when KEY_LEFT | KEY_RIGHT =>
            Ui_Location := Destination_Path_Keys(Key);
            return;
         when 10 =>
            if Is_Directory
                (To_String(Destination_Directory) & "/" &
                 Name(Current(DestinationList))) then
               Destination_Directory :=
                 Destination_Directory & "/" & Name(Current(DestinationList));
               Load_Directory(To_String(Destination_Directory), True);
               ShowDestination;
            end if;
            return;
         when others =>
            return;
      end case;
      if Result = Menu_Ok then
         Refresh(PreviewWindow);
      end if;
   end Destination_Keys;

   function Destination_Path_Keys(Key: Key_Code) return Ui_Locations is
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
            Destination_Directory := To_Unbounded_String("/");
            Update_Destination_Directory_Loop :
            for I in 2 .. Get_Index(Current(Path)) loop
               Append(Destination_Directory, Name(Items(Path, I)));
               if I < Get_Index(Current(Path)) then
                  Append(Destination_Directory, "/");
               end if;
            end loop Update_Destination_Directory_Loop;
            Load_Directory(To_String(Destination_Directory), True);
            ShowDestination;
            return DESTINATION_VIEW;
         when others =>
            Ui_Location := DESTINATION_VIEW;
            Destination_Keys(Key);
            return Ui_Location;
      end case;
      if Result = Menu_Ok then
         Refresh(PathButtons);
      end if;
      return DESTINATION_PATH;
   end Destination_Path_Keys;

   procedure Preview_Keys(Key: Key_Code) is
      Result: Forms.Driver_Result := Unknown_Request;
      FieldIndex: constant Natural :=
        (if Info_Form /= Null_Form then Get_Index(Current(Info_Form)) else 0);
      SelectedItem: constant String := Full_Name(To_String(Current_Selected));
      procedure Set_Permission(Group, Permission: String) is
         PermissionsString: constant String :=
           To_String(Source => Get_Item_Status(SelectedItem, "-c%A"));
         Sign: Character := '+';
      begin
         if Group = "u"
           and then Index(PermissionsString(1 .. 3), Permission) > 0 then
            Sign := '-';
         elsif Group = "g"
           and then Index(PermissionsString(4 .. 6), Permission) > 0 then
            Sign := '-';
         elsif Group = "o"
           and then Index(PermissionsString(7 .. 9), Permission) > 0 then
            Sign := '-';
         end if;
         Tcl.Ada.Tcl_Eval
           (Interpreter,
            "file attributes {" & SelectedItem & "} -permissions " & Group &
            Sign & Permission);
      end Set_Permission;
   begin
      if FieldIndex = 0 then
         case Key is
            when KEY_UP =>
               if Start_Line > 1 then
                  Start_Line := Start_Line - 1;
               end if;
               Show_Preview(False);
            when KEY_DOWN =>
               Start_Line := Start_Line + 1;
               Show_Preview(False);
            when Key_Home =>
               Start_Line := 1;
               Show_Preview(False);
            when KEY_NPAGE =>
               if Start_Line > Positive(Lines - 6) then
                  Start_Line := Start_Line - Positive(Lines - 6);
               else
                  Start_Line := 1;
               end if;
               Show_Preview(False);
            when KEY_PPAGE =>
               Start_Line := Start_Line + Positive(Lines - 6);
               Show_Preview(False);
            when others =>
               null;
         end case;
         return;
      end if;
      case Key is
         when KEY_UP =>
            Result := Driver(Info_Form, F_Previous_Field);
            Result := Driver(Info_Form, F_End_Line);
         when KEY_DOWN =>
            Result := Driver(Info_Form, F_Next_Field);
            Result := Driver(Info_Form, F_End_Line);
         when 10 =>
            if FieldIndex = 2 then
               ShowProgramsMenu;
               Ui_Location := PROGRAMS_MENU;
               return;
            end if;
            if Is_Directory(SelectedItem) then
               case FieldIndex is
                  when 4 =>
                     Set_Permission("u", "w");
                  when 5 =>
                     Set_Permission("u", "r");
                  when 7 =>
                     Set_Permission("g", "w");
                  when 8 =>
                     Set_Permission("g", "r");
                  when 10 =>
                     Set_Permission("o", "w");
                  when 11 =>
                     Set_Permission("o", "r");
                  when others =>
                     null;
               end case;
            else
               case FieldIndex is
                  when 4 =>
                     Set_Permission("u", "x");
                  when 5 =>
                     Set_Permission("u", "w");
                  when 6 =>
                     Set_Permission("u", "r");
                  when 8 =>
                     Set_Permission("g", "x");
                  when 9 =>
                     Set_Permission("g", "w");
                  when 10 =>
                     Set_Permission("g", "r");
                  when 12 =>
                     Set_Permission("o", "x");
                  when 13 =>
                     Set_Permission("o", "w");
                  when 14 =>
                     Set_Permission("o", "r");
                  when others =>
                     null;
               end case;
            end if;
            ShowInfo;
            return;
         when others =>
            return;
      end case;
      if Result = Form_Ok then
         Refresh(FormWindow);
      end if;
   end Preview_Keys;

   procedure Clear_Preview_Window is
   begin
      Clear(PreviewWindow);
      if Ui_Location in PREVIEW | DESTINATION_VIEW then
         Box(PreviewWindow, Default_Character, Default_Character);
      end if;
      Refresh(PreviewWindow);
   end Clear_Preview_Window;

end ShowItems;
