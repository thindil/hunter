-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Tcl.Ada; use Tcl.Ada;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with CopyItems; use CopyItems;
with Inotify; use Inotify;
with LoadData.UI; use LoadData.UI;
with Modules; use Modules;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with UserCommands; use UserCommands;

package body Preferences.UI is

   OptionsMenu, SubMenu: Menu;
   MenuWindow, MenuWindow2: Window;
   OptionsWindow: Window;
   DialogForm, CommandForm: Forms.Form;
   Option_Selected: Boolean := True;
   Modules_List: UnboundedString_Container.Vector;

   procedure Show_Options_Tab(Tab: Positive) is
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      FieldOptions: Field_Option_Set;
   begin
      Clear(OptionsWindow);
      Set_Cursor_Visibility(Visibility);
      case Tab is
         when 1 =>
            declare
               Options_Fields: constant Field_Array_Access :=
                 new Field_Array(1 .. 17);
            begin
               Options_Fields.all(1) := New_Field(1, 50, 0, 0, 0, 0);
               Set_Buffer
                 (Options_Fields.all(1), 0,
                  Mc(Interpreter, "{Directory Listing}"));
               FieldOptions := Get_Options(Options_Fields.all(1));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(1), FieldOptions);
               Options_Fields.all(2) := New_Field(1, 50, 1, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(2), 0,
                  (if Settings.Show_Hidden then Mc(Interpreter, "Show")
                   else Mc(Interpreter, "{Don't show}")) &
                  " " & Mc(Interpreter, "{hidden files}"));
               FieldOptions := Get_Options(Options_Fields.all(2));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(2), FieldOptions);
               Options_Fields.all(3) := New_Field(1, 50, 2, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(3), 0,
                  (if Settings.Show_Last_Modified then Mc(Interpreter, "Show")
                   else Mc(Interpreter, "{Don't show}")) &
                  " " & Mc(Interpreter, "{modification time}"));
               FieldOptions := Get_Options(Options_Fields.all(3));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(3), FieldOptions);
               Options_Fields.all(4) := New_Field(1, 50, 3, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(4), 0,
                  Mc(Interpreter, "{Auto refresh every}") &
                  Natural'Image(Settings.Auto_Refresh_Interval) & " " &
                  Mc(Interpreter, "{seconds}"));
               FieldOptions := Get_Options(Options_Fields.all(4));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(4), FieldOptions);
               Options_Fields.all(5) := New_Field(1, 50, 4, 0, 0, 0);
               Set_Buffer
                 (Options_Fields.all(5), 0, Mc(Interpreter, "Preview"));
               FieldOptions := Get_Options(Options_Fields.all(5));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(5), FieldOptions);
               Options_Fields.all(6) := New_Field(1, 50, 5, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(6), 0,
                  (if Settings.Show_Preview then Mc(Interpreter, "Show")
                   else Mc(Interpreter, "{Don't show}")) &
                  " " & Mc(Interpreter, "preview"));
               FieldOptions := Get_Options(Options_Fields.all(6));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(6), FieldOptions);
               Options_Fields.all(7) := New_Field(1, 50, 6, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(7), 0,
                  (if Settings.Color_Text then Mc(Interpreter, "Enable")
                   else Mc(Interpreter, "Disable")) &
                  " " & Mc(Interpreter, "{syntax highlightning}"));
               FieldOptions := Get_Options(Options_Fields.all(7));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(7), FieldOptions);
               Options_Fields.all(8) := New_Field(1, 50, 7, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(8), 0,
                  Mc(Interpreter, "{Current theme}") & ": " &
                  To_String(Settings.Color_Theme));
               FieldOptions := Get_Options(Options_Fields.all(8));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(8), FieldOptions);
               Options_Fields.all(9) := New_Field(1, 50, 8, 0, 0, 0);
               Set_Buffer
                 (Options_Fields.all(9), 0, Mc(Interpreter, "Interface"));
               FieldOptions := Get_Options(Options_Fields.all(9));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(9), FieldOptions);
               Options_Fields.all(10) := New_Field(1, 50, 9, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(10), 0,
                  (if Settings.Stay_In_Old then
                     Mc(Interpreter, "{Stay in source directory}")
                   else Mc(Interpreter, "{Go to destination}")));
               FieldOptions := Get_Options(Options_Fields.all(10));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(10), FieldOptions);
               Options_Fields.all(11) := New_Field(1, 50, 10, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(11), 0,
                  (if Settings.Show_Finished_Info then Mc(Interpreter, "Show")
                   else Mc(Interpreter, "{Don't show}")) &
                  " " &
                  Mc(Interpreter, "{information about finished action}"));
               FieldOptions := Get_Options(Options_Fields.all(11));
               Set_Options(Options_Fields.all(11), FieldOptions);
               Options_Fields.all(12) := New_Field(1, 50, 11, 0, 0, 0);
               Set_Buffer
                 (Options_Fields.all(12), 0, Mc(Interpreter, "Deleting"));
               FieldOptions := Get_Options(Options_Fields.all(12));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(12), FieldOptions);
               Options_Fields.all(13) := New_Field(1, 50, 12, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(13), 0,
                  (if Settings.Delete_Files then
                     Mc(Interpreter, "{Delete files}")
                   else Mc(Interpreter, "{Move files to Trash}")));
               FieldOptions := Get_Options(Options_Fields.all(13));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(13), FieldOptions);
               Options_Fields.all(14) := New_Field(1, 50, 13, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(14), 0,
                  (if Settings.Clear_Trash_On_Exit then
                     Mc(Interpreter, "Clear")
                   else Mc(Interpreter, "{Don't clear}")) &
                  " " & Mc(Interpreter, "{Trash on exit}"));
               FieldOptions := Get_Options(Options_Fields.all(14));
               Set_Options(Options_Fields.all(14), FieldOptions);
               Options_Fields.all(15) := New_Field(1, 50, 14, 0, 0, 0);
               Set_Buffer
                 (Options_Fields.all(15), 0,
                  Mc(Interpreter, "{Copying or moving}"));
               FieldOptions := Get_Options(Options_Fields.all(15));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(15), FieldOptions);
               Options_Fields.all(16) := New_Field(1, 50, 15, 2, 0, 0);
               Set_Buffer
                 (Options_Fields.all(16), 0,
                  (if Settings.Overwrite_On_Exist then
                     Mc(Interpreter, "{Overwrite}")
                   else Mc(Interpreter, "{Don't overwrite}")) &
                  " " & Mc(Interpreter, "existing"));
               FieldOptions := Get_Options(Options_Fields.all(16));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(16), FieldOptions);
               Options_Fields.all(17) := Null_Field;
               DialogForm := New_Form(Options_Fields);
               Set_Current(DialogForm, Options_Fields(2));
            end;
         when 2 =>
            declare
               Options_Fields: constant Field_Array_Access :=
                 new Field_Array(1 .. 39);
               KeysLabels: constant array(1 .. 19) of Unbounded_String :=
                 (To_Unbounded_String
                    (Mc(Interpreter, "{Show bookmarks menu}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Search for the file or directory}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Show add new item menu}")),
                  To_Unbounded_String(Mc(Interpreter, "{Show delete menu}")),
                  To_Unbounded_String
                    (Mc
                       (Interpreter,
                        "{Show menu with information about the program}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Open selected file or directory}")),
                  To_Unbounded_String
                    (Mc
                       (Interpreter,
                        "{Select or unselect all files and directories}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Rename selected file or directory}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Copy selected files}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Move selected files}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Show the program preferences}")),
                  To_Unbounded_String
                    (Mc
                       (Interpreter,
                        "{Open selected file or directory with command}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{File or directory information}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Preview file or directory}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Add bookmark to this directory}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Remove bookmark from this directory}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Execute selected program}")),
                  To_Unbounded_String
                    (Mc
                       (Interpreter,
                        "{Restore deleted file or directory from Trash}")),
                  To_Unbounded_String
                    (Mc(Interpreter, "{Show the user defined actions}")));
               FieldLength: Column_Position := 1;
            begin
               for Label of KeysLabels loop
                  if Length(Label) > Positive(FieldLength) then
                     FieldLength := Column_Position(Length(Label));
                  end if;
               end loop;
               for I in Options_Fields'First .. Options_Fields'Last - 1 loop
                  if I mod 2 /= 0 then
                     Options_Fields.all(I) :=
                       New_Field
                         (1, FieldLength, Line_Position(I / 2), 0, 0, 0);
                     Set_Buffer
                       (Options_Fields.all(I), 0,
                        To_String(KeysLabels((I / 2) + 1)));
                     FieldOptions := Get_Options(Options_Fields.all(I));
                     FieldOptions.Edit := False;
                     FieldOptions.Active := False;
                     Set_Options(Options_Fields.all(I), FieldOptions);
                  else
                     Options_Fields.all(I) :=
                       New_Field
                         (1, 18, Line_Position((I / 2) - 1), FieldLength + 2,
                          0, 0);
                     Set_Buffer
                       (Options_Fields.all(I), 0,
                        To_String(Accelerators((I / 2) + 1)));
                     FieldOptions := Get_Options(Options_Fields.all(I));
                     FieldOptions.Edit := False;
                     Set_Options(Options_Fields.all(I), FieldOptions);
                  end if;
               end loop;
               Options_Fields.all(39) := Null_Field;
               DialogForm := New_Form(Options_Fields);
               Set_Current(DialogForm, Options_Fields(2));
            end;
         when 3 =>
            declare
               Options_Fields: constant Field_Array_Access :=
                 new Field_Array
                   (1 .. Positive((UserCommandsList.Length * 5) + 6));
               Index: Positive := 6;
               Line: Line_Position := 3;
            begin
               Options_Fields.all(1) := New_Field(1, 18, 0, 0, 0, 0);
               Set_Buffer(Options_Fields.all(1), 0, "Add new command");
               FieldOptions := Get_Options(Options_Fields.all(1));
               FieldOptions.Edit := False;
               Set_Options(Options_Fields.all(1), FieldOptions);
               Options_Fields.all(2) := New_Field(1, 16, 1, 20, 0, 0);
               Set_Buffer(Options_Fields.all(2), 0, "Defined commands");
               FieldOptions := Get_Options(Options_Fields.all(2));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(2), FieldOptions);
               Options_Fields.all(3) := New_Field(1, 10, 2, 0, 0, 0);
               Set_Buffer(Options_Fields.all(3), 0, "Menu label");
               FieldOptions := Get_Options(Options_Fields.all(3));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(3), FieldOptions);
               Options_Fields.all(4) := New_Field(1, 7, 2, 20, 0, 0);
               Set_Buffer(Options_Fields.all(4), 0, "Command");
               FieldOptions := Get_Options(Options_Fields.all(4));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(4), FieldOptions);
               Options_Fields.all(5) := New_Field(1, 6, 2, 40, 0, 0);
               Set_Buffer(Options_Fields.all(5), 0, "Output");
               FieldOptions := Get_Options(Options_Fields.all(5));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(5), FieldOptions);
               for I in UserCommandsList.Iterate loop
                  Options_Fields.all(Index) := New_Field(1, 20, Line, 0, 0, 0);
                  Set_Buffer
                    (Options_Fields.all(Index), 0, Commands_Container.Key(I));
                  FieldOptions := Get_Options(Options_Fields.all(Index));
                  FieldOptions.Edit := False;
                  FieldOptions.Active := False;
                  Set_Options(Options_Fields.all(Index), FieldOptions);
                  Options_Fields.all(Index + 1) :=
                    New_Field(1, 20, Line, 20, 0, 0);
                  Set_Buffer
                    (Options_Fields.all(Index + 1), 0,
                     To_String(UserCommandsList(I).Command));
                  FieldOptions := Get_Options(Options_Fields.all(Index + 1));
                  FieldOptions.Edit := False;
                  FieldOptions.Active := False;
                  Set_Options(Options_Fields.all(Index + 1), FieldOptions);
                  Options_Fields.all(Index + 2) :=
                    New_Field(1, 20, Line, 40, 0, 0);
                  Set_Buffer
                    (Options_Fields.all(Index + 2), 0,
                     (if UserCommandsList(I).NeedOutput then "Yes" else "No"));
                  FieldOptions := Get_Options(Options_Fields.all(Index + 2));
                  FieldOptions.Edit := False;
                  FieldOptions.Active := False;
                  Set_Options(Options_Fields.all(Index + 2), FieldOptions);
                  Options_Fields.all(Index + 3) :=
                    New_Field(1, 5, Line, 50, 0, 0);
                  Set_Buffer(Options_Fields.all(Index + 3), 0, "Edit");
                  FieldOptions := Get_Options(Options_Fields.all(Index + 3));
                  FieldOptions.Edit := False;
                  Set_Options(Options_Fields.all(Index + 3), FieldOptions);
                  Options_Fields.all(Index + 4) :=
                    New_Field(1, 6, Line, 57, 0, 0);
                  Set_Buffer(Options_Fields.all(Index + 4), 0, "Delete");
                  FieldOptions := Get_Options(Options_Fields.all(Index + 4));
                  FieldOptions.Edit := False;
                  Set_Options(Options_Fields.all(Index + 4), FieldOptions);
                  Index := Index + 5;
                  Line := Line + 1;
               end loop;
               Options_Fields.all(Options_Fields'Last) := Null_Field;
               DialogForm := New_Form(Options_Fields);
               Set_Current(DialogForm, Options_Fields(1));
            end;
         when 4 =>
            declare
               Options_Fields: Field_Array_Access := new Field_Array(1 .. 6);
               Line: Line_Position := 1;
               Amount: Natural := 0;
               FormIndex: Positive := 6;
               CurrentDir: constant String :=
                 Ada.Directories.Current_Directory;
               procedure LoadModulesInfo(Path: String) is
                  Directory: Dir_Type;
                  FileName: String(1 .. 1_024);
                  Last: Natural range 0 .. FileName'Last;
                  ConfigName: GNAT.OS_Lib.String_Access;
                  ConfigFile: File_Type;
                  FileLine: Unbounded_String;
               begin
                  Open(Directory, Path);
                  Read_Modules_Directory_Loop :
                  loop
                     Read(Directory, FileName, Last);
                     exit Read_Modules_Directory_Loop when Last = 0;
                     if FileName(1 .. Last) in "." | ".." then
                        goto End_Of_Read_Loop;
                     end if;
                     if not Is_Directory(Path & "/" & FileName(1 .. Last)) then
                        goto End_Of_Read_Loop;
                     end if;
                     ConfigName :=
                       Locate_Regular_File
                         ("module.cfg", Path & "/" & FileName(1 .. Last));
                     if ConfigName = null or
                       Locate_Regular_File
                           ("module.tcl", Path & "/" & FileName(1 .. Last)) =
                         null then
                        goto End_Of_Read_Loop;
                     end if;
                     Options_Fields.all(FormIndex) :=
                       New_Field(1, 7, Line, 0, 0, 0);
                     Set_Buffer
                       (Options_Fields.all(FormIndex), 0,
                        (if
                           Enabled_Modules.Contains
                             (To_Unbounded_String
                                (Path & "/" & FileName(1 .. Last)))
                         then "Yes"
                         else "No"));
                     FieldOptions :=
                       Get_Options(Options_Fields.all(FormIndex));
                     FieldOptions.Edit := False;
                     Set_Options(Options_Fields.all(FormIndex), FieldOptions);
                     Open(ConfigFile, In_File, ConfigName.all);
                     Read_Config_File_Loop :
                     while not End_Of_File(ConfigFile) loop
                        FileLine := Get_Line(ConfigFile);
                        if Length(FileLine) > 5
                          and then Index(FileLine, "Name=") = 1 then
                           Options_Fields.all(FormIndex + 1) :=
                             New_Field(1, 8, Line, 10, 0, 0);
                           Set_Buffer
                             (Options_Fields.all(FormIndex + 1), 0,
                              Slice(FileLine, 6, Length(FileLine)));
                           FieldOptions :=
                             Get_Options(Options_Fields.all(FormIndex + 1));
                           FieldOptions.Edit := False;
                           FieldOptions.Active := False;
                           Set_Options
                             (Options_Fields.all(FormIndex + 1), FieldOptions);
                        elsif Length(FileLine) > 8
                          and then Index(FileLine, "Version=") = 1 then
                           Options_Fields.all(FormIndex + 2) :=
                             New_Field(1, 5, Line, 20, 0, 0);
                           Set_Buffer
                             (Options_Fields.all(FormIndex + 2), 0,
                              Slice(FileLine, 9, Length(FileLine)));
                           FieldOptions :=
                             Get_Options(Options_Fields.all(FormIndex + 2));
                           FieldOptions.Edit := False;
                           FieldOptions.Active := False;
                           Set_Options
                             (Options_Fields.all(FormIndex + 2), FieldOptions);
                        elsif Length(FileLine) > 12
                          and then Index(FileLine, "Description=") = 1 then
                           Options_Fields.all(FormIndex + 3) :=
                             New_Field(2, 22, Line, 27, 0, 0);
                           Set_Buffer
                             (Options_Fields.all(FormIndex + 3), 0,
                              Slice(FileLine, 13, Length(FileLine)));
                           FieldOptions :=
                             Get_Options(Options_Fields.all(FormIndex + 3));
                           FieldOptions.Edit := False;
                           FieldOptions.Active := False;
                           Set_Options
                             (Options_Fields.all(FormIndex + 3), FieldOptions);
                        end if;
                     end loop Read_Config_File_Loop;
                     Close(ConfigFile);
                     Options_Fields.all(FormIndex + 4) :=
                       New_Field(1, 7, Line, 50, 0, 0);
                     Set_Buffer(Options_Fields.all(FormIndex + 4), 0, "Show");
                     FieldOptions :=
                       Get_Options(Options_Fields.all(FormIndex + 4));
                     FieldOptions.Edit := False;
                     Set_Options
                       (Options_Fields.all(FormIndex + 4), FieldOptions);
                     Line := Line + 2;
                     FormIndex := FormIndex + 5;
                     <<End_Of_Read_Loop>>
                  end loop Read_Modules_Directory_Loop;
                  Close(Directory);
               exception
                  when Directory_Error =>
                     null;
               end LoadModulesInfo;
               function CountModules(Path: String) return Natural is
                  ModulesAmount: Natural := 0;
                  Directory: Dir_Type;
                  FileName: String(1 .. 1_024);
                  Last: Natural range 0 .. FileName'Last;
               begin
                  Open(Directory, Path);
                  Count_Modules_Loop :
                  loop
                     Read(Directory, FileName, Last);
                     exit Count_Modules_Loop when Last = 0;
                     if FileName(1 .. Last) in "." | ".." then
                        goto End_Of_Count_Loop;
                     end if;
                     if not Is_Directory(Path & "/" & FileName(1 .. Last)) then
                        goto End_Of_Count_Loop;
                     end if;
                     ModulesAmount := ModulesAmount + 1;
                     Modules_List.Append
                       (To_Unbounded_String(Path & "/" & FileName(1 .. Last)));
                     <<End_Of_Count_Loop>>
                  end loop Count_Modules_Loop;
                  return ModulesAmount;
               end CountModules;
            begin
               Modules_List.Clear;
               Amount := (CountModules("../share/hunter/modules") * 5);
               Options_Fields := new Field_Array(1 .. 6 + Amount);
               Options_Fields.all(1) := New_Field(1, 7, 0, 0, 0, 0);
               Set_Buffer(Options_Fields.all(1), 0, "Enabled");
               FieldOptions := Get_Options(Options_Fields.all(1));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(1), FieldOptions);
               Options_Fields.all(2) := New_Field(1, 4, 0, 10, 0, 0);
               Set_Buffer(Options_Fields.all(2), 0, "Name");
               FieldOptions := Get_Options(Options_Fields.all(2));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(2), FieldOptions);
               Options_Fields.all(3) := New_Field(1, 7, 0, 17, 0, 0);
               Set_Buffer(Options_Fields.all(3), 0, "Version");
               FieldOptions := Get_Options(Options_Fields.all(3));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(3), FieldOptions);
               Options_Fields.all(4) := New_Field(1, 11, 0, 27, 0, 0);
               Set_Buffer(Options_Fields.all(4), 0, "Description");
               FieldOptions := Get_Options(Options_Fields.all(4));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(4), FieldOptions);
               Options_Fields.all(5) := New_Field(1, 4, 0, 50, 0, 0);
               Set_Buffer(Options_Fields.all(5), 0, "Show");
               FieldOptions := Get_Options(Options_Fields.all(5));
               FieldOptions.Edit := False;
               FieldOptions.Active := False;
               Set_Options(Options_Fields.all(5), FieldOptions);
               Options_Fields.all(Options_Fields'Last) := Null_Field;
               -- Load the list of the program modules
               Set_Directory(Containing_Directory(Command_Name));
               LoadModulesInfo("../share/hunter/modules");
               LoadModulesInfo
                 (Ada.Environment_Variables.Value("HOME") &
                  "/.local/share/hunter/modules");
               Set_Directory(CurrentDir);
               DialogForm := New_Form(Options_Fields);
               if Amount > 0 then
                  Set_Current(DialogForm, Options_Fields(6));
               end if;
            end;
         when others =>
            null;
      end case;
      Set_Options(DialogForm, (others => False));
      Scale(DialogForm, FormHeight, FormLength);
      Set_Window(DialogForm, OptionsWindow);
      Set_Sub_Window
        (DialogForm,
         Derived_Window(OptionsWindow, FormHeight, FormLength, 1, 1));
      Post(DialogForm);
      Refresh(OptionsWindow);
      Option_Selected := True;
   end Show_Options_Tab;

   procedure Show_Options is
      Main_Menu_Array: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String(Mc(Interpreter, "Preferences")),
         To_Unbounded_String(Mc(Interpreter, "Shortcuts")),
         To_Unbounded_String(Mc(Interpreter, "Commands")),
         To_Unbounded_String(Mc(Interpreter, "Modules")),
         To_Unbounded_String(Mc(Interpreter, "Close")));
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 6);
   begin
      Temporary_Stop := True;
      Clear;
      Create_Program_Menu_Loop :
      for I in Main_Menu_Array'Range loop
         Menu_Items.all(I) := New_Item(To_String(Main_Menu_Array(I)));
      end loop Create_Program_Menu_Loop;
      Menu_Items.all(6) := Null_Item;
      OptionsMenu := New_Menu(Menu_Items);
      Set_Format(OptionsMenu, 1, 5);
      Set_Mark(OptionsMenu, "");
      MenuWindow := Create(1, Columns, 0, 0);
      Set_Window(OptionsMenu, MenuWindow);
      Set_Sub_Window
        (OptionsMenu, Derived_Window(MenuWindow, 1, Columns, 0, 0));
      Post(OptionsMenu);
      Refresh;
      Refresh(MenuWindow);
      OptionsWindow := Create(Lines - 1, Columns, 1, 0);
      Show_Options_Tab(1);
   end Show_Options;

   procedure Show_Seconds_Menu(Max: Positive := 30) is
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. Max + 3);
      Visibility: Cursor_Visibility := Invisible;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      Set_Cursor_Visibility(Visibility);
      Menu_Items.all(1) := New_Item("Disable");
      Create_Time_Menu_Loop :
      for I in 2 .. Max + 1 loop
         Menu_Items.all(I) :=
           New_Item("every" & Natural'Image(I - 1) & " second(s)");
      end loop Create_Time_Menu_Loop;
      Menu_Items.all(Max + 2) := New_Item("Close");
      Menu_Items.all(Max + 3) := Null_Item;
      SubMenu := New_Menu(Menu_Items);
      Set_Format(SubMenu, 10, 1);
      Set_Mark(SubMenu, "");
      Scale(SubMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create(MenuHeight + 2, MenuLength + 2, Lines / 3, Columns / 3);
      Set_Window(SubMenu, MenuWindow2);
      Set_Sub_Window
        (SubMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Box(MenuWindow2, Default_Character, Default_Character);
      Post(SubMenu);
      Refresh;
      Refresh(MenuWindow2);
   end Show_Seconds_Menu;

   procedure Show_Colors_Menu is
      Menu_Items: Item_Array_Access;
      Visibility: Cursor_Visibility := Invisible;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      ThemesName: Unbounded_String;
      Tokens: Slice_Set;
      Search: Search_Type;
      File: Directory_Entry_Type;
   begin
      Set_Cursor_Visibility(Visibility);
      if not Ada.Environment_Variables.Exists("HIGHLIGHT_DATADIR") then
         Ada.Environment_Variables.Set
           ("HIGHLIGHT_DATADIR",
            Ada.Environment_Variables.Value("APPDIR", "") &
            "/usr/share/highlight");
      end if;
      if Exists
          (Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
           "/themes/base16") then
         Start_Search
           (Search,
            Ada.Environment_Variables.Value("HIGHLIGHT_DATADIR") &
            "/themes/base16",
            "*.theme");
         Create_Themes_List_Loop :
         while More_Entries(Search) loop
            Get_Next_Entry(Search, File);
            Append
              (ThemesName, " " & Ada.Directories.Base_Name(Simple_Name(File)));
         end loop Create_Themes_List_Loop;
         End_Search(Search);
      end if;
      Create(Tokens, To_String(ThemesName), " ");
      Menu_Items := new Item_Array(1 .. Integer(Slice_Count(Tokens)) + 1);
      Set_Menu_Loop :
      for I in 1 .. Integer(Slice_Count(Tokens)) - 1 loop
         Menu_Items.all(I) := New_Item(Slice(Tokens, Slice_Number(I + 1)));
      end loop Set_Menu_Loop;
      Menu_Items.all(Menu_Items'Last - 1) := New_Item("Close");
      Menu_Items.all(Menu_Items'Last) := Null_Item;
      SubMenu := New_Menu(Menu_Items);
      Set_Format(SubMenu, 10, 1);
      Set_Mark(SubMenu, "");
      Scale(SubMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create(MenuHeight + 2, MenuLength + 2, Lines / 3, Columns / 3);
      Set_Window(SubMenu, MenuWindow2);
      Set_Sub_Window
        (SubMenu, Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Box(MenuWindow2, Default_Character, Default_Character);
      Post(SubMenu);
      Refresh;
      Refresh(MenuWindow2);
   end Show_Colors_Menu;

   function Set_Option(TabIndex, OptionIndex: Positive) return UI_Locations is
      Visibility: Cursor_Visibility := Invisible;
      procedure Show_Command_Form(Index: Natural := 0) is
         Command_Fields: constant Field_Array_Access :=
           new Field_Array(1 .. 8);
         FormHeight: Line_Position;
         FormLength: Column_Position;
         FieldOptions: Field_Option_Set;
      begin
         Command_Fields.all(1) := New_Field(1, 12, 0, 0, 0, 0);
         Set_Buffer(Command_Fields.all(1), 0, "Menu label:");
         FieldOptions := Get_Options(Command_Fields.all(1));
         FieldOptions.Edit := False;
         FieldOptions.Active := False;
         Set_Options(Command_Fields.all(1), FieldOptions);
         Command_Fields.all(2) := New_Field(1, 20, 1, 0, 0, 0);
         if Index > 0 then
            Set_Buffer
              (Command_Fields.all(2), 0,
               Trim(Get_Buffer(Fields(DialogForm, Index - 3)), Both));
         end if;
         Command_Fields.all(3) := New_Field(1, 19, 2, 0, 0, 0);
         Set_Buffer(Command_Fields.all(3), 0, "Command to execute:");
         FieldOptions := Get_Options(Command_Fields.all(3));
         FieldOptions.Edit := False;
         FieldOptions.Active := False;
         Set_Options(Command_Fields.all(3), FieldOptions);
         Command_Fields.all(4) := New_Field(1, 30, 3, 0, 0, 0);
         if Index > 0 then
            Set_Buffer
              (Command_Fields.all(4), 0,
               Trim(Get_Buffer(Fields(DialogForm, Index - 2)), Both));
         end if;
         Command_Fields.all(5) := New_Field(1, 20, 4, 0, 0, 0);
         if Index = 0 then
            Set_Buffer(Command_Fields.all(5), 0, "Don't use output");
         else
            Set_Buffer
              (Command_Fields.all(5), 0,
               (if Trim(Get_Buffer(Fields(DialogForm, Index - 1)), Both) = "No"
                then "Don't u"
                else "U") &
               "se output");
         end if;
         FieldOptions := Get_Options(Command_Fields.all(5));
         FieldOptions.Edit := False;
         Set_Options(Command_Fields.all(5), FieldOptions);
         Command_Fields.all(6) := New_Field(1, 8, 5, 2, 0, 0);
         Set_Buffer(Command_Fields.all(6), 0, "[Cancel]");
         FieldOptions := Get_Options(Command_Fields.all(6));
         FieldOptions.Edit := False;
         Set_Options(Command_Fields.all(6), FieldOptions);
         Command_Fields.all(7) := Null_Field;
         Command_Fields.all(7) := New_Field(1, 6, 5, 15, 0, 0);
         Set_Buffer
           (Command_Fields.all(7), 0,
            (if Index = 0 then "[Add]" else "[Edit]"));
         FieldOptions := Get_Options(Command_Fields.all(7));
         FieldOptions.Edit := False;
         Set_Options(Command_Fields.all(7), FieldOptions);
         Command_Fields.all(8) := Null_Field;
         CommandForm := New_Form(Command_Fields);
         Set_Options(CommandForm, (others => False));
         Scale(CommandForm, FormHeight, FormLength);
         MenuWindow2 :=
           Create(FormHeight + 2, FormLength + 2, Lines / 3, Columns / 3);
         Set_Window(CommandForm, MenuWindow2);
         Set_Sub_Window
           (CommandForm,
            Derived_Window(MenuWindow2, FormHeight, FormLength, 1, 1));
         Box(MenuWindow2, Default_Character, Default_Character);
         Post(CommandForm);
         Refresh(MenuWindow2);
      end Show_Command_Form;
   begin
      case TabIndex is
         -- The general preferences of the program
         when 1 =>
            case OptionIndex is
               when 2 =>
                  Settings.Show_Hidden := not Settings.Show_Hidden;
               when 3 =>
                  Settings.Show_Last_Modified :=
                    not Settings.Show_Last_Modified;
               when 4 =>
                  Show_Seconds_Menu;
                  return SECONDS_MENU;
               when 6 =>
                  Settings.Show_Preview := not Settings.Show_Preview;
               when 7 =>
                  Settings.Color_Text := not Settings.Color_Text;
               when 8 =>
                  Show_Colors_Menu;
                  return COLORS_MENU;
               when 10 =>
                  Settings.Stay_In_Old := not Settings.Stay_In_Old;
               when 11 =>
                  Settings.Show_Finished_Info :=
                    not Settings.Show_Finished_Info;
               when 13 =>
                  Settings.Delete_Files := not Settings.Delete_Files;
               when 14 =>
                  Settings.Clear_Trash_On_Exit :=
                    not Settings.Clear_Trash_On_Exit;
               when 16 =>
                  Settings.Overwrite_On_Exist :=
                    not Settings.Overwrite_On_Exist;
               when others =>
                  null;
            end case;
         -- Keyboard shortcuts for the program
         when 2 =>
            Set_Cursor_Visibility(Visibility);
            MenuWindow2 := Create(5, 44, Lines / 3, Columns / 3);
            Move_Cursor(MenuWindow2, 1, 1);
            Add(MenuWindow2, "Press a key which will be set as shortcut.");
            Move_Cursor(MenuWindow2, 2, 1);
            Add(MenuWindow2, "Press Escape twice for cancel.");
            Box(MenuWindow2, Default_Character, Default_Character);
            Refresh;
            Refresh(MenuWindow2);
            return SHORTCUT_FORM;
         -- The user defined commands
         when 3 =>
            case OptionIndex is
               when 1 =>
                  Show_Command_Form;
                  return COMMAND_FORM;
               when others =>
                  declare
                     CurrentOption: constant String :=
                       Trim(Get_Buffer(Fields(DialogForm, OptionIndex)), Both);
                  begin
                     if CurrentOption = "Edit" then
                        Show_Command_Form(OptionIndex);
                        return COMMAND_FORM;
                     elsif CurrentOption = "Delete" then
                        UserCommandsList.Delete
                          (Trim
                             (Get_Buffer(Fields(DialogForm, OptionIndex - 4)),
                              Both));
                     end if;
                  end;
            end case;
         -- The program's modules
         when 4 =>
            case OptionIndex mod 5 is
               when 0 =>
                  Common.Current_Directory :=
                    Modules_List((OptionIndex / 5) - 1);
                  Load_Directory(To_String(Common.Current_Directory));
                  UILocation := DIRECTORY_VIEW;
                  Update_Directory_List(True);
                  Update_Watch(To_String(Common.Current_Directory));
                  Execute_Modules
                    (Interpreter, ON_ENTER,
                     "{" & To_String(Common.Current_Directory) & "}");
                  return DIRECTORY_VIEW;
               when 1 =>
                  Tcl_Eval
                    (Interpreter,
                     "ToggleModule {" &
                     To_String(Modules_List(OptionIndex / 5)) & "}");
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      Show_Options_Tab(TabIndex);
      return OPTIONS_VIEW;
   end Set_Option;

   function Select_Preferences_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      Result2: Forms.Driver_Result := Unknown_Request;
      CurrentIndex: constant Positive := Get_Index(Current(OptionsMenu));
      CurrentOption: constant Positive := Get_Index(Current(DialogForm));
      Visibility: Cursor_Visibility := Invisible;
   begin
      case Key is
         when KEY_LEFT =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_Previous_Item);
            Option_Selected := False;
         when KEY_RIGHT =>
            Set_Cursor_Visibility(Visibility);
            Result := Driver(OptionsMenu, M_Next_Item);
            Option_Selected := False;
         when Key_Home =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_First_Field);
            Option_Selected := True;
         when Key_End =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_Last_Field);
            Option_Selected := True;
         when KEY_UP =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_Previous_Field);
            Option_Selected := True;
         when KEY_DOWN =>
            Visibility := Normal;
            Set_Cursor_Visibility(Visibility);
            Result2 := Driver(DialogForm, F_Next_Field);
            Option_Selected := True;
         when 10 =>
            if not Option_Selected then
               if CurrentIndex = 5 then
                  Set_Cursor_Visibility(Visibility);
                  Temporary_Stop := False;
                  Clear;
                  UILocation := DIRECTORY_VIEW;
                  Show_Main_Window;
                  Execute_Modules
                    (Interpreter, ON_ENTER,
                     "{" & To_String(Common.Current_Directory) & "}");
                  Show_Preview;
                  return DIRECTORY_VIEW;
               else
                  Show_Options_Tab(CurrentIndex);
               end if;
            else
               return Set_Option(CurrentIndex, CurrentOption);
            end if;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow);
      end if;
      if Result2 = Form_Ok then
         Refresh(OptionsWindow);
      end if;
      return OPTIONS_VIEW;
   end Select_Preferences_Keys;

   function Select_Seconds_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
   begin
      case Key is
         when Key_Home =>
            Result := Driver(SubMenu, M_First_Item);
         when Key_End =>
            Result := Driver(SubMenu, M_Last_Item);
         when KEY_UP =>
            Result := Driver(SubMenu, M_Previous_Item);
         when KEY_DOWN =>
            Result := Driver(SubMenu, M_Next_Item);
         when KEY_NPAGE =>
            Result := Driver(SubMenu, M_ScrollUp_Page);
         when KEY_PPAGE =>
            Result := Driver(SubMenu, M_ScrollDown_Page);
         when 10 =>
            if Name(Current(SubMenu)) /= "Close" then
               if Get_Index(Current(DialogForm)) = 4 then
                  Settings.Auto_Refresh_Interval :=
                    Get_Index(Current(SubMenu)) - 1;
               end if;
            end if;
            Show_Options_Tab(1);
            return OPTIONS_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return SECONDS_MENU;
   end Select_Seconds_Keys;

   function Select_Colors_Keys(Key: Key_Code) return UI_Locations is
      Result: Menus.Driver_Result := Unknown_Request;
      ThemeName: constant String := Name(Current(SubMenu));
   begin
      case Key is
         when Key_Home =>
            Result := Driver(SubMenu, M_First_Item);
         when Key_End =>
            Result := Driver(SubMenu, M_Last_Item);
         when KEY_UP =>
            Result := Driver(SubMenu, M_Previous_Item);
         when KEY_DOWN =>
            Result := Driver(SubMenu, M_Next_Item);
         when KEY_NPAGE =>
            Result := Driver(SubMenu, M_ScrollUp_Page);
         when KEY_PPAGE =>
            Result := Driver(SubMenu, M_ScrollDown_Page);
         when 10 =>
            if ThemeName /= "Close" then
               Settings.Color_Theme := To_Unbounded_String(ThemeName);
            end if;
            Show_Options_Tab(1);
            return OPTIONS_VIEW;
         when others =>
            null;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return COLORS_MENU;
   end Select_Colors_Keys;

   function Set_Shortcut_Keys
     (Key: Key_Code; AltKey: Boolean) return UI_Locations is
      CurrentOption: constant Positive := Get_Index(Current(DialogForm));
      Key_Value: constant String := Key_Name(Key);
      New_Key: Unbounded_String := Null_Unbounded_String;
   begin
      if Key = 27 then
         Show_Options_Tab(2);
         return OPTIONS_VIEW;
      end if;
      if AltKey then
         New_Key := To_Unbounded_String("Alt-" & Key_Value);
      else
         if Key_Value(Key_Value'First) /= '^' then
            New_Key := To_Unbounded_String(Key_Value);
         else
            New_Key :=
              To_Unbounded_String
                ("Control-" & To_Lower(Key_Value(Key_Value'Last)));
         end if;
      end if;
      for Accelerator of Accelerators loop
         if Accelerator = New_Key then
            Show_Options_Tab(2);
            return OPTIONS_VIEW;
         end if;
      end loop;
      Accelerators((CurrentOption / 2) + 1) := New_Key;
      Show_Options_Tab(2);
      return OPTIONS_VIEW;
   end Set_Shortcut_Keys;

   function Add_Command_Keys(Key: Key_Code) return UI_Locations is
      Result: Forms.Driver_Result := Unknown_Request;
      CurrentField: constant Positive := Get_Index(Current(CommandForm));
   begin
      case Key is
         when KEY_UP =>
            Result := Driver(CommandForm, F_Previous_Field);
         when KEY_DOWN =>
            Result := Driver(CommandForm, F_Next_Field);
         when KEY_LEFT =>
            if CurrentField in 2 | 4 then
               Result := Driver(CommandForm, F_Previous_Char);
            end if;
         when KEY_RIGHT =>
            if CurrentField in 2 | 4 then
               Result := Driver(CommandForm, F_Next_Char);
            end if;
         when 127 =>
            if CurrentField in 2 | 4 then
               Result := Driver(CommandForm, F_Delete_Previous);
            end if;
         when 10 =>
            if CurrentField = 5 then
               if Get_Buffer(Fields(CommandForm, 5))(1 .. 5) = "Don't" then
                  Set_Buffer(Fields(CommandForm, 5), 0, "Use output");
               else
                  Set_Buffer(Fields(CommandForm, 5), 0, "Don't use output");
               end if;
               Refresh(MenuWindow2);
            end if;
            if CurrentField in 6 | 7 then
               if CurrentField = 7 then
                  declare
                     MenuEntry: constant String :=
                       Trim(Get_Buffer(Fields(CommandForm, 2)), Both);
                     Command: constant String :=
                       Trim(Get_Buffer(Fields(CommandForm, 4)), Both);
                     NeedOutput: constant Boolean :=
                       (if Get_Buffer(Fields(CommandForm, 5))(1 .. 3) = "Use"
                        then True
                        else False);
                  begin
                     if MenuEntry'Length > 0 and Command'Length > 0 then
                        if UserCommandsList.Contains(MenuEntry) then
                           UserCommandsList(MenuEntry) :=
                             (NeedOutput, To_Unbounded_String(Command));
                        else
                           UserCommandsList.Include
                             (MenuEntry,
                              (NeedOutput, To_Unbounded_String(Command)));
                        end if;
                     end if;
                  end;
               end if;
               Show_Options_Tab(3);
               return OPTIONS_VIEW;
            end if;
         when others =>
            if Key /= 91 and CurrentField in 2 | 4 then
               Result := Driver(CommandForm, Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(MenuWindow2);
      end if;
      return COMMAND_FORM;
   end Add_Command_Keys;

end Preferences.UI;
