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
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Preferences.Commands; use Preferences.Commands;
with Utils; use Utils;

package body Preferences is

   procedure LoadSettings is
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      function LoadBoolean return Boolean is
      begin
         if Value = To_Unbounded_String("Yes") then
            return True;
         end if;
         return False;
      end LoadBoolean;
   begin
      Settings :=
        (ShowHidden => True, ShowLastModified => False, ScaleImages => False,
         AutoCloseMessagesTime => 10, WindowWidth => 800, WindowHeight => 600,
         ShowPreview => True, StayInOld => False, ColorText => True,
         ColorTheme => To_Unbounded_String("gruvbox-light-soft"),
         DeleteFiles => True, ClearTrashOnExit => False,
         ShowFinishedInfo => False, OverwriteOnExist => True,
         ToolbarsOnTop => True, AutoRefreshInterval => 10,
         UITheme => To_Unbounded_String("light"));
      Open
        (ConfigFile, In_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      while not End_Of_File(ConfigFile) loop
         RawData := To_Unbounded_String(Get_Line(ConfigFile));
         if Length(RawData) > 0 then
            EqualIndex := Index(RawData, "=");
            FieldName := Head(RawData, EqualIndex - 2);
            Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
            if FieldName = To_Unbounded_String("ShowHidden") then
               Settings.ShowHidden := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowLastModified") then
               Settings.ShowLastModified := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ScaleImages") then
               Settings.ScaleImages := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoCloseMessagesTime") then
               Settings.AutoCloseMessagesTime :=
                 Natural'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowWidth") then
               Settings.WindowWidth := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("WindowHeight") then
               Settings.WindowHeight := Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("ShowPreview") then
               Settings.ShowPreview := LoadBoolean;
            elsif FieldName = To_Unbounded_String("StayInOld") then
               Settings.StayInOld := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ColorText") then
               Settings.ColorText := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ColorTheme") then
               Settings.ColorTheme := Value;
            elsif FieldName = To_Unbounded_String("DeleteFiles") then
               Settings.DeleteFiles := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ClearTrashOnExit") then
               Settings.ClearTrashOnExit := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ShowFinishedInfo") then
               Settings.ShowFinishedInfo := LoadBoolean;
            elsif FieldName = To_Unbounded_String("OverwriteOnExist") then
               Settings.OverwriteOnExist := LoadBoolean;
            elsif FieldName = To_Unbounded_String("ToolbarsOnTop") then
               Settings.ToolbarsOnTop := LoadBoolean;
            elsif FieldName = To_Unbounded_String("AutoRefreshInterval") then
               Settings.AutoRefreshInterval :=
                 Positive'Value(To_String(Value));
            elsif FieldName = To_Unbounded_String("UITheme") then
               Settings.UITheme := Value;
            end if;
         end if;
      end loop;
      if FindExecutable("highlight") = "" then
         Settings.ColorText := False;
      end if;
      Close(ConfigFile);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end LoadSettings;

   procedure SavePreferences is
      ConfigFile: File_Type;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         if Value then
            Put_Line(ConfigFile, Name & " = Yes");
         else
            Put_Line(ConfigFile, Name & " = No");
         end if;
      end SaveBoolean;
   begin
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") & "/.config/hunter") then
         Create_Path
           (Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      end if;
      Create
        (ConfigFile, Append_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      SaveBoolean(Settings.ShowHidden, "ShowHidden");
      SaveBoolean(Settings.ShowLastModified, "ShowLastModified");
      SaveBoolean(Settings.ScaleImages, "ScaleImages");
      Put_Line
        (ConfigFile,
         "AutoCloseMessagesTime =" &
         Natural'Image(Settings.AutoCloseMessagesTime));
      Put_Line
        (ConfigFile, "WindowWidth =" & Positive'Image(Settings.WindowWidth));
      Put_Line
        (ConfigFile, "WindowHeight =" & Positive'Image(Settings.WindowHeight));
      SaveBoolean(Settings.ShowPreview, "ShowPreview");
      SaveBoolean(Settings.StayInOld, "StayInOld");
      SaveBoolean(Settings.ColorText, "ColorText");
      Put_Line(ConfigFile, "ColorTheme = " & To_String(Settings.ColorTheme));
      SaveBoolean(Settings.DeleteFiles, "DeleteFiles");
      SaveBoolean(Settings.ClearTrashOnExit, "ClearTrashOnExit");
      SaveBoolean(Settings.ShowFinishedInfo, "ShowFinishedInfo");
      SaveBoolean(Settings.OverwriteOnExist, "OverwriteOnExist");
      SaveBoolean(Settings.ToolbarsOnTop, "ToolbarsOnTop");
      Put_Line
        (ConfigFile,
         "AutoRefreshInterval =" &
         Positive'Image(Settings.AutoRefreshInterval));
      Put_Line(ConfigFile, "UITheme = " & To_String(Settings.UITheme));
      Close(ConfigFile);
   end SavePreferences;

   procedure CreatePreferencesUI is
   begin
      AddCommands;
   end CreatePreferencesUI;

   procedure LoadTheme is
   begin
      if Settings.UITheme = To_Unbounded_String("light") then
         Tcl_EvalFile(Get_Context, "../share/hunter/themes/light/breeze.tcl");
         Theme_Use("breeze");
      elsif Settings.UITheme = To_Unbounded_String("dark") then
         Tcl_EvalFile
           (Get_Context, "../share/hunter/themes/dark/breeze-dark.tcl");
         Theme_Use("breeze-dark");
      else
         Theme_Use(To_String(Settings.UITheme));
      end if;
   end LoadTheme;

end Preferences;
