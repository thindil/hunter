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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Gtk.Switch; use Gtk.Switch;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.Widget; use Gtk.Widget;

package body Preferences is

   procedure TogglePreferences(Object: access Gtkada_Builder_Record'Class) is
      Popup: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "poppreferences"));
   begin
      if Is_Visible(Popup) then
         Hide(Popup);
      else
         Show_All(Popup);
      end if;
   end TogglePreferences;

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
      Settings := (ShowHidden => True);
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") &
           "/.config/hunter/hunter.cfg") then
         return;
      end if;
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
            end if;
         end if;
      end loop;
      Close(ConfigFile);
   end LoadSettings;

   function SaveSettings(Object: access Gtkada_Builder_Record'Class) return Boolean is
      Changed: Boolean := False;
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
      if Get_Active(Gtk_Switch(Get_Object(Object, "switchhidden"))) /=
        Settings.ShowHidden then
         Changed := True;
         Settings.ShowHidden :=
           Get_Active(Gtk_Switch(Get_Object(Object, "switchhidden")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter1")));
         Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "filesfilter2")));
      end if;
      if not Changed then
         return True;
      end if;
      if not Ada.Directories.Exists
          (Ada.Environment_Variables.Value("HOME") & "/.config/hunter") then
         Ada.Directories.Create_Path
           (Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      end if;
      Create
        (ConfigFile, Append_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.cfg");
      SaveBoolean(Settings.ShowHidden, "ShowHidden");
      Close(ConfigFile);
      return True;
   end SaveSettings;

end Preferences;
