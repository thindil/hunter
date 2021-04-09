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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Modules; use Modules;
with UserCommands; use UserCommands;
with Utils.UI; use Utils.UI;

package body Preferences is

   procedure LoadSettings is
      Reader: Tree_Reader;
      DataFile: File_Input;
      SettingsData: Document;
      NodesList: Node_List;
      NodeName: Unbounded_String;
      DataNode: Node;
      function LoadBoolean(Value: String) return Boolean is
      begin
         if Value = "Yes" then
            return True;
         else
            return False;
         end if;
      end LoadBoolean;
   begin
      SetDefaultSettings;
      SetDefaultAccelerators;
      Open
        (Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml",
         DataFile);
      Parse(Reader, DataFile);
      Close(DataFile);
      SettingsData := Get_Tree(Reader);
      NodesList := Child_Nodes(First_Child(SettingsData));
      Load_Settings_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         DataNode := Item(NodesList, I);
         NodeName := To_Unbounded_String(Node_Name(DataNode));
         -- The program settings
         if NodeName = To_Unbounded_String("setting") then
            if Get_Attribute(DataNode, "name") = "ShowHidden" then
               Settings.Show_Hidden :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowLastModified" then
               Settings.Show_Last_Modified :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ScaleImages" then
               Settings.Scale_Images :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") =
              "AutoCloseMessagesTime" then
               Settings.Auto_Close_Messages_Time :=
                 Natural'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "WindowWidth" then
               Settings.Window_Width :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "WindowHeight" then
               Settings.Window_Height :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowPreview" then
               Settings.Show_Preview :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "StayInOld" then
               Settings.Stay_In_Old :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ColorText" then
               Settings.Color_Text :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ColorTheme" then
               Settings.Color_Theme :=
                 To_Unbounded_String(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "DeleteFiles" then
               Settings.DeleteFiles :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ClearTrashOnExit" then
               Settings.ClearTrashOnExit :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ShowFinishedInfo" then
               Settings.ShowFinishedInfo :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "OverwriteOnExist" then
               Settings.OverwriteOnExist :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ToolbarsOnTop" then
               Settings.ToolbarsOnTop :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "AutoRefreshInterval" then
               Settings.AutoRefreshInterval :=
                 Natural'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "UITheme" then
               Settings.UITheme :=
                 To_Unbounded_String(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "ToolbarsSize" then
               Settings.ToolbarsSize :=
                 Positive'Value(Get_Attribute(DataNode, "value"));
            elsif Get_Attribute(DataNode, "name") = "MonospaceFont" then
               Settings.MonospaceFont :=
                 LoadBoolean(Get_Attribute(DataNode, "value"));
            end if;
         -- The keyboard shortcuts
         elsif NodeName = To_Unbounded_String("accelerator") then
            Accelerators(Positive'Value(Get_Attribute(DataNode, "index"))) :=
              To_Unbounded_String(Get_Attribute(DataNode, "value"));
         -- The user defined commands
         elsif NodeName = To_Unbounded_String("command") then
            if Get_Attribute(DataNode, "needoutput") = "Yes" then
               UserCommandsList.Include
                 (Get_Attribute(DataNode, "menuentry"),
                  (NeedOutput => True,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(DataNode)))));
            else
               UserCommandsList.Include
                 (Get_Attribute(DataNode, "menuentry"),
                  (NeedOutput => False,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(DataNode)))));
            end if;
         -- The program modules
         elsif NodeName = To_Unbounded_String("module") then
            Enabled_Modules.Append
              (To_Unbounded_String(Get_Attribute(DataNode, "path")));
         end if;
      end loop Load_Settings_Loop;
      if FindExecutable("highlight") = "" then
         Settings.Color_Text := False;
      end if;
   exception
      when Ada.Directories.Name_Error =>
         null;
   end LoadSettings;

   procedure SavePreferences is
      ConfigFile: File_Type;
      Configuration: DOM_Implementation;
      SettingNode, MainNode: DOM.Core.Element;
      SettingsData: Document;
      UserCommandNode: Text;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         if Value then
            Set_Attribute(SettingNode, "value", "Yes");
         else
            Set_Attribute(SettingNode, "value", "No");
         end if;
      end SaveBoolean;
      procedure SaveNumber(Value: Natural; Name: String) is
         RawValue: constant String :=
           Trim(Natural'Image(Value), Ada.Strings.Left);
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", RawValue);
      end SaveNumber;
      procedure SaveString(Value: Unbounded_String; Name: String) is
      begin
         SettingNode := Create_Element(SettingsData, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", To_String(Value));
      end SaveString;
   begin
      SettingsData := Create_Document(Configuration);
      MainNode := Create_Element(SettingsData, "hunter");
      MainNode := Append_Child(SettingsData, MainNode);
      SaveBoolean(Settings.Show_Hidden, "ShowHidden");
      SaveBoolean(Settings.Show_Last_Modified, "ShowLastModified");
      SaveBoolean(Settings.Scale_Images, "ScaleImages");
      SaveNumber(Settings.Auto_Close_Messages_Time, "AutoCloseMessagesTime");
      SaveNumber(Settings.Window_Width, "WindowWidth");
      SaveNumber(Settings.Window_Height, "WindowHeight");
      SaveBoolean(Settings.Show_Preview, "ShowPreview");
      SaveBoolean(Settings.Stay_In_Old, "StayInOld");
      SaveBoolean(Settings.Color_Text, "ColorText");
      SaveString(Settings.Color_Theme, "ColorTheme");
      SaveBoolean(Settings.DeleteFiles, "DeleteFiles");
      SaveBoolean(Settings.ClearTrashOnExit, "ClearTrashOnExit");
      SaveBoolean(Settings.ShowFinishedInfo, "ShowFinishedInfo");
      SaveBoolean(Settings.OverwriteOnExist, "OverwriteOnExist");
      SaveBoolean(Settings.ToolbarsOnTop, "ToolbarsOnTop");
      SaveNumber(Settings.AutoRefreshInterval, "AutoRefreshInterval");
      SaveString(Settings.UITheme, "UITheme");
      SaveNumber(Settings.ToolbarsSize, "ToolbarsSize");
      SaveBoolean(Settings.MonospaceFont, "MonospaceFont");
      Save_Accelerators_Loop :
      for I in Accelerators'Range loop
         SettingNode := Create_Element(SettingsData, "accelerator");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "index", Trim(Positive'Image(I), Left));
         Set_Attribute(SettingNode, "value", To_String(Accelerators(I)));
      end loop Save_Accelerators_Loop;
      Save_User_Commands_Loop :
      for I in UserCommandsList.Iterate loop
         SettingNode := Create_Element(SettingsData, "command");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "menuentry", Commands_Container.Key(I));
         if UserCommandsList(I).NeedOutput then
            Set_Attribute(SettingNode, "needoutput", "Yes");
         else
            Set_Attribute(SettingNode, "needoutput", "No");
         end if;
         UserCommandNode :=
           Create_Text_Node
             (SettingsData, To_String(UserCommandsList(I).Command));
         UserCommandNode := Append_Child(SettingNode, UserCommandNode);
      end loop Save_User_Commands_Loop;
      Save_Enabled_Modules_Loop :
      for ModuleName of Enabled_Modules loop
         SettingNode := Create_Element(SettingsData, "module");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "path", To_String(ModuleName));
      end loop Save_Enabled_Modules_Loop;
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      Create
        (ConfigFile, Out_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml");
      Write
        (Stream => Stream(ConfigFile), N => SettingsData,
         Pretty_Print => True);
      Close(ConfigFile);
   end SavePreferences;

   procedure SetDefaultSettings is
   begin
      Settings :=
        (Show_Hidden => True, Show_Last_Modified => False,
         Scale_Images => False, Auto_Close_Messages_Time => 10,
         Window_Width => 800, Window_Height => 600, Show_Preview => True,
         Stay_In_Old => False, Color_Text => True,
         Color_Theme => To_Unbounded_String("gruvbox-light-soft"),
         DeleteFiles => True, ClearTrashOnExit => False,
         ShowFinishedInfo => False, OverwriteOnExist => True,
         ToolbarsOnTop => True, AutoRefreshInterval => 10,
         UITheme => To_Unbounded_String("hunter-light"), ToolbarsSize => 24,
         MonospaceFont => False);
   end SetDefaultSettings;

   procedure SetDefaultAccelerators is
   begin
      Accelerators :=
        (To_Unbounded_String("Control-q"), To_Unbounded_String("Alt-h"),
         To_Unbounded_String("Alt-f"), To_Unbounded_String("Alt-n"),
         To_Unbounded_String("Control-Delete"), To_Unbounded_String("Alt-a"),
         To_Unbounded_String("Alt-o"), To_Unbounded_String("Control-a"),
         To_Unbounded_String("Control-r"), To_Unbounded_String("Alt-c"),
         To_Unbounded_String("Alt-m"), To_Unbounded_String("Alt-p"),
         To_Unbounded_String("Alt-w"), To_Unbounded_String("Alt-i"),
         To_Unbounded_String("Alt-v"), To_Unbounded_String("Alt-b"),
         To_Unbounded_String("Alt-d"), To_Unbounded_String("Alt-e"),
         To_Unbounded_String("Alt-s"), To_Unbounded_String("Alt-t"));
   end SetDefaultAccelerators;

end Preferences;
