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

   procedure Load_Settings is
      Reader: Tree_Reader; --## rule line off IMPROPER_INITIALIZATION
      Data_File: File_Input;
      Saved_Settings_Data: Document;
      Nodes_List: Node_List;
      Data_Node_Name: Unbounded_String := Null_Unbounded_String;
      Data_Node: Node;
      function Load_Boolean(Value: String) return Boolean is
      begin
         if Value = "Yes" then
            return True;
         end if;
         return False;
      end Load_Boolean;
   begin
      Open
        (Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml",
         Data_File);
      Parse(Reader, Data_File); --## rule line off IMPROPER_INITIALIZATION
      Close(Data_File);
      Saved_Settings_Data :=
        Get_Tree(Reader); --## rule line off IMPROPER_INITIALIZATION
      Nodes_List := Child_Nodes(First_Child(Saved_Settings_Data));
      Load_Settings_Loop :
      for I in 0 .. Length(Nodes_List) - 1 loop
         Data_Node := Item(Nodes_List, I);
         Data_Node_Name := To_Unbounded_String(Node_Name(Data_Node));
         -- The program settings
         if Data_Node_Name = To_Unbounded_String("setting") then
            if Get_Attribute(Data_Node, "name") = "ShowHidden" then
               Settings.Show_Hidden :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ShowLastModified" then
               Settings.Show_Last_Modified :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ScaleImages" then
               Settings.Scale_Images :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") =
              "AutoCloseMessagesTime" then
               Settings.Auto_Close_Messages_Time :=
                 Natural'Value(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "WindowWidth" then
               Settings.Window_Width :=
                 Positive'Value(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "WindowHeight" then
               Settings.Window_Height :=
                 Positive'Value(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ShowPreview" then
               Settings.Show_Preview :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "StayInOld" then
               Settings.Stay_In_Old :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ColorText" then
               Settings.Color_Text :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ColorTheme" then
               Settings.Color_Theme :=
                 To_Unbounded_String(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "DeleteFiles" then
               Settings.Delete_Files :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ClearTrashOnExit" then
               Settings.Clear_Trash_On_Exit :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ShowFinishedInfo" then
               Settings.Show_Finished_Info :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "OverwriteOnExist" then
               Settings.Overwrite_On_Exist :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ToolbarsOnTop" then
               Settings.Toolbars_On_Top :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "AutoRefreshInterval" then
               Settings.Auto_Refresh_Interval :=
                 Natural'Value(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "UITheme" then
               Settings.Ui_Theme :=
                 To_Unbounded_String(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "ToolbarsSize" then
               Settings.Toolbars_Size :=
                 Positive'Value(Get_Attribute(Data_Node, "value"));
            elsif Get_Attribute(Data_Node, "name") = "MonospaceFont" then
               Settings.Monospace_Font :=
                 Load_Boolean(Get_Attribute(Data_Node, "value"));
            end if;
         -- The keyboard shortcuts
         elsif Data_Node_Name = To_Unbounded_String("accelerator") then
            Accelerators(Positive'Value(Get_Attribute(Data_Node, "index"))) :=
              To_Unbounded_String(Get_Attribute(Data_Node, "value"));
         -- The user defined commands
         elsif Data_Node_Name = To_Unbounded_String("command") then
            if Get_Attribute(Data_Node, "needoutput") = "Yes" then
               UserCommandsList.Include
                 (Get_Attribute(Data_Node, "menuentry"),
                  (NeedOutput => True,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(Data_Node)))));
            else
               UserCommandsList.Include
                 (Get_Attribute(Data_Node, "menuentry"),
                  (NeedOutput => False,
                   Command =>
                     To_Unbounded_String(Node_Value(First_Child(Data_Node)))));
            end if;
         -- The program modules
         elsif Data_Node_Name = To_Unbounded_String("module") then
            Enabled_Modules.Append
              (To_Unbounded_String(Get_Attribute(Data_Node, "path")));
         end if;
      end loop Load_Settings_Loop;
      if FindExecutable("highlight") = "" then
         Settings.Color_Text := False;
      end if;
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Load_Settings;

   procedure Save_Preferences is
      ConfigFile: File_Type;
      Configuration: DOM_Implementation;
      SettingNode, MainNode: DOM.Core.Element;
      Settings_Data: Document;
      UserCommandNode: Text;
      procedure SaveBoolean(Value: Boolean; Name: String) is
      begin
         SettingNode := Create_Element(Settings_Data, "setting");
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
         SettingNode := Create_Element(Settings_Data, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", RawValue);
      end SaveNumber;
      procedure SaveString(Value: Unbounded_String; Name: String) is
      begin
         SettingNode := Create_Element(Settings_Data, "setting");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "name", Name);
         Set_Attribute(SettingNode, "value", To_String(Value));
      end SaveString;
   begin
      Settings_Data := Create_Document(Configuration);
      MainNode := Create_Element(Settings_Data, "hunter");
      MainNode := Append_Child(Settings_Data, MainNode);
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
      SaveBoolean(Settings.Delete_Files, "DeleteFiles");
      SaveBoolean(Settings.Clear_Trash_On_Exit, "ClearTrashOnExit");
      SaveBoolean(Settings.Show_Finished_Info, "ShowFinishedInfo");
      SaveBoolean(Settings.Overwrite_On_Exist, "OverwriteOnExist");
      SaveBoolean(Settings.Toolbars_On_Top, "ToolbarsOnTop");
      SaveNumber(Settings.Auto_Refresh_Interval, "AutoRefreshInterval");
      SaveString(Settings.Ui_Theme, "UITheme");
      SaveNumber(Settings.Toolbars_Size, "ToolbarsSize");
      SaveBoolean(Settings.Monospace_Font, "MonospaceFont");
      Save_Accelerators_Loop :
      for I in Accelerators'Range loop
         SettingNode := Create_Element(Settings_Data, "accelerator");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "index", Trim(Positive'Image(I), Left));
         Set_Attribute(SettingNode, "value", To_String(Accelerators(I)));
      end loop Save_Accelerators_Loop;
      Save_User_Commands_Loop :
      for I in UserCommandsList.Iterate loop
         SettingNode := Create_Element(Settings_Data, "command");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "menuentry", Commands_Container.Key(I));
         if UserCommandsList(I).NeedOutput then
            Set_Attribute(SettingNode, "needoutput", "Yes");
         else
            Set_Attribute(SettingNode, "needoutput", "No");
         end if;
         UserCommandNode :=
           Create_Text_Node
             (Settings_Data, To_String(UserCommandsList(I).Command));
         UserCommandNode := Append_Child(SettingNode, UserCommandNode);
      end loop Save_User_Commands_Loop;
      Save_Enabled_Modules_Loop :
      for ModuleName of Enabled_Modules loop
         SettingNode := Create_Element(Settings_Data, "module");
         SettingNode := Append_Child(MainNode, SettingNode);
         Set_Attribute(SettingNode, "path", To_String(ModuleName));
      end loop Save_Enabled_Modules_Loop;
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      Create
        (ConfigFile, Out_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml");
      Write
        (Stream => Stream(ConfigFile), N => Settings_Data,
         Pretty_Print => True);
      Close(ConfigFile);
   end Save_Preferences;

end Preferences;
