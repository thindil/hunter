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
      Save_Settings_Data: Document;
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
        (Filename =>
           Ada.Environment_Variables.Value(Name => "HOME") &
           "/.config/hunter/hunter.xml",
         Input => Data_File);
      --## rule off IMPROPER_INITIALIZATION
      Parse(Parser => Reader, Input => Data_File);
      Close(Input => Data_File);
      Save_Settings_Data := Get_Tree(Read => Reader);
      --## rule on IMPROPER_INITIALIZATION
      Nodes_List := Child_Nodes(N => First_Child(N => Save_Settings_Data));
      Load_Settings_Loop :
      for I in 0 .. Length(List => Nodes_List) - 1 loop
         Data_Node := Item(List => Nodes_List, Index => I);
         Data_Node_Name :=
           To_Unbounded_String(Source => Node_Name(N => Data_Node));
         -- The program settings
         if Data_Node_Name = To_Unbounded_String(Source => "setting") then
            if Get_Attribute(Elem => Data_Node, Name => "name") =
              "ShowHidden" then
               Settings.Show_Hidden :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ShowLastModified" then
               Settings.Show_Last_Modified :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ScaleImages" then
               Settings.Scale_Images :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "AutoCloseMessagesTime" then
               Settings.Auto_Close_Messages_Time :=
                 Natural'Value
                   (Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "WindowWidth" then
               Settings.Window_Width :=
                 Positive'Value
                   (Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "WindowHeight" then
               Settings.Window_Height :=
                 Positive'Value
                   (Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ShowPreview" then
               Settings.Show_Preview :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "StayInOld" then
               Settings.Stay_In_Old :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ColorText" then
               Settings.Color_Text :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ColorTheme" then
               Settings.Color_Theme :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "DeleteFiles" then
               Settings.Delete_Files :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ClearTrashOnExit" then
               Settings.Clear_Trash_On_Exit :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ShowFinishedInfo" then
               Settings.Show_Finished_Info :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "OverwriteOnExist" then
               Settings.Overwrite_On_Exist :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ToolbarsOnTop" then
               Settings.Toolbars_On_Top :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "AutoRefreshInterval" then
               Settings.Auto_Refresh_Interval :=
                 Natural'Value
                   (Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "UITheme" then
               Settings.Ui_Theme :=
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "ToolbarsSize" then
               Settings.Toolbars_Size :=
                 Positive'Value
                   (Get_Attribute(Elem => Data_Node, Name => "value"));
            elsif Get_Attribute(Elem => Data_Node, Name => "name") =
              "MonospaceFont" then
               Settings.Monospace_Font :=
                 Load_Boolean
                   (Value =>
                      Get_Attribute(Elem => Data_Node, Name => "value"));
            end if;
         -- The keyboard shortcuts
         elsif Data_Node_Name =
           To_Unbounded_String(Source => "accelerator") then
            Accelerators
              (Positive'Value
                 (Get_Attribute(Elem => Data_Node, Name => "index"))) :=
              To_Unbounded_String
                (Source => Get_Attribute(Elem => Data_Node, Name => "value"));
         -- The user defined commands
         elsif Data_Node_Name = To_Unbounded_String(Source => "command") then
            if Get_Attribute(Elem => Data_Node, Name => "needoutput") =
              "Yes" then
               UserCommandsList.Include
                 (Key => Get_Attribute(Elem => Data_Node, Name => "menuentry"),
                  New_Item =>
                    (NeedOutput => True,
                     Command =>
                       To_Unbounded_String
                         (Source =>
                            Node_Value(N => First_Child(N => Data_Node)))));
            else
               UserCommandsList.Include
                 (Key => Get_Attribute(Elem => Data_Node, Name => "menuentry"),
                  New_Item =>
                    (NeedOutput => False,
                     Command =>
                       To_Unbounded_String
                         (Source =>
                            Node_Value(N => First_Child(N => Data_Node)))));
            end if;
         -- The program modules
         elsif Data_Node_Name = To_Unbounded_String(Source => "module") then
            Enabled_Modules.Append
              (New_Item =>
                 To_Unbounded_String
                   (Source =>
                      Get_Attribute(Elem => Data_Node, Name => "path")));
         end if;
      end loop Load_Settings_Loop;
      if FindExecutable(Name => "highlight") = "" then
         Settings.Color_Text := False;
      end if;
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Load_Settings;

   procedure Save_Preferences is
      Config_File: File_Type;
      Configuration: DOM_Implementation; --## rule line off IMPROPER_INITIALIZATION
      Setting_Node, Main_Node: DOM.Core.Element;
      Save_Settings_Data: Document;
      User_Command_Node: Text;
      procedure Save_Boolean(Value: Boolean; Name: String) is
      begin
         Setting_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element
                  (Doc => Save_Settings_Data, Tag_Name => "setting"));
         Set_Attribute(Elem => Setting_Node, Name => "name", Value => Name);
         if Value then
            Set_Attribute
              (Elem => Setting_Node, Name => "value", Value => "Yes");
         else
            Set_Attribute
              (Elem => Setting_Node, Name => "value", Value => "No");
         end if;
      end Save_Boolean;
      procedure Save_Number(Value: Natural; Name: String) is
         Raw_Value: constant String :=
           Trim(Source => Natural'Image(Value), Side => Ada.Strings.Left);
      begin
         Setting_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element
                  (Doc => Save_Settings_Data, Tag_Name => "setting"));
         Set_Attribute(Elem => Setting_Node, Name => "name", Value => Name);
         Set_Attribute
           (Elem => Setting_Node, Name => "value", Value => Raw_Value);
      end Save_Number;
      procedure Save_String(Value: Unbounded_String; Name: String) is
      begin
         Setting_Node :=
           Append_Child
             (N => Main_Node,
              New_Child =>
                Create_Element
                  (Doc => Save_Settings_Data, Tag_Name => "setting"));
         Set_Attribute(Elem => Setting_Node, Name => "name", Value => Name);
         Set_Attribute
           (Elem => Setting_Node, Name => "value",
            Value => To_String(Source => Value));
      end Save_String;
   begin
      Save_Settings_Data :=
        Create_Document
          (Implementation =>
             Configuration); --## rule line off IMPROPER_INITIALIZATION
      Main_Node :=
        Append_Child
          (N => Save_Settings_Data,
           New_Child =>
             Create_Element(Doc => Save_Settings_Data, Tag_Name => "hunter"));
      Save_Boolean(Settings.Show_Hidden, "ShowHidden");
      Save_Boolean(Settings.Show_Last_Modified, "ShowLastModified");
      Save_Boolean(Settings.Scale_Images, "ScaleImages");
      Save_Number(Settings.Auto_Close_Messages_Time, "AutoCloseMessagesTime");
      Save_Number(Settings.Window_Width, "WindowWidth");
      Save_Number(Settings.Window_Height, "WindowHeight");
      Save_Boolean(Settings.Show_Preview, "ShowPreview");
      Save_Boolean(Settings.Stay_In_Old, "StayInOld");
      Save_Boolean(Settings.Color_Text, "ColorText");
      Save_String(Settings.Color_Theme, "ColorTheme");
      Save_Boolean(Settings.Delete_Files, "DeleteFiles");
      Save_Boolean(Settings.Clear_Trash_On_Exit, "ClearTrashOnExit");
      Save_Boolean(Settings.Show_Finished_Info, "ShowFinishedInfo");
      Save_Boolean(Settings.Overwrite_On_Exist, "OverwriteOnExist");
      Save_Boolean(Settings.Toolbars_On_Top, "ToolbarsOnTop");
      Save_Number(Settings.Auto_Refresh_Interval, "AutoRefreshInterval");
      Save_String(Settings.Ui_Theme, "UITheme");
      Save_Number(Settings.Toolbars_Size, "ToolbarsSize");
      Save_Boolean(Settings.Monospace_Font, "MonospaceFont");
      Save_Accelerators_Loop :
      for I in Accelerators'Range loop
         Setting_Node := Create_Element(Save_Settings_Data, "accelerator");
         Setting_Node := Append_Child(Main_Node, Setting_Node);
         Set_Attribute(Setting_Node, "index", Trim(Positive'Image(I), Left));
         Set_Attribute(Setting_Node, "value", To_String(Accelerators(I)));
      end loop Save_Accelerators_Loop;
      Save_User_Commands_Loop :
      for I in UserCommandsList.Iterate loop
         Setting_Node := Create_Element(Save_Settings_Data, "command");
         Setting_Node := Append_Child(Main_Node, Setting_Node);
         Set_Attribute(Setting_Node, "menuentry", Commands_Container.Key(I));
         if UserCommandsList(I).NeedOutput then
            Set_Attribute(Setting_Node, "needoutput", "Yes");
         else
            Set_Attribute(Setting_Node, "needoutput", "No");
         end if;
         User_Command_Node :=
           Create_Text_Node
             (Save_Settings_Data, To_String(UserCommandsList(I).Command));
         User_Command_Node := Append_Child(Setting_Node, User_Command_Node);
      end loop Save_User_Commands_Loop;
      Save_Enabled_Modules_Loop :
      for ModuleName of Enabled_Modules loop
         Setting_Node := Create_Element(Save_Settings_Data, "module");
         Setting_Node := Append_Child(Main_Node, Setting_Node);
         Set_Attribute(Setting_Node, "path", To_String(ModuleName));
      end loop Save_Enabled_Modules_Loop;
      Create_Path(Ada.Environment_Variables.Value("HOME") & "/.config/hunter");
      Create
        (Config_File, Out_File,
         Ada.Environment_Variables.Value("HOME") &
         "/.config/hunter/hunter.xml");
      Write
        (Stream => Stream(Config_File), N => Save_Settings_Data,
         Pretty_Print => True);
      Close(Config_File);
   end Save_Preferences;

end Preferences;
