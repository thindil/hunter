-- Copyright (c) 2019-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- ****h* Preferences/Preferences
-- FUNCTION
-- Provide code for save/restore and manipulate the program settings
-- SOURCE
package Preferences is
-- ****

   -- ****s* Preferences/Preferences.Settings_Data
   -- FUNCTION
   -- Data structure to the program settings
   -- OPTIONS
   -- Show_Hidden              - If true, show hidden files
   -- Show_Last_Modifi ed      - If true, show column with last modification time
   --                            on files list
   -- Scale_Images             - If true, scale images in preview
   -- Auto_Close Messages_Time - Amount of seconds after which current message
   --                            will be auto hidden
   -- Window_Width             - Width of the main window in pixels
   -- Window_Height            - Height of the main window in pixels
   -- Show_Preview             - If true, show preview panel for files and
   --                            directories
   -- Stay_In_Old              - If true, after copying, moving files and
   --                            directories or creating new link, stay in old
   --                            directory instead of going to the destination
   --                            directory.
   -- Color_Text               - If true, try color syntax in text files in
   --                            preview
   -- Color_Theme              - Selected by user color theme for coloring syntax
   --                            in text files in preview
   -- Delete_Files             - If true, delete file and directories instead of
   --                            moving them to trash
   -- Clear_Trash_On_Exit      - Clear trash on quit from the program
   -- Show_Finished_Info       - If true, show info about finished copying, moving
   --                            deleting files and directories
   -- Overwrite_On_Exist       - If true, copied or moved file or directory will
   --                            be overwritting existing with that same name
   -- Toolbars_On_Top          - If true, toolbars for actions and information
   --                            will be show on the top of the window. Otherwise
   --                            toolbar action will be on the left and toolbar
   --                            with information will be on the right
   -- Auto_Refresh_Interval    - How often, in seconds, auto refresh should be
   --                            triggered
   -- Ui_Theme                 - Tk theme used by the program
   -- Toolbars_Size            - Size of the toolbars icons
   -- Monospace_Font           - If true, use monospace font in text files preview
   -- SOURCE
   type Settings_Data is record
      Show_Hidden: Boolean;
      Show_Last_Modified: Boolean;
      Scale_Images: Boolean;
      Auto_Close_Messages_Time: Natural range 0 .. 60;
      Window_Width: Positive;
      Window_Height: Positive;
      Show_Preview: Boolean;
      Stay_In_Old: Boolean;
      Color_Text: Boolean;
      Color_Theme: Unbounded_String;
      Delete_Files: Boolean;
      Clear_Trash_On_Exit: Boolean;
      Show_Finished_Info: Boolean;
      Overwrite_On_Exist: Boolean;
      Toolbars_On_Top: Boolean;
      Auto_Refresh_Interval: Natural range 0 .. 30;
      Ui_Theme: Unbounded_String;
      Toolbars_Size: Positive range 8 .. 128;
      Monospace_Font: Boolean;
   end record;
   -- ****

   -- ****d* Preferences/Preferenes.Default_Settings
   -- FUNCTION
   -- Default the program settings
   -- SOURCE
   Default_Settings: constant Settings_Data :=
     (Show_Hidden => True, Show_Last_Modified => False, Scale_Images => False,
      Auto_Close_Messages_Time => 10, Window_Width => 800,
      Window_Height => 600, Show_Preview => True, Stay_In_Old => False,
      Color_Text => True,
      Color_Theme => To_Unbounded_String(Source => "gruvbox-light-soft"),
      Delete_Files => True, Clear_Trash_On_Exit => False,
      Show_Finished_Info => False, Overwrite_On_Exist => True,
      Toolbars_On_Top => True, Auto_Refresh_Interval => 10,
      Ui_Theme => To_Unbounded_String(Source => "hunter-light"),
      Toolbars_Size => 24, Monospace_Font => False);
   -- ****

   --## rule off GLOBAL_REFERENCES
   -- ****v* Preferences/Preferences.Settings
   -- FUNCTION
   -- The program settings
   -- SOURCE
   Settings: Settings_Data := Default_Settings;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****t* Preferences/Preferences.Accelerators_Array
   -- FUNCTION
   -- Array used to store keyboard shortcuts
   -- SOURCE
   type Accelerators_Array is array(1 .. 21) of Unbounded_String;
   -- ****

   -- ****d* Preferences/Preferences.Default_Accelerators
   -- FUNCTION
   -- Default the program keyboard shortcuts
   -- SOURCE
   Default_Accelerators: constant Accelerators_Array :=
     (1 => To_Unbounded_String(Source => "Alt-q"),
      2 => To_Unbounded_String(Source => "Alt-h"),
      3 => To_Unbounded_String(Source => "Alt-f"),
      4 => To_Unbounded_String(Source => "Alt-n"),
      5 => To_Unbounded_String(Source => "Control-Delete"),
      6 => To_Unbounded_String(Source => "Alt-a"),
      7 => To_Unbounded_String(Source => "Alt-o"),
      8 => To_Unbounded_String(Source => "Control-a"),
      9 => To_Unbounded_String(Source => "Control-r"),
      10 => To_Unbounded_String(Source => "Alt-c"),
      11 => To_Unbounded_String(Source => "Alt-m"),
      12 => To_Unbounded_String(Source => "Alt-p"),
      13 => To_Unbounded_String(Source => "Alt-w"),
      14 => To_Unbounded_String(Source => "Alt-i"),
      15 => To_Unbounded_String(Source => "Alt-v"),
      16 => To_Unbounded_String(Source => "Alt-b"),
      17 => To_Unbounded_String(Source => "Alt-d"),
      18 => To_Unbounded_String(Source => "Alt-e"),
      19 => To_Unbounded_String(Source => "Alt-s"),
      20 => To_Unbounded_String(Source => "Alt-t"),
      21 => To_Unbounded_String(Source => "Control-h"));
   -- ****

   --## rule off GLOBAL_REFERENCES
   -- ****v* Preferences/Preferences.Accelerators
   -- FUNCTION
   -- Array with keyboard shortcuts used by the program
   -- SOURCE
   Accelerators: Accelerators_Array := Default_Accelerators;
   -- ****
   --## rule on GLOBAL_REFERENCES

   -- ****f* Preferences/Preferences.Load_Settings
   -- FUNCTION
   -- Load the program settings from file. If file not exists, load default
   -- settings.
   -- SOURCE
   procedure Load_Settings;
   -- ****

   -- ****f* Preferences/Preferences.Save_Preferences
   -- FUNCTION
   -- Save the program preferences to the file.
   -- SOURCE
   procedure Save_Preferences;
   -- ****

end Preferences;
