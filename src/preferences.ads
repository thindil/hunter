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
   -- ShowHidden            - If true, show hidden files
   -- ShowLastModified      - If true, show column with last modification time
   --                         on files list
   -- ScaleImages           - If true, scale images in preview
   -- AutoCloseMessagesTime - Amount of seconds after which current message
   --                         will be auto hidden
   -- WindowWidth           - Width of the main window in pixels
   -- WindowHeight          - Height of the main window in pixels
   -- ShowPreview           - If true, show preview panel for files and
   --                         directories
   -- StayInOld             - If true, after copying, moving files and
   --                         directories or creating new link, stay in old
   --                         directory instead of going to the destination
   --                         directory.
   -- ColorText             - If true, try color syntax in text files in
   --                         preview
   -- ColorTheme            - Selected by user color theme for coloring syntax
   --                         in text files in preview
   -- DeleteFiles           - If true, delete file and directories instead of
   --                         moving them to trash
   -- ClearTrashOnExit      - Clear trash on quit from the program
   -- ShowFinishedInfo      - If true, show info about finished copying, moving
   --                         deleting files and directories
   -- OverwriteOnExist      - If true, copied or moved file or directory will
   --                         be overwritting existing with that same name
   -- ToolbarsOnTop         - If true, toolbars for actions and information
   --                         will be show on the top of the window. Otherwise
   --                         toolbar action will be on the left and toolbar
   --                         with information will be on the right
   -- AutoRefreshInterval   - How often, in seconds, auto refresh should be
   --                         triggered
   -- UITheme               - Tk theme used by the program
   -- ToolbarsSize          - Size of the toolbars icons
   -- MonospaceFont         - If true, use monospace font in text files preview
   -- SOURCE
   type Settings_Data is record
      ShowHidden: Boolean;
      ShowLastModified: Boolean;
      ScaleImages: Boolean;
      AutoCloseMessagesTime: Natural range 0 .. 60;
      WindowWidth: Positive;
      WindowHeight: Positive;
      ShowPreview: Boolean;
      StayInOld: Boolean;
      ColorText: Boolean;
      ColorTheme: Unbounded_String;
      DeleteFiles: Boolean;
      ClearTrashOnExit: Boolean;
      ShowFinishedInfo: Boolean;
      OverwriteOnExist: Boolean;
      ToolbarsOnTop: Boolean;
      AutoRefreshInterval: Natural range 0 .. 30;
      UITheme: Unbounded_String;
      ToolbarsSize: Positive range 8 .. 128;
      MonospaceFont: Boolean;
   end record;
   -- ****

   -- ****v* Preferences/Preferences.Settings
   -- FUNCTION
   -- The program settings
   -- SOURCE
   Settings: Settings_Data;
   -- ****

   -- ****t* Preferences/Preferences.Accelerators_Array
   -- FUNCTION
   -- Array used to store keyboard shortcuts
   -- SOURCE
   type Accelerators_Array is array(1 .. 20) of Unbounded_String;
   -- ****

   -- ****v* Preferences/Preferences.Accelerators
   -- FUNCTION
   -- Array with keyboard shortcuts used by the program
   -- SOURCE
   Accelerators: Accelerators_Array;
   -- ****

   -- ****f* Preferences/Preferences.LoadSettings
   -- FUNCTION
   -- Load the program settings from file. If file not exists, load default
   -- settings.
   -- SOURCE
   procedure LoadSettings;
   -- ****

   -- ****f* Preferences/Preferences.SavePreferences
   -- FUNCTION
   -- Save the program preferences to the file.
   -- SOURCE
   procedure SavePreferences;
   -- ****

   -- ****f* Preferences/Preferences.CreatePreferences
   -- FUNCTION
   -- Create preferences UI and fill it with data from the program settings
   -- SOURCE
   procedure CreatePreferencesUI;
   -- ****

private

   -- ****f* Preferences/Preferences.SetDefaultSettings
   -- FUNCTION
   -- Set default values for the program's settings
   -- SOURCE
   procedure SetDefaultSettings;
   -- ****

   -- ****f* Preferences/Preferences.SetDefaultAccelerators
   -- FUNCTION
   -- Set default values for the program's keyboard shortcuts
   -- SOURCE
   procedure SetDefaultAccelerators;
   -- ****

   -- ****f* Preferences/Preferences.Clear_Add_Command
   -- FUNCTION
   -- Clear form for edit or add user defined commands
   -- SOURCE
   procedure Clear_Add_Command;
   -- ****

end Preferences;
