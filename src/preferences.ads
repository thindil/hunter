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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Hunter/Preferences
-- FUNCTION
-- Provide code for save/restore and manipulate the program settings
-- SOURCE
package Preferences is
-- ****

   -- ****t* Preferences/Settings_Data
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
-- OverwriteOnExist      - If true, copied or moved file or directory will be
   --                         overwritting existing with that same name.
   -- SOURCE
   type Settings_Data is record
      ShowHidden: Boolean;
      ShowLastModified: Boolean;
      ScaleImages: Boolean;
      AutoCloseMessagesTime: Natural;
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
   end record;
   -- ****

   -- ****v* Preferences/Settings
   -- FUNCTION
   -- The program settings
   -- SOURCE
   Settings: Settings_Data;
   -- ****

   -- ****f* Preferences/TogglePreferences
   -- FUNCTION
   -- Show or hide the program preferences window
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SOURCE
   procedure TogglePreferences(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Preferences/SetDeleteTooltip
   -- FUNCTION
   -- Set tooltip for delete button, depends did delete action delete files or
   -- move them to trash
   -- SOURCE
   procedure SetDeleteTooltip;
   -- ****

   -- ****f* Preferences/LoadSettings
   -- FUNCTION
   -- Load the program settings from file. If file not exists, load default
   -- settings.
   -- SOURCE
   procedure LoadSettings;
   -- ****

   -- ****f* Preferences/SaveSettings
   -- FUNCTION
   -- Save the program settings to file and update program to the new
   -- configuration if needed.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- RESULT
   -- Always False so default handler will be running too.
   -- SEE ALSO
   -- SaveSettingsProc
   -- SOURCE
   function SaveSettings
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****

   -- ****f* Preferences/SaveSettingsProc
   -- FUNCTION
   -- Save the program settings to file and update program to the new
   -- configuration if needed. Some GTK elements need procedures instead
   -- of function.
   -- PARAMETERS
   -- Object - GtkAda Builder used to create UI
   -- SEE ALSO
   -- SaveSettings
   -- SOURCE
   procedure SaveSettingsProc(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Preferences/SavePreferences
   -- FUNCTION
   -- Save the program preferences to the file.
   -- SOURCE
   procedure SavePreferences;
   -- ****

end Preferences;
