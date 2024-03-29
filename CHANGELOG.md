# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Use Enter key to accept values in dialogs in console version
- Use Escape key to close dialogs in console version
- Use Escape key to close menus in console version
- Missing translations
- Keyboard shortcut for toggling visibility of hidden files and directories
- Option to right click menu for toggle visibility of hidden files and
  directories

### Changed
- During confirmation of deleting files and directories, show only names of
  items to delete, not their full paths
- Updated README.md
- Updated Polish translation
- Updated contributing guide
- During copying or moving files and directories, bookmarks change destination
  directory, not source
- Use left and right arrows to move between path buttons in console version
- Moved all actions in Trash in console version to Actions menu
- The default keyboard shortcut for quitting from program to Alt-q
- The default keyboard shortcut for deleting files and directories to Alt-r

### Removed
- Quitting from the console version of the program with Alt-q keys

### Fixed
- Don't auto hide questions or confirmation messages in graphical version
- Refresh preview after showing various dialogs in console version
- Showing information about invalid command when trying execute file or
  directory with it in console version
- Crash after copying files or directories when the program is set to stay
  in the old directory in console version
- Crash after moving files or directories when the program is set to stay
  in the old directory in console version
- Crash when activate file in destination directory during copying or moving
  files or directories in console version
- Moving up and down the list of files in directories in preview of directory
  in console version
- Crash when trying to open the project's website in about dialog in console
  and graphical versions when xdg-open isn't installed
- Showing info about the selected file or directory in Trash in console version
- Showing the main program menu in Trash after deleting file or directory from
  it in console version
- Crash when trying to add bookmark when directory with bookmarks' settings
  doesn't exist
- Showing remove bookmark button in the graphical version of the program
- Showing remove bookmark button for the standard bookmarks in the console
  version of the program
- Showing path buttons after start the program in the graphical version
- Showing path buttons for destination directory after starting copying or
  moving files and directories in the graphical version
- Not working path buttons after finishing copying or moving files and
  directories in the graphical version

## [1.6] - 2021-08-08

### Added
- Missing translations
- Path buttons in Trash
- Console version of the program

### Changed
- Updated the program themes
- Better default look of the program
- Updated Ada language definition with aspects and pragmas
- Updated Polish translation
- Updated README.md
- Updated contributing guide

### Fixed
- Showing preferences button on fresh start
- Crash when going up from the home directory
- Update path buttons when showing the selected program's module directory
- Some typos in UI
- Showing translated info for copy and move buttons
- Showing preview of empty directories when entering them
- Possible crash on auto refresh directory listing
- Possible crash on loading preview when excutable script is there
- Showing preview for some types of text files
- Crash on showing files in About menu
- Showing content of directories when entering them
- Crash on executing the program's modules
- Deleting items in Trash

## [1.5] - 2020-11-20

### Added
- Option to configure the program keyboard shortcuts
- Separated keyboard shortcut for restore files or directories from Trash
- User defined commands and actions
- Blocked option to remove system (based on XDG directories) bookmarks
- Modding guide
- Option to extend the program with external modules
- Option to show modding guide in the program

### Changed
- Show preferences in main window instead as a separated dialog
- Keyboard shortcut for quitting from the program now works only when quit
  button is visible
- Disabled Tab/Alt-Tab traversing by toolbars
- Default keyboard shortcut for removing bookmarks
- Hide Trash actions buttons when Trash is empty
- Updated README.md
- Replace old configuration file with new based on xml

### Fixed
- Tooltips for remove bookmark, select all and file information buttons
- Open file or directory with command when command has arguments
- Don't enter the selected directory on header clicking
- Added missing translations
- Running file or directory with command when file or directory contains space
- Crash on creating bookmark when bookmarks file doesn't exists
- Crash during starting program when bookmarks file contains invalid data
- Openining files with spaces in names
- Typos in README.md, CONTRIBUTING.md
- Showing path buttons for destination after finished copying, moving or
  creating link to file or directory
- Hang when trying to refresh the Trash view
- Showing content of directories in Trash

## [1.4] - 2020-08-19

### Added
- Information about the program translators
- Bring back the Polish translation
- Light and dark theme for the program
- Ability to set monospace font in text files preview
- Ability to restore the program's default settings

### Changed
- Better detection of dialogs height
- Updated README.md
- Updated contributing guide
- Autohide scrollbars
- Use localized names for the default bookmarks

### Fixed
- Crash on lack of configuration file
- Crash on very large files
- Crash on showing info about items with space in their names
- Deleting items from right click menu
- Refreshing info about changed file or directory
- Setting location of toolbars
- Show proper buttons when changing toolbars location
- Showing buttons after finishing moving/copying items
- Showing name of home directory bookmark
- Hide path buttons for destination directory after ending copying/moving items
- Refreshing preview in empty Trash
- Don't show "Show trash" option in Trash
- Refreshing tooltip for path button in Trash

## [1.3] - 2020-07-10

### Changed
- Updated contributing guide
- Updated README.md
- Rewritten to use Tk library instead of GTK

## [1.2.1] - 2020-01-21

### Fixed
- Crash when associated with file program is not installed
- Showing error info when crash occurs
- Showing info about file on start the program

## [1.2] - 2020-01-18

### Added
- Refresh preview of files and directories if they are changed on disk

### Changed
- Updated README.md
- Updated contributing guide
- Autorefresh directory listing on any change now should be a lot faster
- Simplified autorefresh directory listing setting
- Moved whole UI to code
- Updated Polish translation of the program

### Fixed
- Typos in contributing guide and changelog
- Searching for files or directories during copying or moving
- Not clearing preview on empty search result
- Refreshing bookmarks buttons after adding/deleting bookmark
- Set focus on text entry when entering custom location to go
- Showing deletion, moving, copying progress

## [1.1] - 2019-10-28

### Added
- Limit amount of items to delete in message to 10

### Changed
- Updated contributing guide
- Updated README.md
- Updated Polish translation of the program

### Fixed
- Memory leaks on auto-scaling images

## [1.0] - 2019-09-22

### Added
- Contributing guide
- Option to show contributing guide file

### Changed
- Updated README.md
- Updated Polish translation of the program
- Updated code documentation

### Fixed
- Detecting files types
- Crash when libmagic database file can't be found
- Canceling moving items with Cancel button

## [0.9] - 2019-09-12

### Added
- Option to show README.md and CHANGELOG.md files
- Option to set position of toolbars to the top or the left of the main window
- Option to select or unselect all files and directories in current directory
- Option to search for files and directories during copying or moving
- Autorefreshing directory listing on any change to it files or directories
- Option to enable or disable autorefresh and ability to set interval for it

### Changed
- Updated README.md
- Updated Polish translation of the program
- Updated code documentation

### Fixed
- Counting amount of files and directories in directories info
- Missing translations of bookmarks
- Showing destination directory when changed selected files and directories
  for copying or moving
- Loading setting for the syntax highlighting theme
- Showing some buttons after quit from Trash directory
- Hide delete and restore buttons when Trash is empty

## [0.8] - 2019-08-26

### Added
- Option to show information about finished copying, moving and deleting files
  and directories
- Info about progress of copying, moving or deleting files and directories
- Option to rename instead of overwriting copied or moved file or directory
  when in destination directory exists something with that same name.

### Changed
- Updated README.md
- Updated interface
- Updated Polish translation of the program
- Updated code documentation

### Fixed
- Loading syntax highlighting setting when no config file
- Showing '&' symbol in syntax highlighting
- Showing items actions buttons after closing creating items entry
- GTK warning on changing syntax colors scheme
- Crash on previewing files
- Showing proper buttons after delete files or directories from trash
- Showing directory with relative path at start of the program
- Cancelling copying or moving files or directories

## [0.7] - 2019-08-09

### Added
- Option to stay in old directory after copying or moving files and
  directories or creating links
- Syntax color for text files in preview
- Option to enable or disable coloring syntax for text files in preview
- Option to select color theme for coloring syntax of text files in preview
- Option to select delete files and directories or move them to trash
- Option to clean trash
- Option to clean trash on exit from the program
- Option to restore files and directories from trash
- Option to delete files and directories from trash

### Changed
- Updated README.md
- Hide files and directories actions buttons in empty directories
- Updated Polish translation of the program

### Fixed
- Don't show file and directory actions icons during creating, renaming or
  deleting files or directories
- Showing bookmarks button after creating, renaming or deleting files or
  directories
- Don't show toolbars on crash info screen
- Crash after starting the program via binary
- Hang on error info screen
- Show last modification time in local time
- Showing preview when start the program with empty directory
- Preview on entering empty directory
- Showing files sizes

## [0.6] - 2019-07-12

### Added
- Multilingual support
- Polish translation of the program
- Option to show/hide preview of files and directories
- Context menu with actions on right click on directory listing

### Changed
- Updated interface
- Updated code documentation
- Updated README.md

### Fixed
- Typo in info about unknown error during deleting files and directories
- Typo in README.md
- Double programs names on applications list
- Keyboard shortcut for preview file or directory
- Info about preview or info of file or directory
- Keyboard shortcut for stopping moving or copying files or directories
- Hide destination list on cancel creating links
- Show preview after copying/moving many files or directories
- Crash on showing preview of files or directories after copying or moving

## [0.5] - 2019-06-23

### Added
- Option to go to selected directory
- Option to create links to files and directories
- Option to show or hide hidden files and directories
- Option to show or hide information about files and directories last
  modification time
- Info about last modification time to files and directories listing
- Option to enable or disable scaling images in preview
- Option to set time amount after which messages are auto closes

### Changed
- Updated RoboDOC configuration file
- Updated code documentation
- Updated interface
- Updated README.md
- Program icon

### Fixed
- Typo in code documentation
- Detecting files and directories name with space inside
- Memory leaks when previewing images
- Shortcut for one of buttons in current directory path
- Showing path buttons for long paths
- Showing modification time for invalid links
- Showing owner group name
- Running the program after extracting AppImage

## [0.4] - 2019-06-02

### Added
- Keyboard shortcuts for path buttons
- Preview for images
- Option to open selected file or directory with custom command
- Option to set associated application with selected file
- Info about file or directory permissions
- Option to set file or directory permissions
- Info about the program
- Option to add or remove bookmarks to the directories

### Changed
- Updated interface
- Updated README.md
- Updated code documentation

### Fixed
- Double reload on press path buttons
- Crash on moving invalid symbolic links
- Crash on trying renaming invalid symbolic links
- Crash on creating new file or directory when invalid symbolic link exists
- Copying whole directories
- Updated file or directory preview when file or directory was updated
  (copied, moved, etc.)
- Info about interrupted deleting files or directories
- Typos in Changelog
- Deleting directories
- Refresh preview of directory after deleting last file or directory inside
- Overwritting directory content during copying

## [0.3] - 2019-05-15

### Added
- Option to go to XDG user directories (like Download, Documents, etc.)
- Option to go to GTK bookmark locations
- Code documentation
- Open all text files in application used to open plain text files if there
  no set application to open them
- Confirmation on overwrite files and directories during copying and moving

### Changed
- Updated interface
- Updated README.md

### Fixed
- Reset directory listing on hide search entry
- Copy permissions on copying files or directories
- Deleting directories with invalid symbolic links
- Creating empty files
- Crash on moving files or directories to new location
- Showing close button in box on questions
- Update UI after deleting current directory

## [0.2] - 2019-04-26

### Added
- Command line argument to start the program with showing selected directory
- Better searching for files/directories in viewed directory
- Option to create new files/directories
- Option to delete files/directories
- Option to rename files/directories
- Option to move files/directories
- Option to copy files/directories

### Changed
- Updated README.md
- Use file command to determine files mime types instead of xdg-mime
- Show current path as buttons

### Fixed
- Detecting current icons for the program in AppImage
- Link in error dialog
- Block go up button when reach root directory

## [0.1] - 2019-04-12
- Initial release
