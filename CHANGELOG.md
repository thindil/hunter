# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Refresh preview of files and directories if they are changed on disk

### Changed
- Updated README.md
- Updated contributing guide
- Autorefresh directory listing on any change now should be a lot faster
- Simplified autorefresh directory listing setting

### Fixed
- Typos in contributing guide
- Searching for files or directories during copying or moving

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
