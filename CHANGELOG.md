# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Keyboard shortcuts for path buttons
- Preview for images
- Option to open selected file or directory with custom command

### Changed
- Updated interface

### Fixed
- Double reload on press path buttons
- Crash on moving invalid symbolic links
- Crash on trying renaming invalid symbolic links
- Crash on creating new file or directory when invalid symbolic link exists
- Copying whole directories

## [0.3] - 2019-05-15

### Added
- Option to go to XDG user directories (like Download, Documents, etc)
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
