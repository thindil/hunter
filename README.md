Hunter is a graphical file manager for Linux, inspired by [Ranger](https://ranger.github.io/).
At this moment, Hunter is under development and have all basic features
of a file manager. To open files, the program uses default applications
from [XDG MIME Application Specification](https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html).

**Note:** This version of README.md is about development version of the
program. Some things may be different in released version of Hunter.

This is the new version based on Tk library. For the stable version of the
program please look [here](https://github.com/thindil/hunter/tree/master)

## Features

- Two column mode default or one column mode: One column always show files
  and directories, second column is used to preview files and directories
  or select destination location for move or copy files. In one column
  mode, second column appear only when copying or moving files or directories.
- Preview of text files (with syntax highlighting) and images.
- Standard file manager features: copy, delete, move files and directories,
  change their permissions, change application associated with them.
- Written in TK and Ada.
- Available in English and Polish languages.

## Build the program from sources

To build you need:

* compiler - GCC with enabled Ada support. The most distributions should have
  it. The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

* Tcl/Tk library. Should be available in any Linux distribution.

* TASHY library with included binding to Tk and TkLib. You can get it from:

   https://github.com/thindil/tashy

   **Important:** To build this version of Hunter the best version is 8.6.8.
   Ealier versions will not works due to lack of some bindings and newer
   versions may bring some incompatibility changes.

* File command (libmagic) development files. It can have different names in
  every Linux distribution: in Fedora it is *file-devel* on Debian/Ubuntu/Mint
  *file-dev*.

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile the program is use Gnat Studio. It should be available
  in the most distributions. Just run it, select *hunter.gpr* as a project file
  and select option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P hunter.gpr` for debug mode build or for release mode:
  `gprbuild -P hunter.gpr -XMode=release`. If you have installed
  [Bob](https://github.com/thindil/bob) you can type `bob debug` for build in
  debug mode or `bob release` to prepare release for the program.

## Running Hunter

If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More informations about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

**IMPORTANT:** The current AppImage version require GNU LibC in version at
least 2.29. It will not works with earlier versions. In that situation,
unfortunately you will have to build the program by yourself.

When you trying to run build by yourself version of the program, run
`hunter` from `bin` directory. Additionally, the program requires a few
more libraries:

* Tk extension *tklib*. Should be available in every Linux distribution.

* Tk extension *Img*. In Debian/Ubuntu/Mint it is named *libtk-img*.

* Tk extension *tksvg*. You can get it from:

   https://github.com/auriocus/tksvg


### Starting parameter

You can set directory to view when starting the program by adding it full path
to the starting command. For example, to view root directory `/` run the
program with `hunter /`

### Testing versions

Here are available also testing versions of the program. You can find them
in [Releases](https://github.com/thindil/hunter/releases/tag/travis-dev-build).
To use them, first you must download normal release. Then, inside directory
where the program is, type `./hunter-x86_64.AppImage --appimage-extract` to
extract whole program to directory *squashfs-root*. And then just move files
from the archive to proper locations. To run that version, just enter
*squashfs-root* directory and type in console `./AppRun`.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
If you have it, in main program directory (where this file is) enter terminal
command: `others/generatedocs.py`. For more information about this script,
please look [here](https://github.com/thindil/roboada#generatedocspy). This
version of script have set all default settings for Hunter code. If you have
[Bob](https://github.com/thindil/bob) installed, you can type `bob docs`.

## Contributing to the project
For detailed informations about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses
Hunter is available under [GPLv3](COPYING) license.

xdg-mime and xdg-open scripts distributed with the program are part of the
xdg-utils project and released under MIT license:

https://github.com/freedesktop/xdg-utils

Highlight program distributed with the program is under GPLv3 license:

http://www.andre-simon.de/doku/highlight/en/highlight.php

Tcl/Tk, Tklib libraries are available under BSD-like license:

https://www.tcl.tk/

Tk Img library is available under BSD-like license:

https://sourceforge.net/projects/tkimg/

Tksvg library is available under BSD-like license:

https://github.com/auriocus/tksvg

TASHY is distributed under GPLv2 with runtime exception license:

https://github.com/thindil/tashy

Icons included in the program are modified version of the KDE Breeze Icons
theme which are available under LGPLv2 license:

https://github.com/KDE/breeze-icons

The Hunter default themes are tkBreeze themes released under LGPLv2 license:

https://github.com/thindil/tkBreeze

----

That's all for now, and again, probably I forgot about something important ;)

Bartek thindil Jasicki
