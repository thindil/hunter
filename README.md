Hunter is a graphical file manager for Linux, inspired by [Ranger](https://ranger.github.io/).
At this moment, Hunter is under development and have all basic features
of a file manager. To open files, the program uses default applications
from [XDG MIME Application Specification](https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html).

**Note:** This version of README.md is about development version of the
program. Some things may be different in released version of Hunter.

This is the new version based on Tk library. For the stable version of the
program please look [here](https://github.com/thindil/hunter/tree/master)

## Features

* Two column mode default or one column mode: One column always show files
  and directories, second column is used to preview files and directories
  or select destination location for move or copy files. In one column
  mode, second column appear only when copying or moving files or directories.
* Preview of text files (with syntax highlighting) and images.
* Standard file manager features: copy, delete, move files and directories,
  change their permissions, change application associated with them.
* Written in TK and Ada.
* Available in English and Polish languages.

## Build the program from sources

### Docker way

You can use Docker image `adabuild` from the project [dockerada](https://github.com/thindil/dockerada).
It contains all libraries and compiler needed to build the program.

To build the program, download `adabuild` image and type in console:

`docker run --rm -v [path to source code]:/app ghcr.io/thindil/adabuild /bin/bash -c "cd /app && gprbuild -p -P hunter.gpr -XMode=release"`

### Classic way

To build you need:

* compiler - GCC with enabled Ada support. The most distributions should have
  it. The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

* Tcl/Tk library. Should be available in any Linux distribution.

* XmlAda - Should be available in the most Linux distributions. In other
  situation, you may need to download it from:

  https://github.com/AdaCore/xmlada

* TASHY library with included binding to Tk and TkLib. You can get it from:

   https://github.com/thindil/tashy

   **Important:** To build this version of Hunter the version 8.6.9 is
   required. Ealier versions will not work due to lack of some bindings.

* File command (libmagic) development files. It can have different names in
  every Linux distribution: in Fedora it is *file-devel* on Debian/Ubuntu/Mint
  *file-dev*.

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* The simplest way to compile the program is use Gnat Studio. It should be available
  in the most distributions. Just run it, select *hunter.gpr* as a project file
  and select option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P hunter.gpr` for debug mode build or for release mode:
  `gprbuild -P hunter.gpr -XMode=release`. If you have installed
  [Bob](https://github.com/thindil/bob) you can type `bob debug` for build in
  debug mode or `bob release` to prepare release for the program.

## Running Hunter

If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More information about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

**IMPORTANT:** The current AppImage version require GNU LibC in version at
least 2.29. It will not work with earlier versions. In that situation,
unfortunately you will have to build the program by yourself.

When you are trying to run build by yourself version of the program, run
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

Here are available testing versions of the game. You can find them
in [Actions](https://github.com/thindil/hunter/actions?query=workflow%3A"Continuous+Integration").
Just select option from the list of results to see Artifacts list.
To use them, first you must download normal release. Then, for Linux: inside
directory where the game is, type `./hunter-x86_64.AppImage --appimage-extract`
to extract whole game to directory *squashfs-root*. And then move files
from the archive to the proper location. To run that version, enter
*squashfs-root* directory and type in console `./AppRun`.

Size is a file's size after unpacking. You will download it compressed with
Zip.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
If you have it, in main program directory (where this file is) enter terminal
command: `others/generatedocs.py`. For more information about this script,
please look [here](https://github.com/thindil/roboada#generatedocspy). This
version of script have set all default settings for Hunter code. If you have
[Bob](https://github.com/thindil/bob) installed, you can type `bob docs`.

## Contributing to the project
For detailed information about contributing to the project (bugs reporting,
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
