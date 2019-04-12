Hunter is (or better, will be) a graphical file manager for Linux, heavy
inspired by [Ranger](https://ranger.github.io/). At this moment, Hunter is
under heavy development and have only basic features of file viewer than
manager.

**Note:** This version of README.md is about development version of the
program. Some things may be different in released version of Hunter.

## Build game from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2018 to compile the program.
  The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

* GtkAda library which should be available in most Linux distributions. Best
  option is to use (with GNAT GPL) AdaCore version of GtkAda from:

  https://www.adacore.com/download/more

  At this moment tested version of GtkAda is 2018 and the program require GTK
  library in version 3.14 (may not works with other versions).

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile the program is use Gnat Programming Studio included
  in GNAT. Just run GPS, select *hunter.gpr* as a project file and select
  option `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`

## Running Steam Sky

### Linux
If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More informations about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

When you trying to run build by yourself version of the program, use script
`hunter.sh`. The program will not works if you try to start it by binary file
`hunter` from `bin` directory.

## Licenses
Hunter is available under [GPLv3](COPYING) license.

xdg-mime and xdg-open scripts are part of the xdg-utils project and released
under MIT license:

https://github.com/freedesktop/xdg-utils

----

That's all for now, and again, probably I forgot about something important ;)

Bartek thindil Jasicki
