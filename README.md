Hunter is (or better, will be) a graphical file manager for Linux, heavy
inspired by [Ranger](https://ranger.github.io/). At this moment, Hunter is
under heavy development and have only basic features of file viewer than
manager. To open files, the program uses default applications from [XDG MIME
Application Specification](https://specifications.freedesktop.org/mime-apps-spec/mime-apps-spec-latest.html).

**Note:** This version of README.md is about development version of the
program. Some things may be different in released version of Hunter.

## Build the program from sources

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

## Running Hunter

If you use downloaded AppImage version, you don't need any additional
libraries. Just run it as any AppImage file. More informations about AppImage
files usage, you can find at:

https://docs.appimage.org/user-guide/run-appimages.html

When you trying to run build by yourself version of the program, use script
`hunter.sh`. The program will not works if you try to start it by binary file
`hunter` from `bin` directory.

### Starting parameter

You can set directory to view when starting the program by adding it full path
to the starting commmand. For example, to view root directory `/` run the
program with `hunter.sh /`

### Wrong theme used by the program

Hunter uses GTK library in version 3.14. Your selected theme probably don't
support this version of GTK or don't have compiled support for it. In this
second situation, please check documentation of selected theme to find how to
recompile it. If your theme completely don't support GTK in version 3.14 you
can still use default GTK theme by adding environment variable `GTK_THEME`.
For example: `GTK_THEME=Adwaita ./hunter-x86-64.AppImage`.

## Licenses
Hunter is available under [GPLv3](COPYING) license.

xdg-mime and xdg-open scripts are part of the xdg-utils project and released
under MIT license:

https://github.com/freedesktop/xdg-utils

GtkAda library distributed with the program are under GPLv3 license.

https://github.com/AdaCore/gtkada

Gtk library distributed with the program is under LGPLv2.1 license:

https://www.gtk.org/

----

That's all for now, and again, probably I forgot about something important ;)

Bartek thindil Jasicki
