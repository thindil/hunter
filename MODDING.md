## General information
The program can be modified by using Tcl scripts organized in modules:
directories containing proper files in proper locations on disk. Theoretically,
it is possible to extend the program with any programming language, just the
entry point must be done in Tcl language. To see how module can look, please
look at the module included in the program release. Please note, the whole
modding system is very fresh and can change before the stable release or with
time.

## Modules locations
The program search for the modules in the following directories:
* *../share/hunter/modules* - path relative to the place where file `hunter` is.
  There are located the global modules (for example, distributed with the
  program)
* *$HOME/.local/share/hunter/modules* - absolute path. There are located the
  modules available for the selected user.

## Modules content
Each module must have two files. All other files are optional and depends on
the content of the selected module. Needed files are:
* *module.cfg* - the configuration file of the module.
* *module.tcl* - the entry point of the module. This Tcl script will be loaded
  when the selected module will be enabled.

## Module configuration
File *module.cfg*  is a standard configuration file. It is organized in
`[key]=[value]` pairs. Each value is one line only. The file must contain at
least three settings:
* Name - The name of the module. For example: `Name=My Module`.
* Version - The version of the module. For example: `Version=2.0 beta`.
* Description - The short description of the module. It will be presented to
  the user, thus it will be good if it will be informative. Example:
  `Description=Small module to present the program modification abilities`.

## Module.tcl
File *module.tcl* contains the Tcl code of the module. Tcl context (`namespace`)
name must be exactly that same as directory name of the module. For example,
if module is in `my_module` directory, context must be `my_module` also
(case-sensitive too). If you want to run any Tcl code on the selected trigger
event, you have to create a proper procedure inside the file. Available
options are:
* `on_enable {path}` - triggered when the module was enabled by the user.
  Parameter *path* is the path where the module is located.
* `on_disable {}` - triggered when the module was disabled by the user.
* `on_start {path}` - triggered when the program starts, after creating all UI.
  Parameter *path* is the path where the module is located.
* `on_enter {path}` - triggered when the user enters directory, after loading
  the data and updating UI. Parameter *path* is the path to the directory to
  which the program entered.
* `on_quit {}` - triggered when the user closes the program, after saving the
  program configuration.
* `on_activate {path}` - triggered when the user activate file or enter the
  selected directory, after loading the data and updating UI. Parameter *path*
  is the path of the item which was activated.
You don't need to add all procedures to the module, but it may be a bit faster
if they are all there. In the Tcl code you can use any command which is
declared in the program code. For more information about which commands are
available, please look and the program code documentation.
