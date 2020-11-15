## General information
The program can be modified by using Tcl scripts organized in modules:
directories containing proper files in proper locations on disk. Theoretically,
it is possible to extend the program with any programming language, just the
entry point must be done in Tcl language. To see how module can look, please
look at the module included in the program release. Please note, the whole
modding system is very fresh and can change before the stable release.

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
File module.cfg  is a standard configuration file. It is organized in
`[key]=[value]` pairs. Each value is one line only. The file must contain at
least three settings:
* Name - The name of the module. For example: `Name=My Module`.
* Version - The version of the module. For example: `Version=2.0 beta`.
* Description - The short description of the module. It will be presented to
  the user, thus it will be good if it will be informative. Example:
  `Description=Small module to present the program modification abilities`.
