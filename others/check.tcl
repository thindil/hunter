#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {![file exists hunter.gpr]} {
   puts {This script must be run in the directory where hunter.gpr file is}
   return
}

set rootdir [pwd]

exec gprclean -P hunter.gpr >@stdout
file delete [file join $rootdir adacontrol.log]
cd [file join obj]
if {$argc == 0} {
   set adaoptions "-r hunter-tcl-cargv-chelper-unicode-sax-dom-input_sources"
} else {
   set adaoptions "[file join $rootdir src [lindex $argv 0]]"
}
if {[catch {exec adactl -f [file join $rootdir others rules.aru] -p [file join $rootdir hunter.gpr] -o [file join $rootdir adacontrol.log] -w $adaoptions} results options]} {
   if {[file size [file join $rootdir adacontrol.log]] > 1} {
      return -options $options -level 0 $results
   } else {
      file delete [file join $rootdir adacontrol.log]
   }
}
