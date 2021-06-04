#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {![file exists hunter.gpr]} {
   puts {This script must be run in the directory where hunter.gpr file is}
   return
}

set rootdir [pwd]
set logfile "[file join $rootdir adacontrol.log]"

exec gprclean -P hunter.gpr >@stdout
file delete $logfile
cd [file join obj]
if {$argc == 0} {
   set adaoptions "-r hunter-tcl-cargv-chelper-unicode-sax-dom-input_sources"
} else {
   set adaoptions "[file join $rootdir src [lindex $argv 0]]"
}
if {[catch {exec adactl -f [file join $rootdir others rules.aru] -p [file join $rootdir hunter.gpr] -o $logfile -w $adaoptions} results options]} {
   if {[file size $logfile] > 1} {
      return -options $options -level 0 $results
   } else {
      file delete $logfile
   }
}
