#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists hunter.gpr] == 0} {
   puts {This script must be run in the directory where hunter.gpr file is}
   return
}

exec sudo docker run --rm -v [pwd]:/app hunter/build:latest bin/bash -c "cd /app && others/build.tcl"
