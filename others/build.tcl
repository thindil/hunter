#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists hunter.gpr] == 0} {
   puts {This script must be run in the directory where hunter.gpr file is}
   return
}

# directory where the release will be created
set releasedir usr

exec gprclean -P hunter.gpr >@stdout
exec gprbuild -p -P hunter.gpr -XMode=release >@stdout
puts -nonewline {Copying files and directories ... }
file mkdir $releasedir/share/doc/hunter
file mkdir $releasedir/share/metainfo/
file copy bin $releasedir/
file copy CHANGELOG.md $releasedir/share/doc/hunter/
file copy COPYING $releasedir/share/doc/hunter/
file copy README.md $releasedir/share/doc/hunter/
file copy CONTRIBUTING.md $releasedir/share/doc/hunter/
file copy others/hunter.appdata.xml $releasedir/share/metainfo
file copy share/file $releasedir/share
file copy share/highlight $releasedir/share
file copy share/hunter $releasedir/share
puts {done}
exec gprclean -P hunter.gpr >@stdout
