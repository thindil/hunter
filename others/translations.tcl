#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

# Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

if {[file exists hunter.gpr] == 0} {
   puts {This script must be run in the directory where hunter.gpr file is}
   return
}

proc ShowHelp {} {
   puts {Possible options:
   - generate      - (Re)Generate ROOT translation of the program
   - create [name] - Create new empty translation for the selected language
   - update        - Updated existing translations from the ROOT translation
   }
}

# Show help
if {$argc == 0} {
   ShowHelp
   return
}

# Set directory where translations are
set directory "share/hunter/translations"

# Generate ROOT translation from the source files
if {[lindex $argv 0] == "generate"} {
   puts -nonewline {(Re)Generating the ROOT translation ...}
   set rootmsg [open $directory/ROOT.msg w]
   set translist {}
   puts $rootmsg "::msgcat::mcflmset {"
   foreach filename [glob -directory src *.ad*] {
      set adafile [open $filename r]
      set content [read $adafile]
      close $adafile
      set translations [regexp -all -inline {Mc+\W*\(\W*\w+,\W* "[^"]+} $content]
      foreach translation $translations {
         regsub {^.+"+\{*} $translation "" translation
         regsub {\}$} $translation "" translation
         if {[lsearch -exact $translist $translation] == -1} {
            puts $rootmsg "   \"$translation\" \"$translation\""
            lappend translist $translation
         }
      }
   }
   puts $rootmsg "}"
   close $rootmsg
   puts { done.}
# Creating a new translation for the selected language
} elseif {[lindex $argv 0] == "create"} {
   set lang [lindex $argv 1]
   if {[file exists $directory/$lang.msg] == 1} {
      puts "Can't create a new translation for language $lang because there exists one."
      return
   }
   if {[file exists $directory/ROOT.msg] == 0} {
      puts "ROOT translation doesn't exists. Please create it first."
      return
   }
   puts -nonewline "Creating a new translation for language $lang ..."
   file copy $directory/ROOT.msg $directory/$lang.msg
   puts { done.}
# Updating all existing translations from the ROOT translation
} elseif {[lindex $argv 0] == "update"} {
   puts -nonewline {Updating all existing translations ...}
   set translist {}
   set rootfile [open $directory/ROOT.msg r]
   while {[gets $rootfile line] >= 0} {
      set translation [string range [regexp -inline {"[^"]+"} $line] 1 end-1]
      if {$translation != ""} {
         lappend translist $translation
      }
   }
   close $rootfile
   foreach filename [glob -directory $directory *.msg] {
      if {[file tail $filename] == "ROOT.msg"} {
         continue
      }
      file rename $filename $filename.bak
      set newmsg [open $filename w]
      puts $newmsg "::msgcat::mcflmset {"
      set oldmsg [open $filename.bak r]
      while {[gets $oldmsg line] >= 0} {
         set translation [string range [regexp -inline {"[^"]+"} $line] 1 end-1]
         if {[lsearch -exact $translist $translation] > -1} {
            puts $newmsg $line
            set translist [lsearch -inline -all -not -exact $translist $translation]
         }
      }
      close $oldmsg
      if {[llength $translist] > 0} {
         foreach translation $translist {
            if {$translation != "\" \""} {
               puts $newmsg "   $translation $translation"
            }
         }
      }
      puts $newmsg "}"
      close $newmsg
      file delete $filename.bak
   }
   puts { done.}
} else {
   ShowHelp
}
