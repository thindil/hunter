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

# Generate ROOT translation from the source files
if {[lindex $argv 0] == "generate"} {
   puts -nonewline {(Re)Generating the ROOT translation ...}
   set rootmsg [open share/hunter/translations/ROOT.msg w]
   set translist [list ]
   puts $rootmsg "::msgcat::mcflmset {"
   foreach filename [glob -directory src *.ad*] {
      set adafile [open $filename r]
      set content [read $adafile]
      set translations [regexp -all -inline {Mc+\W*\(\W*\w+,\W* "[^"]+} $content]
      foreach translation $translations {
         regsub {^.+"+\{*} $translation "" translation
         regsub {\}$} $translation "" translation
         if {[lsearch $translist $translation] == -1} {
            puts $rootmsg "   \"$translation\" \"$translation\""
            lappend translist $translation
         }
      }
      close $adafile
   }
   puts $rootmsg "}"
   close $rootmsg
   puts {done.}
# Creating a new translation for the selected language
} elseif {[lindex $argv 0] == "create"} {
   puts -nonewline "Creating a new translation for language [lindex $argv 1]..."
   puts {done.}
# Updating all existing translations from the ROOT translation
} elseif {[lindex $argv 0] == "update"} {
   puts -nonewline {Updating all existing translations...}
   puts {done.}
} else {
   ShowHelp
}
