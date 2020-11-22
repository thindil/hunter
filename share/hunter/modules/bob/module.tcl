# Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# ****h* bob
# FUNCTION
# Each module must be in it own namespace. The namespace's name must be that
# same as the name of directory in which the modules is. It is also case
# sensitive
# SOURCE
namespace eval bob {
# ****

   # ****f* bob/bob.Create_UI
   # FUNCTION
   # Create the module related UI (button)
   # PARAMETERS
   # path - Path to the directory where the module is located
   # SOURCE
   proc Create_UI {path} {
   # ****
      image create photo BobIcon -file [file join $path bob.svg] -format "svg -scaletoheight [GetConfig toolbarssize]"
      menu .bobmenu -tearoff false
      ttk::menubutton .mainframe.toolbars.actiontoolbar.bobbutton -style Toolbutton -image BobIcon -takefocus 0 -menu .bobmenu
      tooltip::tooltip .mainframe.toolbars.actiontoolbar.bobbutton {Show Bob commands menu}
   }

   # ****m* bob/bob.on_enable
   # FUNCTION
   # Code executed when the module was enabled by the user. If module doesn't
   # have any code to execute during enabling, you can omit this procedure.
   # But it is always good to have one even empty.
   # This one creates the module UI.
   # PARAMETERS
   # path - Path to the directory where the module is located
   # SOURCE
   proc on_enable {path} {
   # ****
      bob::Create_UI $path
   }

   # ****m* bob/bob.on_disable
   # FUNCTION
   # Code executed when the module was disabled by the user. If module doesn't
   # have any code to execute during disabling, you can omit this procedure.
   # But it is always good to have one even empty.
   # This one removes the module UI
   # SOURCE
   proc on_disable {} {
   # ****
      image delete BobIcon
      destroy .mainframe.toolbars.actiontoolbar.bobbutton
      destroy .bobmenu
   }

   # ****m* bob/bob.on_start
   # FUNCTION
   # Code executed when the program starts and the module is enabled. If module
   # doesn't have any code to execute during the program start, you can omit
   # this procedure. But it is always good to have one even empty.
   # This one created the module UI.
   # PARAMETERS
   # path - Path to the directory where the module is located
   # SOURCE
   proc on_start {path} {
   # ****
      bob::Create_UI $path
   }

   # ****m* bob/bob.on_enter
   # FUNCTION
   # Code executed when the user enters any directory, also automatically,
   # after start the program on show the first directory. If module doesn't
   # have any code to execute during disabling, you can omit this procedure.
   # But it is always good to have one even empty.
   # This one looking for Bob configuration file in the selected directory.
   # If find, creates the module button menu with available options.
   # PARAMETERS
   # path - Path to the directory to which the user entered
   # SOURCE
   proc on_enter {path} {
   # ****
      set filename [file join $path .bob.yml]
      if {[file exists $filename] == 0} {
         pack forget .mainframe.toolbars.actiontoolbar.bobbutton
         return
      } else {
         pack .mainframe.toolbars.actiontoolbar.bobbutton -after .mainframe.toolbars.actiontoolbar.deletebutton
         if {[GetConfig toolbarsontop]} {
            pack configure .mainframe.toolbars.actiontoolbar.bobbutton -side left
         } else {
            pack configure .mainframe.toolbars.actiontoolbar.bobbutton -side top
         }
      }
      .bobmenu delete 0 end
      set bobfile [open $filename]
      while {[gets $bobfile line] >= 0} {
         if {[string first {name:} $line] == 3} {
            set cmdname [string range $line 9 end]
            .bobmenu add command -label $cmdname -command "ExecuteModuleCommand {bob $cmdname} {$path}"
         }
      }
      close $bobfile
   }

   # ****m* bob/bob.on_quit
   # FUNCTION
   # Code executed when the programs ends (quits). If the module doesn't have
   # any code to execute during the program ending, you can omit this
   # procedure. But it is always good to have one even empty.
   # This one does nothing
   # SOURCE
   proc on_quit {} {
   # ****
      return
   }

   # ****m* bob/bob.on_activate
   # FUNCTION
   # Code executed when the user activates file or directory. If the module
   # doesn't have any code to execute during the activation of an item, you
   # can omit this procedure. But it is always good to have one even empty.
   # This one does nothing
   # PARAMETERS
   # path - Path to the item (file or directory) which was activated
   # SOURCE
   proc on_activate {path} {
   # ****
      return
   }
}
