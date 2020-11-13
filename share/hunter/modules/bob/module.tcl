namespace eval bob {

   proc Create_UI {path} {
      image create photo BobIcon -file [file join $path bob.svg] -format "svg -scaletoheight [GetConfig toolbarssize]"
      menu .bobmenu -tearoff false
      ttk::menubutton .mainframe.toolbars.actiontoolbar.bobbutton -style Toolbutton -image BobIcon -takefocus 0 -menu .bobmenu
      tooltip::tooltip .mainframe.toolbars.actiontoolbar.bobbutton {Show Bob commands menu}
   }

   proc on_enable {path} {
      bob::Create_UI $path
   }

   proc on_disable {} {
      destroy .mainframe.toolbars.actiontoolbar.bobbutton
   }

   proc on_start {path} {
      bob::Create_UI $path
   }

   proc on_enter {path} {
      set filename [file join $path .bob.yml]
      if {[file exists $filename] == 0} {
         pack forget .mainframe.toolbars.actiontoolbar.bobbutton
         return
      } else {
         pack .mainframe.toolbars.actiontoolbar.bobbutton -after .mainframe.toolbars.actiontoolbar.deletebutton
      }
      .bobmenu delete 0 end
      set bobfile [open $filename]
      while {[gets $bobfile line] >= 0} {
         if {[string first {name:} $line] == -1} {
            continue
         }
         .bobmenu add command -label [string range $line 9 end]
      }
      close $bobfile
   }
}
