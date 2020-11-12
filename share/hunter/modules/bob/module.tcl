namespace eval bob {

   proc Create_UI {path} {
      image create photo BobIcon -file [file join $path bob.svg] -format "svg -scaletoheight [GetConfig toolbarssize]"
      ttk::menubutton .mainframe.toolbars.actiontoolbar.bobbutton -style Toolbutton -image BobIcon -takefocus 0
      pack .mainframe.toolbars.actiontoolbar.bobbutton -after .mainframe.toolbars.actiontoolbar.deletebutton
   }

   proc on_enable {path} {
      bob::Create_UI $path
   }

   proc on_disable {} {
      puts {I'm disabled :(}
   }

   proc on_start {path} {
      bob::Create_UI $path
   }

   proc on_enter {path} {
      puts "Entered directory '$path'"
   }
}
