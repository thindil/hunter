namespace eval bob {

   proc Create_UI {} {
      image create photo BobIcon -file bob.svg -format "svg -scaletoheight [GetConfig toolbarssize]"
      ttk::menubutton .actiontoolbar.bobbutton -style Toolbutton -image BobIcon -takefocus 0
      pack .actiontoolbar.bobbutton -after .actiontoolbar.deletebutton
   }

   proc on_enable {} {
      bob::Create_UI
   }

   proc on_disable {} {
      puts {I'm disabled :(}
   }

   proc on_start {} {
      bob::Create_UI
   }

   proc on_enter {path} {
      puts "Entered directory '$path'"
   }
}
