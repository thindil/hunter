-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Tcl; use Tcl;
with CopyItems; use CopyItems;

-- ****h* Modules/Modules
-- FUNCTION
-- Provide code for manipulating the program's modules
-- SOURCE
package Modules is
-- ****

   -- ****v* Modules/Modules.Enabled_Modules
   -- FUNCTION
   -- Store paths to the enabled the program's modules
   -- SOURCE
   Enabled_Modules: UnboundedString_Container.Vector;
   -- ****

   -- ****t* Modules/Modules.Triggers
   -- FUNCTION
   -- The program states on which the selected the program's module code will
   -- be triggered
   -- OPTIONS
   -- ON_QUIT     - The program quits normally, triggered after the config is
   --               saved but before clear the Trash and stop inotify
   -- ON_ACTIVATE - On activating by the user the selected directory or file
   --               on the directory list
   -- ON_ENTER    - On entering the selected directory: during the start the
   --               program, when the user activate it on the directory list,
   --               when the user uses path buttons or when the user uses
   --               bookmarks
   -- SOURCE
   type Triggers is (ON_QUIT, ON_ACTIVATE, ON_ENTER);
   -- ****

   -- ****d* Modules/Modules.On_Enter_Trigger
   -- FUNCTION
   -- Default trigger for modules, on enter the directory
   -- SOURCE
   On_Enter_Trigger: constant Triggers := ON_ENTER;
   -- ****

   -- ****f* Modules/Modules.Load_Modules
   -- FUNCTION
   -- Load all enabled modules at start the program
   -- PARAMETERS
   -- Interpreter - Tcl_Interpreter on which the modules will be loaded
   -- SOURCE
   procedure Load_Modules(Interpreter: Tcl_Interp);
   -- ****

   -- ****f* Modules/Modules.Execute_Modules
   -- FUNCTION
   -- Execute the program modules code based on the selected trigger
   -- PARAMETERS
   -- Interpreter - Tcl_Interpreter on which the modules will be executed
   -- State       - Trigger state on which the module code will be executed
   -- Arguments   - Arguments passed to the Tcl code. Can be empty. Default
   --               value is empty
   -- SOURCE
   procedure Execute_Modules
     (Interpreter: Tcl_Interp; State: Triggers; Arguments: String := "");
   -- ****

end Modules;
