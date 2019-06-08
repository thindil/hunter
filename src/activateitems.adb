-- Copyright (c) 2019 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;
with LoadData; use LoadData;
with MainWindow; use MainWindow;
with Messages; use Messages;
with Utils; use Utils;

package body ActivateItems is

   procedure ActivateFile(User_Data: access GObject_Record'Class) is
      DestinationIter: Gtk_Tree_Iter;
      DestinationModel: Gtk_Tree_Model;
   begin
      if User_Data = Get_Object(Builder, "treedestination") then
         Get_Selected
           (Gtk.Tree_View.Get_Selection(Gtk_Tree_View(User_Data)),
            DestinationModel, DestinationIter);
         CurrentSelected :=
           CurrentDirectory &
           To_Unbounded_String
             ("/" & Get_String(DestinationModel, DestinationIter, 0));
         DestinationPath := CurrentSelected;
      end if;
      if Is_Directory(To_String(CurrentSelected)) then
         if not Is_Read_Accessible_File(To_String(CurrentSelected)) then
            ShowMessage("You can't enter this directory.");
            return;
         end if;
         if CurrentDirectory = To_Unbounded_String("/") then
            CurrentDirectory := Null_Unbounded_String;
         end if;
         CurrentDirectory := CurrentSelected;
         if User_Data = Get_Object(Builder, "treefiles") then
            LoadDirectory(To_String(CurrentDirectory), "fileslist");
         else
            LoadDirectory(To_String(CurrentDirectory), "fileslist2");
         end if;
         Set_Cursor
           (Gtk_Tree_View(User_Data), Gtk_Tree_Path_New_From_String("0"), null,
            False);
         Grab_Focus(Gtk_Widget(User_Data));
      else
         if User_Data = Get_Object(Builder, "treedestination") then
            return;
         end if;
         declare
            MimeType: constant String :=
              GetMimeType(To_String(CurrentSelected));
            Pid: GNAT.OS_Lib.Process_Id;
            Openable: Boolean := CanBeOpened(MimeType);
            Arguments: constant Argument_List :=
              (new String'(To_String(CurrentSelected)), new String'(""));
         begin
            if MimeType(1 .. 4) = "text" and not Openable then
               Openable := CanBeOpened("text/plain");
            end if;
            if not Openable then
               ShowMessage
                 ("I can't open this file. No application associated with this type of files.");
               return;
            else
               Pid :=
                 Non_Blocking_Spawn
                   (Containing_Directory(Command_Name) & "/xdg-open",
                    Arguments(Arguments'First .. Arguments'Last - 1));
            end if;
            if Pid = GNAT.OS_Lib.Invalid_Pid then
               ShowMessage
                 ("I can't open this file. Can't start application asociated with this type of files.");
            end if;
         end;
      end if;
   end ActivateFile;

   procedure StartOpenWith(Object: access Gtkada_Builder_Record'Class) is
      GEntry: constant Gtk_Widget := Gtk_Widget(Get_Object(Object, "entry"));
   begin
      NewAction := OPENWITH;
      Set_Icon_Tooltip_Text
        (Gtk_GEntry(GEntry), Gtk_Entry_Icon_Secondary,
         "Enter command to use to open selected item.");
      Set_Text(Gtk_GEntry(GEntry), "");
      Show_All(GEntry);
      Grab_Focus(GEntry);
   end StartOpenWith;

   procedure OpenItemWith
     (Self: access Gtk_Entry_Record'Class;
      Icon_Pos: Gtk_Entry_Icon_Position) is
      Command: GNAT.OS_Lib.String_Access;
      Arguments: Argument_List(1 .. 3);
      Pid: GNAT.OS_Lib.Process_Id;
      CommandName, CommandArguments: Unbounded_String;
      EnteredCommand: constant String := Get_Text(Self);
   begin
      if Icon_Pos = Gtk_Entry_Icon_Primary then
         Set_Text(Self, "");
         Hide(Gtk_Widget(Self));
         return;
      end if;
      if Get_Text(Self) = "" then
         return;
      end if;
      if Index(Get_Text(Self), " ") > 0 then
         CommandName :=
           To_Unbounded_String
             (EnteredCommand(1 .. Index(EnteredCommand, " ") - 1));
         CommandArguments :=
           To_Unbounded_String
             (EnteredCommand
                (Index(EnteredCommand, " ") + 1 .. EnteredCommand'Length));
      else
         CommandName := To_Unbounded_String(EnteredCommand);
         CommandArguments := Null_Unbounded_String;
      end if;
      Command := Locate_Exec_On_Path(To_String(CommandName));
      if Command = null then
         ShowMessage("Command " & To_String(CommandName) & " does not exist.");
         Set_Text(Self, "");
         Hide(Gtk_Widget(Self));
         return;
      end if;
      Arguments :=
        (Command, new String'(To_String(CommandArguments)),
         new String'(To_String(CurrentSelected)));
      if CommandArguments /= Null_Unbounded_String then
         Pid :=
           Non_Blocking_Spawn
             (Program_Name => Arguments(Arguments'First).all,
              Args => Arguments(Arguments'First + 1 .. Arguments'Last));
      else
         Pid :=
           Non_Blocking_Spawn
             (Program_Name => Arguments(Arguments'First).all,
              Args => Arguments(Arguments'First + 2 .. Arguments'Last));
      end if;
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         ShowMessage("Can't start command: " & Get_Text(Self));
      end if;
      Set_Text(Self, "");
      Hide(Gtk_Widget(Self));
      Free(Command);
   end OpenItemWith;

   procedure ExecuteFile(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      Pid: GNAT.OS_Lib.Process_Id;
   begin
      Pid :=
        Non_Blocking_Spawn
          (Full_Name(To_String(CurrentSelected)),
           Argument_String_To_List("").all);
      if Pid = GNAT.OS_Lib.Invalid_Pid then
         ShowMessage("I can't execute this file.");
      end if;
   end ExecuteFile;

end ActivateItems;
