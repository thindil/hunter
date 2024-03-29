-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Tcl;
with Tcl.MsgCat.Ada; use Tcl.MsgCat.Ada;
with Common; use Common;
with LoadData.UI; use LoadData.UI;
with Messages.UI;
with Modules; use Modules;
with RefreshData; use RefreshData;
with ShowItems; use ShowItems;
with Utils.UI; use Utils.UI;

package body Bookmarks.UI is

   procedure Create_Bookmarks_List is
   begin
      Fill_Bookmarks_List;
   end Create_Bookmarks_List;

   function Show_Bookmarks_Menu return Item_Array_Access is
      Menu_Items: Item_Array_Access;
      Menu_Index: Positive := 1;
   begin
      Menu_Items := new Item_Array(1 .. Positive(Bookmarks_List.Length) + 4);
      Menu_Items.all(Menu_Index) :=
        New_Item(Name => Mc(Interp => Interpreter, Src_String => "{Home}"));
      Menu_Index := Menu_Index + 1;
      Add_Bookmarks_Loop :
      for I in Bookmarks_List.Iterate loop
         Menu_Items.all(Menu_Index) :=
           New_Item(Name => Bookmarks_Container.Key(Position => I));
         Menu_Index := Menu_Index + 1;
      end loop Add_Bookmarks_Loop;
      Menu_Items.all(Menu_Index) :=
        New_Item
          (Name =>
             Mc(Interp => Interpreter, Src_String => "{Enter destination}"));
      Menu_Index := Menu_Index + 1;
      Menu_Items.all(Menu_Index) :=
        New_Item(Name => Mc(Interp => Interpreter, Src_String => "Close"));
      Menu_Index := Menu_Index + 1; --## rule line off ASSIGNMENTS
      Menu_Items.all(Menu_Index) := Null_Item;
      return Menu_Items;
   end Show_Bookmarks_Menu;

   -- ****iv* BookmarksTUI/BookmarksTUI.Dialog_Form
   -- FUNCTION
   -- The form to enter the selected directory
   -- SOURCE
   Dialog_Form: Forms.Form;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* BookmarksTUI/BookmarksTUI.Get_Dialog_Form
   -- FUNCTION
   -- Get the execute items with dialog form
   -- RESULT
   -- The ncurses dialog form for enter the selected directory
   -- SOURCE
   function Get_Dialog_Form return Forms.Form is
      -- ****
   begin
      return Dialog_Form;
   end Get_Dialog_Form;
   --## rule on REDUCEABLE_SCOPE

   -- ****if* BookmarksTUI/BookmarksTUI.Set_Dialog_Form
   -- FUNCTION
   -- Set the new value for the dialog form for enter the selected directory
   -- PARAMETERS
   -- New_Form - The new value for the Dialog_Form
   -- SOURCE
   procedure Set_Dialog_Form(New_Form: Forms.Form) is
      -- ****
   begin
      Dialog_Form := New_Form;
   end Set_Dialog_Form;

   -- ****iv* BookmarksTUI/BookmarksTUI.Form_Window
   -- FUNCTION
   -- The window to show the form to enter the selected directory
   -- SOURCE
   Form_Window: Window; --## rule line off DIRECTLY_ACCESSED_GLOBALS
   -- ****

   function Go_To_Bookmark(Bookmark: String) return Ui_Locations is
      procedure Show_Bookmarks_Form is
         Create_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
         Visibility: Cursor_Visibility := Normal;
         Field_Options: Field_Option_Set;
      begin
         Set_Cursor_Visibility(Visibility => Visibility);
         Create_Fields.all(1) :=
           New_Field
             (Height => 1,
              Width =>
                Column_Position'Value
                  (Mc_Max
                     (Strings => "{Enter the destination directory:}",
                      Interp => Interpreter)),
              Top => 0, Left => 8, Off_Screen => 0, More_Buffers => 0);
         Set_Buffer
           (Fld => Create_Fields.all(1), Buffer => 0,
            Str =>
              Mc
                (Interp => Interpreter,
                 Src_String => "{Enter the destination directory:}"));
         Field_Options := Get_Options(Fld => Create_Fields.all(1));
         Field_Options.Active := False; --## rule line off ASSIGNMENTS
         Set_Options(Fld => Create_Fields.all(1), Options => Field_Options);
         Create_Fields.all(2) :=
           New_Field
             (Height => 1, Width => 40, Top => 1, Left => 0, Off_Screen => 0,
              More_Buffers => 0);
         Set_Buffer
           (Fld => Create_Fields.all(2), Buffer => 0,
            Str => To_String(Source => Common.Current_Directory));
         Field_Options := Get_Options(Fld => Create_Fields.all(2));
         Field_Options.Auto_Skip := False; --## rule line off ASSIGNMENTS
         Set_Options(Fld => Create_Fields.all(2), Options => Field_Options);
         Create_Fields.all(3) :=
           New_Field
             (Height => 1,
              Width =>
                Column_Position'Value
                  (Mc_Max(Strings => "{Cancel}", Interp => Interpreter)) +
                2,
              Top => 2, Left => 7, Off_Screen => 0, More_Buffers => 0);
         Set_Buffer
           (Fld => Create_Fields.all(3), Buffer => 0,
            Str =>
              "[" & Mc(Interp => Interpreter, Src_String => "{Cancel}") & "]");
         Field_Options := Get_Options(Fld => Create_Fields.all(3));
         Field_Options.Edit := False; --## rule line off ASSIGNMENTS
         Set_Options(Fld => Create_Fields.all(3), Options => Field_Options);
         Create_Fields.all(4) :=
           New_Field
             (Height => 1, Width => 7, Top => 2, Left => 23, Off_Screen => 0,
              More_Buffers => 0);
         Field_Options := Get_Options(Fld => Create_Fields.all(4));
         Field_Options.Edit := False; --## rule line off ASSIGNMENTS
         Set_Options(Fld => Create_Fields.all(4), Options => Field_Options);
         Set_Buffer
           (Fld => Create_Fields.all(4), Buffer => 0,
            Str =>
              "[" & Mc(Interp => Interpreter, Src_String => "{Enter}") & "]");
         Create_Fields.all(5) := Null_Field;
         Create_Go_To_Dialog_Block :
         declare
            New_Dialog_Form: Forms.Form := New_Form(Fields => Create_Fields);
            Form_Height: Line_Position;
            Form_Length: Column_Position;
         begin
            Set_Current(Frm => New_Dialog_Form, Fld => Create_Fields(2));
            --## rule off DIRECTLY_ACCESSED_GLOBALS
            Create_Dialog
              (DialogForm => New_Dialog_Form, FormWindow => Form_Window,
               Form_Height => Form_Height, Form_Length => Form_Length);
            --## rule on DIRECTLY_ACCESSED_GLOBALS
            Set_Dialog_Form(New_Form => New_Dialog_Form);
         end Create_Go_To_Dialog_Block;
      end Show_Bookmarks_Form;
   begin
      if New_Action in COPY | MOVE then
         if Bookmarks_List.Contains(Key => Bookmark) then
            Destination_Directory :=
              To_Unbounded_String(Source => Bookmarks_List(Bookmark));
         elsif Bookmark =
           Mc(Interp => Interpreter, Src_String => "{Home}") then
            Destination_Directory :=
              To_Unbounded_String(Source => Value(Name => "HOME"));
         elsif Bookmark = Mc(Interp => Interpreter, Src_String => "Close") then
            Update_Directory_List;
            ShowDestination;
            return DESTINATION_VIEW;
         else
            Show_Bookmarks_Form;
            return BOOKMARKS_FORM;
         end if;
         Update_Directory_List;
         Load_Directory
           (Directory_Name => To_String(Source => Destination_Directory),
            Second => True);
         ShowDestination;
         return DESTINATION_VIEW;
      end if;
      if Bookmark = Mc(Interp => Interpreter, Src_String => "Close") then
         Show_Preview;
         Ui_Location := DIRECTORY_VIEW;
         Update_Directory_List;
         return DIRECTORY_VIEW;
      end if;
      if Bookmarks_List.Contains(Key => Bookmark) then
         New_Action := Default_Item_Action;
         Create_Program_Menu(Update => True);
         Common.Current_Directory :=
           To_Unbounded_String(Source => Bookmarks_List(Bookmark));
      elsif Bookmark = Mc(Interp => Interpreter, Src_String => "{Home}") then
         New_Action := Default_Item_Action;
         Create_Program_Menu(Update => True);
         Common.Current_Directory :=
           To_Unbounded_String(Source => Value(Name => "HOME"));
      else
         Update_Directory_List;
         Show_Preview;
         Show_Bookmarks_Form;
         return BOOKMARKS_FORM;
      end if;
      Load_Directory
        (Directory_Name => To_String(Source => Common.Current_Directory));
      Ui_Location := DIRECTORY_VIEW;
      Clear_Preview_Window;
      Update_Directory_List(Clear => True);
      Show_Preview;
      Update_Watch(Path => To_String(Source => Common.Current_Directory));
      Execute_Modules
        (Interpreter => Interpreter, State => On_Enter_Trigger,
         Arguments =>
           "{" & To_String(Source => Common.Current_Directory) & "}");
      return DIRECTORY_VIEW;
   end Go_To_Bookmark;

   function Bookmarks_Form_Keys(Key: Key_Code) return Ui_Locations is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Messages.UI;

      Result: Forms.Driver_Result := Unknown_Request;
      Dialog_Frm: Forms.Form := Get_Dialog_Form;
      Field_Index: constant Positive :=
        Get_Index(Fld => Current(Frm => Dialog_Frm));
      Visibility: Cursor_Visibility := Invisible;
      function Hide_Dialog return Ui_Locations is
      begin
         Set_Cursor_Visibility(Visibility => Visibility);
         Post(Frm => Dialog_Frm, Post => False);
         Delete(Frm => Dialog_Frm);
         Set_Dialog_Form(New_Form => Dialog_Frm);
         if New_Action not in MOVE | COPY then
            Show_Preview;
            Ui_Location := DIRECTORY_VIEW;
            Update_Directory_List(Clear => True);
            return DIRECTORY_VIEW;
         else
            Update_Directory_List;
            ShowDestination;
            return PREVIEW;
         end if;
      end Hide_Dialog;
   begin
      case Key is
         when KEY_UP =>
            Result := Go_Previous_Field(DialogForm => Dialog_Frm);
         when KEY_DOWN =>
            Result := Go_Next_Field(DialogForm => Dialog_Frm);
         when KEY_LEFT =>
            if Field_Index = 2 then
               Result := Driver(Frm => Dialog_Frm, Key => F_Previous_Char);
            end if;
         when KEY_RIGHT =>
            if Field_Index = 2 then
               Result := Driver(Frm => Dialog_Frm, Key => F_Next_Char);
            end if;
         when 127 =>
            Result := Driver(Frm => Dialog_Frm, Key => F_Delete_Previous);
         when 27 =>
            return Hide_Dialog;
         when 10 =>
            if Field_Index = 2 then
               Result := Go_Previous_Field(DialogForm => Dialog_Frm);
               return Bookmarks_Form_Keys(Key => 10);
            end if;
            if Field_Index = 4 then
               if not Ada.Directories.Exists
                   (Name =>
                      Trim
                        (Source =>
                           Get_Buffer
                             (Fld => Fields(Frm => Dialog_Frm, Index => 2)),
                         Side => Both)) then
                  Show_Message
                    (Message =>
                       Mc(Interp => Interpreter, Src_String => "{Directory}") &
                       " " &
                       Trim
                         (Source =>
                            Get_Buffer
                              (Fld => Fields(Frm => Dialog_Frm, Index => 2)),
                          Side => Both) &
                       " " &
                       Mc(Interp => Interpreter,
                          Src_String => "{doesn't exist.}"));
                  return MESSAGE_FORM;
               end if;
               if New_Action not in MOVE | COPY then
                  New_Action := Default_Item_Action;
                  Common.Current_Directory :=
                    To_Unbounded_String
                      (Source =>
                         Trim
                           (Source =>
                              Get_Buffer
                                (Fld => Fields(Frm => Dialog_Frm, Index => 2)),
                            Side => Both));
                  Load_Directory
                    (Directory_Name =>
                       To_String(Source => Common.Current_Directory));
                  Update_Watch
                    (Path => To_String(Source => Common.Current_Directory));
                  Execute_Modules
                    (Interpreter => Interpreter, State => On_Enter_Trigger,
                     Arguments =>
                       "{" & To_String(Source => Common.Current_Directory) &
                       "}");
               else
                  Destination_Directory :=
                    To_Unbounded_String
                      (Source =>
                         Trim
                           (Source =>
                              Get_Buffer
                                (Fld => Fields(Frm => Dialog_Frm, Index => 2)),
                            Side => Both));
                  Load_Directory
                    (Directory_Name =>
                       To_String(Source => Destination_Directory),
                     Second => True);
               end if;
            end if;
            if Field_Index /= 2 then
               return Hide_Dialog;
            end if;
         when others =>
            if Key /= 91 then
               Result := Driver(Frm => Dialog_Frm, Key => Key);
            end if;
      end case;
      if Result = Form_Ok then
         Refresh(Win => Form_Window);
      end if;
      return BOOKMARKS_FORM;
   end Bookmarks_Form_Keys;

   procedure Add_Bookmark is
      File: File_Type;
   begin
      if Ada.Directories.Exists
          (Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks") then
         Open
           (File => File, Mode => Append_File,
            Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks");
      else
         Create_Path
           (New_Directory => Value(Name => "HOME") & "/.config/gtk-3.0/");
         Create
           (File => File, Mode => Append_File,
            Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Put_Line(File => File, Item => "file://" & Current_Selected);
      Close(File => File);
      Create_Bookmarks_List;
   end Add_Bookmark;

   procedure Remove_Bookmark is
      New_File, Old_File: File_Type;
      Line, Path: Unbounded_String := Null_Unbounded_String;
      Added: Boolean := False;
   begin
      Rename
        (Old_Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks",
         New_Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks.old");
      Open
        (File => Old_File, Mode => In_File,
         Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks.old");
      Create
        (File => New_File, Mode => Out_File,
         Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks");
      Update_Bookmarks_Loop :
      while not End_Of_File(File => Old_File) loop
         Line := Get_Line(File => Old_File);
         if Length(Source => Line) > 7
           and then Slice(Source => Line, Low => 1, High => 7) = "file://" then
            Path :=
              Unbounded_Slice
                (Source => Line, Low => 8, High => Length(Source => Line));
            if Path /= Current_Selected then
               Put_Line(File => New_File, Item => Line);
               Added := True;
            end if;
         end if;
      end loop Update_Bookmarks_Loop;
      Close(File => New_File);
      Close(File => Old_File);
      Delete_File
        (Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks.old");
      if not Added then
         Delete_File
           (Name => Value(Name => "HOME") & "/.config/gtk-3.0/bookmarks");
      end if;
      Create_Bookmarks_List;
   end Remove_Bookmark;

end Bookmarks.UI;
