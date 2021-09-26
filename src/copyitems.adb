-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Tcl.MsgCat.Ada;
with Messages;
with Preferences; use Preferences;
with ShowItems;
with Utils.UI;

package body CopyItems is

   procedure Copy_Item
     (Name: String; Path: Unbounded_String; Success: in out Boolean) is
      use Utils.UI;

      New_Path: Unbounded_String := Path;
      procedure Copy_File(File_Name: String) is
         New_Name: Unbounded_String :=
           New_Path &
           To_Unbounded_String(Source => "/" & Simple_Name(Name => File_Name));
      begin
         if Exists(Name => To_String(Source => New_Name)) then
            if Settings.Overwrite_On_Exist then
               Delete_File(Name => To_String(Source => New_Name));
            else
               New_File_Name_Loop :
               loop
                  New_Name :=
                    New_Path &
                    To_Unbounded_String
                      (Source =>
                         "/" &
                         Base_Name(Name => To_String(Source => New_Name)) &
                         "_." &
                         Extension(Name => To_String(Source => New_Name)));
                  exit New_File_Name_Loop when not Exists
                      (Name => To_String(Source => New_Name));
               end loop New_File_Name_Loop;
            end if;
         end if;
         GNAT.OS_Lib.Copy_File
           (Name => File_Name, Pathname => To_String(Source => New_Name),
            Success => Success, Mode => Copy, Preserve => Full);
      end Copy_File;
      procedure Process_File(Item: Directory_Entry_Type) is
      begin
         Copy_File(File_Name => Full_Name(Directory_Entry => Item));
      end Process_File;
      procedure Process_Directory(Item: Directory_Entry_Type) is
      begin
         if Simple_Name(Directory_Entry => Item) not in "." | ".." then
            Copy_Item
              (Name => Full_Name(Directory_Entry => Item), Path => New_Path,
               Success => Success);
         end if;
      exception
         when Ada.Directories.Name_Error =>
            null;
      end Process_Directory;
   begin
      if Is_Directory(Name => Name) then
         Append
           (Source => New_Path, New_Item => "/" & Simple_Name(Name => Name));
         if Exists(Name => To_String(Source => New_Path)) and
           not Settings.Overwrite_On_Exist then
            New_Directory_Name_Loop :
            loop
               New_Path := New_Path & "_";
               exit New_Directory_Name_Loop when not Exists
                   (Name => To_String(Source => New_Path));
            end loop New_Directory_Name_Loop;
         end if;
         Create_Path(New_Directory => To_String(Source => New_Path));
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => False, others => True),
            Process => Process_File'Access);
         Search
           (Directory => Name, Pattern => "",
            Filter => (Directory => True, others => False),
            Process => Process_Directory'Access);
      else
         Copy_File(File_Name => Name);
      end if;
      Update_Progress_Bar;
   end Copy_Item;

   function Copy_Items
     (Interpreter: Tcl_Interp; Overwrite: in out Boolean) return Boolean is
      use Tcl.MsgCat.Ada;
      use Messages;
      use ShowItems;

      Path, Item_Type: Unbounded_String := Null_Unbounded_String;
      Success: Boolean := True;
   begin
      Copy_Items_Loop :
      while Copy_Items_List.Length > 0 loop
         Path := DestinationDirectory;
         if Exists
             (Name =>
                To_String(Source => Path) & "/" &
                Simple_Name
                  (Name => To_String(Source => Copy_Items_List(1)))) and
           not Overwrite and Settings.Overwrite_On_Exist then
            Item_Type :=
              (if
                 Is_Directory
                   (Name =>
                      To_String(Source => Path) & "/" &
                      Simple_Name
                        (Name => To_String(Source => Copy_Items_List(1))))
               then
                 To_Unbounded_String
                   (Source =>
                      Mc(Interp => Interpreter, Src_String => "{Directory}"))
               else To_Unbounded_String
                   (Source =>
                      Mc(Interp => Interpreter, Src_String => "{File}")));
            Show_Message
              (Message =>
                 To_String(Source => Item_Type) & " " &
                 Simple_Name(Name => To_String(Source => Copy_Items_List(1))) &
                 " " &
                 Mc(Interp => Interpreter,
                    Src_String => "{exists. Do you want to overwrite it?}"),
               Message_Type => "question");
            return False;
         end if;
         Copy_Item
           (Name => To_String(Source => Copy_Items_List(1)), Path => Path,
            Success => Success);
         exit Copy_Items_Loop when not Success;
         Copy_Items_List.Delete(Index => 1);
         if not Yes_For_All then
            Overwrite := False;
         end if;
      end loop Copy_Items_Loop;
      Copy_Items_List.Clear;
      return True;
   end Copy_Items;

end CopyItems;
