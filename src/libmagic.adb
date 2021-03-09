-- Copyright (c) 2019-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Utils.UI; use Utils.UI;

package body LibMagic is

   -- ****it* LibMagic/LibMagic.Magic_Set
   -- FUNCTION
   -- Used to store Magic data
   -- SOURCE
   type Magic_Set is null record;
   -- ****

   -- ****it* LibMagic/LibMagic.Magic_T
   -- FUNCTION
   -- Used as pointer to the Magic data
   -- SOURCE
   type Magic_T is access all Magic_Set;
   -- ****

   -- ****iv* LibMagic/LibMagic.Magic_Data
   -- FUNCTION
   -- Pointer to the Magic data
   -- SOURCE
   Magic_Data: Magic_T;
   -- ****

   -- ****iv* LibMagic/LibMagic.Initialized
   -- FUNCTION
   -- If true, libmagic was succesfully initialized. Default is false.
   -- SOURCE
   Initialized: Boolean := False;
   -- ****

   -- ****if* LibMagic/LibMagic.Magic_Open_C
   -- FUNCTION
   -- Binding to the C function
   -- PARAMETERS
   -- Arg1 - Type of data to retrieve
   -- RESULT
   -- New pointer to the magic data
   -- SOURCE
   function Magic_Open_C(Arg1: int) return Magic_T with
      Import => True,
      Convention => C,
      External_Name => "magic_open";
      -- ****

      -- ****if* LibMagic/LibMagic.Magic_Load_C
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- Arg1 - Pointer to the Magic data
      -- Arg2 - unused, set to Null_Ptr
      -- RESULT
      -- 0 if data was loaded
      -- SOURCE
   function Magic_Load_C(Arg1: Magic_T; Arg2: chars_ptr) return int with
      Import => True,
      Convention => C,
      External_Name => "magic_load";
      -- ****

      -- ****if* LibMagic/LibMagic.Magic_Close_C
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- Arg1 -  Pointer to the Magic data
      -- SOURCE
   procedure Magic_Close_C(Arg1: Magic_T) with
      Import => True,
      Convention => C,
      External_Name => "magic_close";
      -- ****

      -- ****if* LibMagic/LibMagic.Magic_File_C
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- Arg1 - Pointer to the Magic data
      -- Arg2 - Full path the the file which will be checked
      -- RESULT
      -- MIME Type of selected file
      -- SOURCE
   function Magic_File_C(Arg1: Magic_T; Arg2: chars_ptr) return chars_ptr with
      Import => True,
      Convention => C,
      External_Name => "magic_file";
      -- ****

   procedure Magic_Open is
   begin
      Magic_Data := Magic_Open_C(Arg1 => 16#0000010#);
      if Magic_Load_C
          (Arg1 => Magic_Data,
           Arg2 =>
             New_String
               (Str =>
                  Value(Name => "APPDIR", Default => "") &
                  "/usr/share/file/magic")) /=
        -1 then
         Initialized := True;
      end if;
   end Magic_Open;

   function Magic_File(Name: String) return String is
   begin
      if Initialized then
         return Value
             (Item =>
                Magic_File_C
                  (Arg1 => Magic_Data, Arg2 => New_String(Str => Name)));
      end if;
      Get_Mime_Type_Block :
      declare
         Process_Desc: Process_Descriptor;
         Result: Expect_Match;
         Executable_Name: constant String :=
           FindExecutable(Name => "xdg-mime");
         Mime_Type: Unbounded_String;
      begin
         if Executable_Name = "" then
            return "unknown";
         end if;
         Non_Blocking_Spawn
           (Descriptor => Process_Desc, Command => Executable_Name,
            Args => Argument_String_To_List("query filetype " & Name).all);
         Expect(Process_Desc, Result, Regexp => ".+", Timeout => 1_000);
         Mime_Type :=
           (if Result = 1 then
              To_Unbounded_String(Expect_Out_Match(Process_Desc))
            else To_Unbounded_String("unknown"));
         Close(Process_Desc);
         return To_String(Mime_Type);
      exception
         when Process_Died =>
            return "unknown";
      end Get_Mime_Type_Block;
   end Magic_File;

   procedure Magic_Close is
   begin
      if Initialized then
         Magic_Close_C(Magic_Data);
      end if;
   end Magic_Close;

end LibMagic;
