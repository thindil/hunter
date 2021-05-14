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

with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with System;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Expect;
with GNAT.OS_Lib;
with Utils.UI;

package body LibMagic is

   -- ****iv* LibMagic/LibMagic.Magic_Data
   -- FUNCTION
   -- Pointer to the Magic data
   -- SOURCE
   Magic_Data: System.Address;
   -- ****

   -- ****if* LibMagic/LibMagic.Get_Magic_Instance
   -- FUNCTION
   -- Get the instance of libmagic
   -- RESULT
   -- Currently set instance of libmagic
   -- SOURCE
   function Get_Magic_Instance return System.Address is
      -- ****
   begin
      return Magic_Data;
   end Get_Magic_Instance;

   -- ****iv* LibMagic/LibMagic.Initialized
   -- FUNCTION
   -- If true, libmagic was succesfully initialized. Default is false.
   -- SOURCE
   Initialized: Boolean := False;
   -- ****

   -- ****if* LibMagic/LibMagic.Is_Initialized
   -- FUNCTION
   -- Check if libmagic instance is initialized
   -- RESULT
   -- True, if libmagic instance was properly initialized, otherwise False
   -- SOURCE
   function Is_Initialized return Boolean is
      -- ****
   begin
      return Initialized;
   end Is_Initialized;

   procedure Magic_Open is
      use Ada.Environment_Variables;
      use Interfaces.C;

      function Magic_Load_C
        (Arg1: System.Address;
         Arg2: chars_ptr) return int with
         Import => True,
         Convention => C,
         External_Name => "magic_load";
      function Magic_Open_C(Arg1: int) return System.Address with
         Import => True,
         Convention => C,
         External_Name => "magic_open";
   begin
      Magic_Data := Magic_Open_C(Arg1 => 16#0000010#);
      if Magic_Load_C
          (Arg1 => Get_Magic_Instance,
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
      function Magic_File_C
        (Arg1: System.Address;
         Arg2: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "magic_file";
   begin
      if Is_Initialized then
         return Value
             (Item =>
                Magic_File_C
                  (Arg1 => Get_Magic_Instance,
                   Arg2 => New_String(Str => Name)));
      end if;
      Get_Mime_Type_Block: declare
         use Ada.Strings.Unbounded;
         use GNAT.Expect;
         use GNAT.OS_Lib;
         use Utils.UI;

         Process_Desc: Process_Descriptor;
         Result: Expect_Match;
         Executable_Name: constant String :=
           Find_Executable(Name => "xdg-mime");
         Mime_Type: Unbounded_String;
      begin
         if Executable_Name = "" then
            return "unknown";
         end if;
         Non_Blocking_Spawn
           (Descriptor => Process_Desc,
            Command => Executable_Name,
            Args =>
              Argument_String_To_List
                (Arg_String => "query filetype " & Name).all);
         Expect
           (Descriptor => Process_Desc,
            Result => Result,
            Regexp => ".+",
            Timeout => 1_000);
         Mime_Type :=
           (if
              Result = 1
            then
              To_Unbounded_String
                (Source => Expect_Out_Match(Descriptor => Process_Desc))
            else To_Unbounded_String(Source => "unknown"));
         Close(Descriptor => Process_Desc);
         return To_String(Source => Mime_Type);
      exception
         when Process_Died =>
            return "unknown";
      end Get_Mime_Type_Block;
   end Magic_File;

   procedure Magic_Close is
      procedure Magic_Close_C(Arg1: System.Address) with
         Import => True,
         Convention => C,
         External_Name => "magic_close";
   begin
      if Is_Initialized then
         Magic_Close_C(Arg1 => Get_Magic_Instance);
      end if;
   end Magic_Close;

end LibMagic;
