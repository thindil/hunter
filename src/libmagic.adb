-- Copyright (c) 2019-2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

   -- ****it* LibMagic/LibMagic.magic_set
   -- FUNCTION
   -- Used to store Magic data
   -- SOURCE
   type magic_set is null record;
   -- ****

   -- ****it* LibMagic/LibMagic.magic_t
   -- FUNCTION
   -- Used as pointer to the Magic data
   -- SOURCE
   type magic_t is access all magic_set;
   -- ****

   -- ****iv* LibMagic/LibMagic.MagicData
   -- FUNCTION
   -- Pointer to the Magic data
   -- SOURCE
   MagicData: magic_t;
   -- ****

   -- ****iv* LibMagic/LibMagic.Initialized
   -- FUNCTION
   -- If true, libmagic was succesfully initialized. Default is false.
   -- SOURCE
   Initialized: Boolean := False;
   -- ****

   -- ****if* LibMagic/LibMagic.magic_open
   -- FUNCTION
   -- Binding to the C function
   -- PARAMETERS
   -- arg1 - Type of data to retrieve
   -- RESULT
   -- New pointer to the magic data
   -- SOURCE
   function magic_open(arg1: int) return magic_t with
      Import => True,
      Convention => C,
      External_Name => "magic_open";
      -- ****

      -- ****if* LibMagic/LibMagic.magic_load
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- arg1 - Pointer to the Magic data
      -- arg2 - unused, set to Null_Ptr
      -- RESULT
      -- 0 if data was loaded
      -- SOURCE
   function magic_load(arg1: magic_t; arg2: chars_ptr) return int with
      Import => True,
      Convention => C,
      External_Name => "magic_load";
      -- ****

      -- ****if* LibMagic/LibMagic.magic_close
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- arg1 -  Pointer to the Magic data
      -- SOURCE
   procedure magic_close(arg1: magic_t) with
      Import => True,
      Convention => C,
      External_Name => "magic_close";
      -- ****

      -- ****if* LibMagic/LibMagic.magic_file
      -- FUNCTION
      -- Binding to the C function
      -- PARAMETERS
      -- arg1 - Pointer to the Magic data
      -- arg2 - Full path the the file which will be checked
      -- RESULT
      -- MIME Type of selected file
      -- SOURCE
   function magic_file(arg1: magic_t; arg2: chars_ptr) return chars_ptr with
      Import => True,
      Convention => C,
      External_Name => "magic_file";
      -- ****

   procedure Magic_Open is
   begin
      MagicData := magic_open(16#0000010#);
      if magic_load
          (MagicData,
           New_String(Value("APPDIR", "") & "/usr/share/file/magic")) /=
        -1 then
         Initialized := True;
      end if;
   end Magic_Open;

   function Magic_File(Name: String) return String is
   begin
      if Initialized then
         return Value(magic_file(MagicData, New_String(Name)));
      else
         declare
            ProcessDesc: Process_Descriptor;
            Result: Expect_Match;
            ExecutableName: constant String := FindExecutable("xdg-mime");
            MimeType: Unbounded_String;
         begin
            if ExecutableName = "" then
               return "unknown";
            end if;
            Non_Blocking_Spawn
              (ProcessDesc, ExecutableName,
               Argument_String_To_List("query filetype " & Name).all);
            Expect(ProcessDesc, Result, Regexp => ".+", Timeout => 1_000);
            MimeType :=
              (if Result = 1 then
                 To_Unbounded_String(Expect_Out_Match(ProcessDesc))
               else To_Unbounded_String("unknown"));
            Close(ProcessDesc);
            return To_String(MimeType);
         exception
            when Process_Died =>
               return "unknown";
         end;
      end if;
   end Magic_File;

   procedure Magic_Close is
   begin
      if Initialized then
         magic_close(MagicData);
      end if;
   end Magic_Close;

end LibMagic;
