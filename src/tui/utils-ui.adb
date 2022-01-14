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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with MainWindow; use MainWindow;
with ShowItems; use ShowItems;

package body Utils.UI is

   -- ****iv* UtilsTUI/UtilsTUI.ProgressIndex
   -- FUNCTION
   -- Currrent index of item
   -- SOURCE
   ProgressIndex: Natural;
   -- ****

   procedure Set_Progress_Bar(Amount: Positive) is
      pragma Unreferenced(Amount);
   begin
      ProgressIndex := 0;
   end Set_Progress_Bar;

   procedure Update_Progress_Bar is
   begin
      ProgressIndex := ProgressIndex + 1;
   end Update_Progress_Bar;

   procedure Create_Dialog
     (DialogForm: in out Forms.Form; FormWindow: out Window;
      Form_Height: out Line_Position; Form_Length: out Column_Position) is
      UnusedResult: Forms.Driver_Result := Unknown_Request;
   begin
      Set_Options(DialogForm, (others => False));
      Scale(DialogForm, Form_Height, Form_Length);
      FormWindow :=
        Create
          (Form_Height + 2, Form_Length + 2, ((Lines / 3) - (Form_Height / 2)),
           ((Columns / 2) - (Form_Length / 2)));
      Box(FormWindow, Default_Character, Default_Character);
      Set_Window(DialogForm, FormWindow);
      Set_Sub_Window
        (DialogForm,
         Derived_Window(FormWindow, Form_Height, Form_Length, 1, 1));
      Post(DialogForm);
      UnusedResult := Driver(DialogForm, REQ_END_LINE);
      Refresh;
      Refresh(FormWindow);
   end Create_Dialog;

   procedure Delete_Dialog
     (DialogForm: in out Forms.Form; Clear: Boolean := False) is
   begin
      Post(DialogForm, False);
      Delete(DialogForm);
      Show_Preview;
      UILocation := DIRECTORY_VIEW;
      Update_Directory_List(Clear);
   end Delete_Dialog;

   function Go_Previous_Field
     (DialogForm: Forms.Form) return Forms.Driver_Result is
   begin
      return Result: Forms.Driver_Result do
         Result := Driver(DialogForm, F_Previous_Field);
         Result := Driver(DialogForm, F_End_Line);
      end return;
   end Go_Previous_Field;

   function Go_Next_Field(DialogForm: Forms.Form) return Forms.Driver_Result is
   begin
      return Result: Forms.Driver_Result do
         Result := Driver(DialogForm, F_Next_Field);
         Result := Driver(DialogForm, F_End_Line);
      end return;
   end Go_Next_Field;

end Utils.UI;
