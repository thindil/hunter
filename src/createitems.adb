with Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Tcl.MsgCat.Ada;
with Messages;

package body CreateItems is

   function Is_Creating_Possible
     (New_Item_Name, Item_Type: String; Interp: Tcl.Tcl_Interp)
      return Boolean is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use GNAT.OS_Lib;
      use Tcl.MsgCat.Ada;
      use Messages;

      Action_String, Action_Blocker: Unbounded_String := Null_Unbounded_String;
   begin
      if Exists(Name => New_Item_Name) or
        Is_Symbolic_Link(Name => New_Item_Name) then
         Action_String :=
           To_Unbounded_String
             (Source =>
                Mc(Interp => Interp, Src_String => "{create}") & " " &
                Item_Type & " " &
                Mc(Interp => Interp, Src_String => "{with}"));
         Action_Blocker :=
           (if Is_Directory(Name => New_Item_Name) then
              To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "directory"))
            else To_Unbounded_String
                (Source => Mc(Interp => Interp, Src_String => "file")));
         Show_Message
           (Message =>
              Mc(Interp => Interp, Src_String => "{You can't}") & " " &
              To_String(Source => Action_String) & " " &
              Mc(Interp => Interp, Src_String => "{name}") & " '" &
              New_Item_Name & "' " &
              Mc(Interp => Interp, Src_String => "{because there exists}") &
              " " & To_String(Source => Action_Blocker) & " " &
              Mc(Interp => Interp, Src_String => "{with that name.}"));
         return False;
      end if;
      if not Is_Write_Accessible_File
          (Name => Containing_Directory(Name => New_Item_Name)) then
         Show_Message
           (Message =>
              Mc
                (Interp => Interp,
                 Src_String => "{You don't have permissions to write to}") &
              " " & Containing_Directory(Name => New_Item_Name));
         return False;
      end if;
      return True;
   end Is_Creating_Possible;

end CreateItems;
