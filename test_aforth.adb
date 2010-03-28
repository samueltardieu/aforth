with Ada.Command_Line;   use Ada.Command_Line;
with Ada.IO_Exceptions;
with Aforth;             use Aforth;
with Aforth.Builtins;
pragma Warnings (Off, Aforth.Builtins);
with Aforth.Timing;
pragma Warnings (Off, Aforth.Timing);

procedure Test_Aforth is
begin
   for I in 1 .. Argument_Count loop
      Include_File (Argument (I));
   end loop;
   Quit;
exception
   when Ada.IO_Exceptions.Name_Error =>
      Set_Exit_Status (1);
   when Bye_Exception =>
      return;
end Test_Aforth;
