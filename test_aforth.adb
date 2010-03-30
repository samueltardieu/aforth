with Ada.Command_Line;   use Ada.Command_Line;
with Ada.IO_Exceptions;
with Forth.Interpreter;  use Forth.Interpreter;
with Forth.Types;        use Forth.Types;

procedure Test_Aforth is
   Interpreter : constant Interpreter_Type := New_Interpreter;
begin
   for I in 1 .. Argument_Count loop
      Include_File (Interpreter, Argument (I));
   end loop;
   Quit (Interpreter);
exception
   when Ada.IO_Exceptions.Name_Error =>
      Set_Exit_Status (1);
   when Bye_Exception =>
      return;
end Test_Aforth;
