with Ada.Real_Time; use Ada.Real_Time;

package body Aforth.Timing is

   --------
   -- MS --
   --------

   procedure MS is
   begin
      delay until Clock + Milliseconds (Integer (Pop));
   end MS;

begin
   Register_Ada_Word ("MS", MS'Access);
end Aforth.Timing;
