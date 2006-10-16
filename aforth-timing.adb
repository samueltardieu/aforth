with Ada.Real_Time; use Ada.Real_Time;

package body Aforth.Timing is

   --------
   -- Ms --
   --------

   procedure Ms is
   begin
      delay until Clock + To_Time_Span (Duration (Float (Pop) / 1000.0));
   end Ms;

begin
   Register_Ada_Word ("MS", Ms'Access);
end Aforth.Timing;
