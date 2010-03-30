with Interfaces; use Interfaces;

package Forth.Types is

   pragma Pure;

   type Cell is new Integer_32;

   type Cell_Array is array (Positive range <>) of Cell;

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   Compile_Only                 : exception;
   Unbalanced_Control_Structure : exception;

end Forth.Types;
