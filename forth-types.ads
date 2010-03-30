with Interfaces; use Interfaces;

package Forth.Types is

   pragma Pure;

   type Cell is new Integer_32;

   type Cell_Array is array (Positive range <>) of Cell;

   Compile_Only                 : exception;
   Stack_Underflow              : exception;
   Unbalanced_Control_Structure : exception;
   Word_Not_Found               : exception;

end Forth.Types;
