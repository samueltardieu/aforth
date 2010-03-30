with Interfaces; use Interfaces;

package Forth.Types is

   pragma Pure;

   type Cell is new Integer_32;

   type Cell_Array is array (Positive range <>) of Cell;

   Compile_Only                 : exception;
   Stack_Underflow              : exception;
   Unbalanced_Control_Structure : exception;
   Word_Not_Found               : exception;

   procedure Raise_Word_Not_Found (Word : String);
   pragma No_Return (Raise_Word_Not_Found);

end Forth.Types;
