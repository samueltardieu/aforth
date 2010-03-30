with Ada.Containers.Vectors;
with Forth.Types;            use Forth.Types;

package Forth.Stacks is

   pragma Preelaborate;

   type Stack_Type is private;
   --  The stack elements go from 1 to Length (Stack)

   function New_Stack return Stack_Type;
   --  Create a new empty stack

   procedure Push (S : Stack_Type; X : Cell);
   --  Push an element to the top of the stack

   function Pop (S : Stack_Type) return Cell;
   --  Remove the element from the top of the stack

   function Peek (S : Stack_Type) return Cell;
   --  Return the top of the stack

   function Length (S : Stack_Type) return Natural;
   --  Return the number of elements on the stack

   function Element (S : Stack_Type; I : Positive) return Cell;
   --  Return one element from the stack

   function Is_Empty (S : Stack_Type) return Boolean;
   --  Check whether the stack is empty

   procedure Clear (S : Stack_Type);
   --  Clear the stack

   procedure Insert (S : Stack_Type; I : Positive; C : Cell);
   --  Insert an element before position I

   procedure Delete (S : Stack_Type; I : Positive);
   --  Remove the element at position I

   procedure Delete_Last (S : Stack_Type);
   --  Remove the last element of the stack

private

   package Stacks is
      new Ada.Containers.Vectors (Positive, Cell);

   type Stack_Type is not null access Stacks.Vector;

end Forth.Stacks;
