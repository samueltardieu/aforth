------------------------------------------------------------------------------
--                                                                          --
--                            AFORTH COMPONENTS                             --
--                                                                          --
--                         F O R T H . S T A C K S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2010 Samuel Tardieu <sam@rfc1149.net>        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--   The main repository for this software is located at:                   --
--       http://git.rfc1149.net/aforth.git                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Forth.Types;            use Forth.Types;

package Forth.Stacks is

   pragma Preelaborate;

   type Stack_Type is limited private;
   --  The stack elements go from 1 to Length (Stack)

   procedure New_Stack (Stack : out Stack_Type; Stack_Size : Cell);
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

   type Stack_Type is record
      Data : access Stacks.Vector;
      Size : Cell;
   end record;

end Forth.Stacks;
