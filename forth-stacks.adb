------------------------------------------------------------------------------
--                                                                          --
--                            AFORTH COMPONENTS                             --
--                                                                          --
--                         F O R T H . S T A C K S                          --
--                                                                          --
--                                 B o d y                                  --
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

package body Forth.Stacks is

   -----------
   -- Clear --
   -----------

   procedure Clear (S : Stack_Type) is
   begin
      S.Clear;
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete (S : Stack_Type; I : Positive) is
   begin
      if Length (S) < I then
         raise Stack_Underflow;
      end if;
      S.Delete (I);
   end Delete;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (S : Stack_Type) is
   begin
      if S.Is_Empty then
         raise Stack_Underflow;
      end if;
      S.Delete_Last;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element (S : Stack_Type; I : Positive) return Cell is
   begin
      if Length (S) < I then
         raise Stack_Underflow;
      end if;
      return S.Element (I);
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert (S : Stack_Type; I : Positive; C : Cell) is
   begin
      if Length (S) < I then
         raise Stack_Underflow;
      end if;
      S.Insert (I, C);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Stack_Type) return Boolean is
   begin
      return S.Is_Empty;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   function Length (S : Stack_Type) return Natural is
   begin
      return Natural (S.Length);
   end Length;

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack return Stack_Type is
   begin
      return new Stacks.Vector;
   end New_Stack;

   ----------
   -- Peek --
   ----------

   function Peek (S : Stack_Type) return Cell is
   begin
      if S.Is_Empty then
         raise Stack_Underflow;
      end if;
      return S.Last_Element;
   end Peek;

   ---------
   -- Pop --
   ---------

   function Pop (S : Stack_Type) return Cell is
   begin
      if S.Is_Empty then
         raise Stack_Underflow;
      end if;
      return Result : Cell do
         Result := S.Last_Element;
         S.Delete_Last;
      end return;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (S : Stack_Type; X : Cell) is
   begin
      S.Append (X);
   end Push;

end Forth.Stacks;
