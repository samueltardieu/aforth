package body Forth.Types is

   --------------------------
   -- Raise_Word_Not_Found --
   --------------------------

   procedure Raise_Word_Not_Found (Word : String) is
   begin
      raise Word_Not_Found with Word;
   end Raise_Word_Not_Found;

end Forth.Types;
