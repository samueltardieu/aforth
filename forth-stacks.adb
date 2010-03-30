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
