with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Read_Line;

package body Aforth is

   --  Notes:
   --    - the compilation stack is the data stack

   package Integer_32_IO is new Ada.Text_IO.Integer_IO (Integer_32);
   use Integer_32_IO;

   procedure Free is
      new Ada.Unchecked_Deallocation (Dictionary_Array, Dictionary_Access);

   type Byte_Access is access all Unsigned_8;

   pragma Warnings (Off);
   function To_Integer_32_Access is
      new Ada.Unchecked_Conversion (Byte_Access, Integer_32_Access);
   pragma Warnings (On);

   function To_Unsigned_32 is
      new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function To_Integer_32 is
      new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);

   Already_Handled : exception;

   Here      : Integer_32_Access;
   Base      : Integer_32_Access;
   TIB       : Integer_32;
   TIB_Count : Integer_32_Access;
   IN_Ptr    : Integer_32_Access;
   State     : Integer_32_Access;

   Forth_Exit : constant Action_Type := (Kind       => Forth_Word,
                                         Immediate  => True,
                                         Forth_Proc => -1);

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32_Access);

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32);

   Current_Name   : String_Access;
   Current_Action : Action_Type (Forth_Word);
   Current_IP     : Integer_32 := -1;

   Use_RL         : Boolean := True;
   --  Should the current input method use Read_Line?

   procedure Start_Definition (Name : in String);

   function To_String return String;

   procedure Execute_Action (Action : in Action_Type);

   procedure Execute_Forth_Word (Addr : in Integer_32);

   procedure Main_Loop;

   function Word return String;

   procedure Jump;
   procedure Jump_If_False;
   procedure Patch_Jump (To_Patch : in Integer_32; Target : in Integer_32);

   procedure Add_To_Compilation_Buffer (Ada_Proc : in Ada_Word_Access);
   procedure Add_To_Compilation_Buffer (Value : in Integer_32);

   procedure DoDoes;

   procedure Refill_Line (Buffer : in String);

   procedure Check_Compile_Only;

   procedure Tick (Name : in String);

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Action : in Action_Type) is
   begin
      Check_Compile_Only;

      --  Properly inline Ada words called through a Forth wrapper
      --  as they may need to preserve the return stack (>R and friends).

      if Action.Kind = Forth_Word and then Action /= Forth_Exit then
         declare
            First_Action : constant Action_Type :=
              Compilation_Buffer (Action.Forth_Proc);
         begin
            if First_Action.Kind = Ada_Word and then
              Compilation_Buffer (Action.Forth_Proc + 1) = Forth_Exit
            then
               Add_To_Compilation_Buffer (First_Action);
               return;
            end if;
         end;
      end if;
      Compilation_Buffer (Compilation_Index) := Action;
      Compilation_Index := Compilation_Index + 1;
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Ada_Proc : in Ada_Word_Access) is
   begin
      Add_To_Compilation_Buffer
        (Action_Type'(Kind      => Ada_Word,
                      Immediate => True,
                      Ada_Proc  => Ada_Proc));
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Value : in Integer_32) is
   begin
      Add_To_Compilation_Buffer
        (Action_Type'(Kind      => Number,
                      Immediate => True,
                      Value     => Value));
   end Add_To_Compilation_Buffer;

   -----------
   -- Again --
   -----------

   procedure Again is
   begin
      Literal;
      Add_To_Compilation_Buffer (Jump'Access);
      Drop;
   end Again;

   -----------
   -- Ahead --
   -----------

   procedure Ahead is
   begin
      Push (Compilation_Index);
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump'Access);
   end Ahead;

   -----------
   -- Align --
   -----------

   procedure Align is
   begin
      if Here.all mod 4 /= 0 then
         Here.all := Here.all + (4 - (Here.all mod 4));
      end if;
   end Align;

   --------
   -- Bl --
   --------

   procedure Bl is
   begin
      Push (32);
   end Bl;

   ------------
   -- Bounds --
   ------------

   procedure Bounds is
   begin
      Over;
      Plus;
   end Bounds;

   ------------
   -- Cfetch --
   ------------

   procedure Cfetch is
   begin
      Push (Integer_32 (Memory (Pop)));
   end Cfetch;

   ------------
   -- Cfetch --
   ------------

   function Cfetch (Addr : Integer_32) return Integer_32 is
   begin
      Push (Addr);
      Cfetch;
      return Pop;
   end Cfetch;

   ----------
   -- Char --
   ----------

   procedure Char is
   begin
      Push (Character'Pos (Word (1)));
   end Char;

   ------------------------
   -- Check_Compile_Only --
   ------------------------

   procedure Check_Compile_Only is
   begin
      if State.all /= 1 then
         raise Compile_Only;
      end if;
   end Check_Compile_Only;

   -------------------
   -- Compile_Comma --
   -------------------

   procedure Compile_Comma is
   begin
      Add_To_Compilation_Buffer (Pop);
      Add_To_Compilation_Buffer (Execute'Access);
   end Compile_Comma;

   ------------------
   -- Compile_Mode --
   ------------------

   procedure Compile_Mode is
   begin
      State.all := 1;
   end Compile_Mode;

   ------------
   -- Cquote --
   ------------

   procedure Cquote is
      Length : Integer_32;
      Addr   : Integer_32;
   begin
      Check_Compile_Only;
      Push (Character'Pos ('"'));
      Parse;
      Length := Pop;
      Addr := Pop;
      Add_To_Compilation_Buffer (Here.all);
      Push (Length);
      Ccomma;
      for I in 1 .. Length loop
         Push (Integer_32 (Memory (Addr)));
         Ccomma;
         Addr := Addr + 1;
      end loop;
   end Cquote;

   -----------
   -- Colon --
   -----------

   procedure Colon is
   begin
      Start_Definition (Word);
   end Colon;

   ------------------
   -- Colon_Noname --
   ------------------

   procedure Colon_Noname is
   begin
      Push (Compilation_Index);
      Start_Definition ("");
   end Colon_Noname;

   ------------
   -- Ccomma --
   ------------

   procedure Ccomma is
   begin
      Memory (Here.all) := Unsigned_8 (Pop);
      Here.all := Here.all + 1;
   end Ccomma;

   -----------
   -- Comma --
   -----------

   procedure Comma is
   begin
      pragma Warnings (Off);
      To_Integer_32_Access (Memory (Here.all) 'Access) .all := Pop;
      pragma Warnings (On);
      Here.all := Here.all + 4;
   end Comma;

   --------
   -- Cr --
   --------

   procedure Cr is
   begin
      Push (13);
      Emit;
      Push (10);
      Emit;
   end Cr;

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Colon;
      Add_To_Compilation_Buffer (Here.all);
      Semicolon;
   end Create;

   ------------
   -- Cstore --
   ------------

   procedure Cstore is
      Addr : constant Integer_32 := Pop;
   begin
      Memory (Addr) := Unsigned_8 (Pop);
   end Cstore;

   -----------
   -- Depth --
   -----------

   procedure Depth is
   begin
      Push (Integer_32 (Data_Stack.Top));
   end Depth;

   ---------
   -- Div --
   ---------

   procedure Div is
      A : constant Integer_32 := Pop;
   begin
      Push (Pop / A);
   end Div;

   ------------
   -- DivMod --
   ------------

   procedure DivMod is
      B : constant Integer_32 := Pop;
      A : constant Integer_32 := Pop;
   begin
      Push (A rem B);
      Push (A / B);
   end DivMod;

   ------------
   -- DoDoes --
   ------------

   procedure DoDoes is
   begin
      --  Patch the latest exit by inserting a call to the current
      --  action.

      pragma Assert (Compilation_Buffer (Compilation_Index - 1) = Forth_Exit);
      Compilation_Buffer (Compilation_Index) :=
        Compilation_Buffer (Compilation_Index - 1);
      Compilation_Buffer (Compilation_Index - 1) :=
        Action_Type'(Kind       => Forth_Word,
                     Immediate  => True,
                     Forth_Proc => Pop);
      Compilation_Index := Compilation_Index + 1;
   end DoDoes;

   ----------
   -- Does --
   ----------

   procedure Does is

      --  Terminate current word after asking to patch the latest created
      --  one. Compilation buffer after index, call to DoDoes and exit
      --  is Compilation_Index + 3.

      Does_Part : constant Integer_32 := Compilation_Index + 3;
   begin
      Add_To_Compilation_Buffer (Does_Part);
      Add_To_Compilation_Buffer (DoDoes'Access);
      Semicolon;

      --  Start an unnamed word corresponding to the DOES> part

      Start_Definition ("");
      pragma Assert (Compilation_Index = Does_Part);
   end Does;

   ---------
   -- Dot --
   ---------

   procedure Dot is
   begin
      Put (Pop, Base => Integer (Base.all), Width => 0);
   end Dot;

   --------------
   -- DotQuote --
   --------------

   procedure DotQuote is
   begin
      Squote;
      if State.all = 0 then
         Forth_Type;
      else
         Add_To_Compilation_Buffer (Forth_Type'Access);
      end if;
   end DotQuote;

   ----------
   -- DotS --
   ----------

   procedure DotS is
   begin
      Put ('<');
      Depth;
      Dot;
      Put ('>');
      for I in 1 .. Data_Stack.Top loop
         Space;
         Push (Data_Stack.Data (I));
         Dot;
      end loop;
   end DotS;

   ----------
   -- Drop --
   ----------

   procedure Drop is
      Value : constant Integer_32 := Pop;
      pragma Unreferenced (Value);
   begin
      null;
   end Drop;

   ---------
   -- Dup --
   ---------

   procedure Dup is
      Value : constant Integer_32 := Pop;
   begin
      Push (Value);
      Push (Value);
   end Dup;

   ----------
   -- Emit --
   ----------

   procedure Emit is
   begin
      Put (Character'Val (Pop));
   end Emit;

   -----------
   -- Equal --
   -----------

   procedure Equal is
   begin
      Push (Pop = Pop);
   end Equal;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Execute_Forth_Word (Pop);
   end Execute;

   --------------------
   -- Execute_Action --
   --------------------

   procedure Execute_Action (Action : in Action_Type) is
   begin
      case Action.Kind is
         when Ada_Word =>
            Action.Ada_Proc.all;
         when Forth_Word =>
            Execute_Forth_Word (Action.Forth_Proc);
         when Number =>
            Push (Action.Value);
      end case;
   end Execute_Action;

   ------------------------
   -- Execute_Forth_Word --
   ------------------------

   procedure Execute_Forth_Word (Addr : in Integer_32) is
   begin
      Push (Return_Stack, Current_IP);
      Current_IP := Addr;
      loop
         declare
            Current_Action : constant Action_Type :=
              Compilation_Buffer (Current_IP);
         begin
            Current_IP := Current_IP + 1;
            if Current_Action = Forth_Exit then
               Current_IP := Pop (Return_Stack);
               return;
            end if;
            Execute_Action (Current_Action);
         end;
      end loop;
   end Execute_Forth_Word;

   -----------
   -- Fetch --
   -----------

   procedure Fetch is
      pragma Warnings (Off);
      Addr  : constant Integer_32_Access :=
        To_Integer_32_Access (Memory (Pop)'Access);
      pragma Warnings (On);
   begin
      Push (Addr.all);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   function Fetch (Addr : Integer_32) return Integer_32 is
   begin
      Push (Addr);
      Fetch;
      return Pop;
   end Fetch;

   ----------
   -- Find --
   ----------

   function Find (Dict : Dictionary_Access; Name : String) return Action_Type
   is
      Lower_Name : constant String := To_Lower (Name);
   begin
      for I in reverse Dict'Range loop
         if To_Lower (Dict (I) .Name.all) = Lower_Name then
            pragma Assert (Dict (I) .Action.Kind = Forth_Word);
            return Dict (I) .Action;
         end if;
      end loop;
      Raise_Exception (Not_Found'Identity,
                       Name & " not found");
   end Find;

   ---------------
   -- Forth_And --
   ---------------

   procedure Forth_And is
   begin
      Push_Unsigned (Pop_Unsigned and Pop_Unsigned);
   end Forth_And;

   -----------------
   -- Forth_Begin --
   -----------------

   procedure Forth_Begin is

      --  The structure of the BEGIN/WHILE/REPEAT loop on the compilation
      --  stack is:
      --    -1
      --    addr of first WHILE to patch
      --    addr of second WHILE to patch
      --    ...
      --    addr of the beginning of the loop

   begin
      Push (-1);
      Push (Compilation_Index);
   end Forth_Begin;

   ----------------
   -- Forth_Else --
   ----------------

   procedure Forth_Else is
   begin
      Ahead;
      Swap;
      Forth_Then;
   end Forth_Else;

   --------------
   -- Forth_If --
   --------------

   procedure Forth_If is
   begin
      Push (Compilation_Index);
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_If;

   ---------------
   -- Forth_Mod --
   ---------------

   procedure Forth_Mod is
      A : constant Integer_32 := Pop;
   begin
      Push (Pop mod A);
   end Forth_Mod;

   --------------
   -- Forth_Or --
   --------------

   procedure Forth_Or is
   begin
      Push_Unsigned (Pop_Unsigned or Pop_Unsigned);
   end Forth_Or;

   ----------------
   -- Forth_Then --
   ----------------

   procedure Forth_Then is
   begin
      Patch_Jump (To_Patch => Pop, Target => Compilation_Index);
   end Forth_Then;

   ----------------
   -- Forth_Type --
   ----------------

   procedure Forth_Type is
   begin
      Put (To_String);
   end Forth_Type;

   -----------------
   -- Forth_Until --
   -----------------

   procedure Forth_Until is
   begin
      Literal;
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_Until;

   -----------------
   -- Forth_While --
   -----------------

   procedure Forth_While is
   begin
      Push (Compilation_Index);
      Swap;
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_While;

   ---------------
   -- Forth_Xor --
   ---------------

   procedure Forth_Xor is
   begin
      Push_Unsigned (Pop_Unsigned xor Pop_Unsigned);
   end Forth_Xor;

   ------------
   -- From_R --
   ------------

   procedure From_R is
   begin
      Push (Pop (Return_Stack));
   end From_R;

   -------------
   -- Greater --
   -------------

   procedure Greater is
      B : constant Integer_32 := Pop;
   begin
      Push (Pop > B);
   end Greater;

   ------------------
   -- Greaterequal --
   ------------------

   procedure Greaterequal is
      B : constant Integer_32 := Pop;
   begin
      Push (Pop >= B);
   end Greaterequal;

   -----------
   -- Ichar --
   -----------

   procedure Ichar is
   begin
      Char;
      Literal;
   end Ichar;

   -------------
   -- Include --
   -------------

   procedure Include is
   begin
      Include_File (Word);
   end Include;

   ------------------
   -- Include_File --
   ------------------

   procedure Include_File (File_Name : in String)
   is
      Previous_Input : constant File_Access := Current_Input;
      File           : File_Type;
      Old_TIB_Count  : constant Integer_32  := TIB_Count.all;
      Old_IN_Ptr     : constant Integer_32  := IN_Ptr.all;
      Old_TIB        : constant Byte_Array  :=
        Memory (TIB .. TIB + Old_TIB_Count - 1);
      Old_Use_RL     : constant Boolean     := Use_RL;
   begin
      begin
         Open (File, In_File, File_Name);
      exception
         when Name_Error =>
            Put_Line ("*** File not found: " & File_Name);
            raise Already_Handled;
      end;
      Set_Input (File);
      Use_RL := False;
      begin
         Main_Loop;
      exception
         when End_Error =>
            Close (File);
            Set_Input (Previous_Input.all);
            Memory (TIB .. TIB + Old_TIB_Count - 1) := Old_TIB;
            TIB_Count.all                           := Old_TIB_Count;
            IN_Ptr.all                              := Old_IN_Ptr;
            Use_RL                                  := Old_Use_RL;
         when others =>
            Close (File);
            Set_Input (Previous_Input.all);
            Use_RL := Old_Use_RL;
            raise;
      end;
   end Include_File;

   ---------------
   -- Interpret --
   ---------------

   procedure Interpret is
   begin
      loop
         declare
            W : constant String := Word;
            A : Action_Type;
            I : Integer_32;
         begin
            if W'Length = 0 then
               exit;
            end if;
            if State.all = 0 then
               begin
                  A := Find (Dict, W);
                  A.Immediate := True;
                  Execute_Action (A);
               exception
                  when Not_Found =>
                     begin
                        I := Integer_32'Value (W);
                     exception
                        when Constraint_Error =>
                           Raise_Exception (Not_Found'Identity, W);
                     end;
                     Push (I);
               end;
            else
               begin
                  A := Find (Dict, W);
                  if A.Immediate then
                     Execute_Action (A);
                  else
                     Add_To_Compilation_Buffer (A);
                  end if;
               exception
                  when Not_Found =>
                     begin
                        I := Integer_32'Value (W);
                     exception
                        when Constraint_Error =>
                           Raise_Exception (Not_Found'Identity, W);
                     end;
                     Add_To_Compilation_Buffer (I);
               end;
            end if;
         end;
      end loop;
   end Interpret;

   --------------------
   -- Interpret_Line --
   --------------------

   procedure Interpret_Line (Line : in String) is
   begin
      Refill_Line (Line);
      Interpret;
   end Interpret_Line;

   --------------------
   -- Interpret_Mode --
   --------------------

   procedure Interpret_Mode is
   begin
      State.all := 0;
   end Interpret_Mode;

   ----------
   -- Jump --
   ----------

   procedure Jump is
   begin
      Current_IP := Pop;
   end Jump;

   -------------------
   -- Jump_If_False --
   -------------------

   procedure Jump_If_False is
      Target : constant Integer_32 := Pop;
   begin
      if Pop = 0 then
         Current_IP := Target;
      end if;
   end Jump_If_False;

   ---------
   -- Key --
   ---------

   procedure Key is
      C : Character;
   begin
      Get_Immediate (C);
      Push (Integer_32 (Character'Pos (C)));
   end Key;

   -------------
   -- Literal --
   -------------

   procedure Literal is
   begin
      Add_To_Compilation_Buffer (Pop);
   end Literal;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop is
   begin
      loop
         Refill;
         Interpret;
      end loop;
   end Main_Loop;

   --------------------------------
   -- Make_And_Remember_Variable --
   --------------------------------

   procedure Make_And_Remember_Variable
     (Name          : in String;
      Var           : out Integer_32_Access;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0)
   is
   begin
      Make_Variable (Name, Size, Initial_Value);
      Remember_Variable (Name, Var);
   end Make_And_Remember_Variable;

   --------------------------------
   -- Make_And_Remember_Variable --
   --------------------------------

   procedure Make_And_Remember_Variable
     (Name          : in String;
      Var           : out Integer_32;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0)
   is
   begin
      Make_Variable (Name, Size, Initial_Value);
      Remember_Variable (Name, Var);
   end Make_And_Remember_Variable;

   -------------------
   -- Make_Variable --
   -------------------

   procedure Make_Variable
     (Name          : in String;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0)
   is
   begin
      if Size = 4 then
         Align;
         Store (Here.all, Initial_Value);
      elsif Initial_Value /= 0 then
         raise Program_Error;
      end if;
      Start_Definition (Name);
      Add_To_Compilation_Buffer (Here.all);
      Semicolon;
      Here.all := Here.all + Size;
   end Make_Variable;

   -----------
   -- Minus --
   -----------

   procedure Minus is
      A : constant Integer_32 := Pop;
   begin
      Push (Pop - A);
   end Minus;

   ----------------
   -- Minusstore --
   ----------------

   procedure Minusstore is
      Addr : constant Integer_32 := Pop;
      Cst  : constant Integer_32 := Pop;
   begin
      Store (Addr, Fetch (Addr) - Cst);
   end Minusstore;

   --------
   -- Ms --
   --------

   procedure Ms is
   begin
      delay until Clock + To_Time_Span (Duration (Float (Pop) / 1000.0));
   end Ms;

   --------------
   -- Notequal --
   --------------

   procedure Notequal is
   begin
      Push (Pop /= Pop);
   end Notequal;

   --------------
   -- Oneminus --
   --------------

   procedure Oneminus is
   begin
      Push (Pop - 1);
   end Oneminus;

   -------------
   -- Oneplus --
   -------------

   procedure Oneplus is
   begin
      Push (Pop + 1);
   end Oneplus;

   ----------
   -- Over --
   ----------

   procedure Over is
   begin
      Push (1);
      Pick;
   end Over;

   -----------
   -- Parse --
   -----------

   procedure Parse
   is
      Char : constant Unsigned_8 := Unsigned_8 (Pop);
   begin
      Push (TIB + IN_Ptr.all);
      for I in IN_Ptr.all .. TIB_Count.all loop
         if Memory (TIB + I) = Char then
            Push (I - IN_Ptr.all);
            IN_Ptr.all := I + 1;
            return;
         end if;
      end loop;
      Push (TIB_Count.all - IN_Ptr.all);
      IN_Ptr.all := TIB_Count.all;
   end Parse;

   ----------------
   -- Patch_Jump --
   ----------------

   procedure Patch_Jump (To_Patch : in Integer_32; Target : in Integer_32) is
   begin
      Compilation_Buffer (To_Patch) .Value := Target;
   end Patch_Jump;

   ----------
   -- Pick --
   ----------

   procedure Pick is
      How_Deep : constant Integer := Integer (Pop);
   begin
      Push (Data_Stack.Data (Data_Stack.Top - How_Deep));
   end Pick;

   ----------
   -- Plus --
   ----------

   procedure Plus is
   begin
      Push (Pop + Pop);
   end Plus;

   ---------
   -- Pop --
   ---------

   function Pop (S : access Stack_Type) return Integer_32 is
   begin
      if S.Top = 0 then
         raise Stack_Underflow;
      end if;
      S.Top := S.Top - 1;
      return S.Data (S.Top + 1);
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop return Integer_32 is
   begin
      return Pop (Data_Stack);
   end Pop;

   ------------------
   -- Pop_Unsigned --
   ------------------

   function Pop_Unsigned return Unsigned_32 is
   begin
      return To_Unsigned_32 (Pop);
   end Pop_Unsigned;

   --------------
   -- Postpone --
   --------------

   procedure Postpone is
      W      : constant String := Word;
      Action : Action_Type;
   begin
      Action := Find (Dict, W);
      if Action.Immediate then
         Add_To_Compilation_Buffer (Action);
      else
         Add_To_Compilation_Buffer (Action.Forth_Proc);
         Add_To_Compilation_Buffer (Compile_Comma'Access);
      end if;
   exception
      when Not_Found =>
         begin
            Add_To_Compilation_Buffer (Integer_32'Value (W));
         exception
            when Constraint_Error =>
               Raise_Exception (Not_Found'Identity, W);
         end;
   end Postpone;

   ----------
   -- Push --
   ----------

   procedure Push (S : access Stack_Type; X : in Integer_32) is
   begin
      if S.Top = S.Data'Last then
         raise Stack_Overflow;
      end if;
      S.Top          := S.Top + 1;
      S.Data (S.Top) := X;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (X : in Integer_32) is
   begin
      Push (Data_Stack, X);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (B : in Boolean) is
   begin
      if B then
         Push (-1);
      else
         Push (0);
      end if;
   end Push;

   -------------------
   -- Push_Unsigned --
   -------------------

   procedure Push_Unsigned (X : in Unsigned_32) is
   begin
      Push (To_Integer_32 (X));
   end Push_Unsigned;

   ----------
   -- Quit --
   ----------

   procedure Quit is
   begin
      loop
         Data_Stack.Top := 0;
         Return_Stack.Top := 0;
         Interpret_Mode;
         begin
            Main_Loop;
         exception
            when End_Error =>
               return;
            when NF : Not_Found =>
               Put_Line ("*** Word not found: " & Exception_Message (NF));
            when Stack_Overflow =>
               Put_Line ("*** Stack overflow");
            when Stack_Underflow =>
               Put_Line ("*** Stack underflow");
            when Already_Handled =>
               null;
            when E : others =>
               Put_Line ("*** Exception " & Exception_Name (E) &
                         " with message " &
                         Exception_Message (E));
         end;
      end loop;
   end Quit;

   ----------
   -- R_At --
   ----------

   procedure R_At is
   begin
      Push (Return_Stack.Data (Return_Stack.Top));
   end R_At;

   -------------
   -- Recurse --
   -------------

   procedure Recurse is
      A : Action_Type := Current_Action;
   begin
      A.Immediate := True;
      Add_To_Compilation_Buffer (A);
   end Recurse;

   ------------
   -- Refill --
   ------------

   procedure Refill is
   begin
      if Use_RL then
         if State.all = 0 then
            Cr;
            Refill_Line (Read_Line ("ok> "));
         else
            Refill_Line (Read_Line ("] "));
         end if;
      else
         declare
            Buffer : String (1 .. 1024);
            Last   : Natural;
         begin
            Get_Line (Buffer, Last);
            Refill_Line (Buffer (1 .. Last));
         end;
      end if;
   end Refill;

   -----------------
   -- Refill_Line --
   -----------------

   procedure Refill_Line (Buffer : in String) is
      Last : constant Natural := Natural'Min (Buffer'Length, 1024);
   begin
      for I in 1 .. Integer'Min (Buffer'Length, 1024) loop
         Memory (TIB + Integer_32 (I) - 1) := Character'Pos (Buffer (I));
      end loop;
      TIB_Count.all := Integer_32 (Last);
      IN_Ptr.all := 0;
   end Refill_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : in String;
      Action : in Action_Type)
   is
      Old_Dict : Dictionary_Access := Dict;
   begin
      Dict := new Dictionary_Array (Dict'First .. Dict'Last + 1);
      Dict (Old_Dict'First .. Old_Dict'Last) := Old_Dict.all;
      Free (Old_Dict);
      Dict (Dict'Last) := (Name   => new String'(Name),
                           Action => Action);
   end Register;

   -----------------------
   -- Register_Ada_Word --
   -----------------------

   procedure Register_Ada_Word
     (Name      : in String;
      Word      : in Ada_Word_Access;
      Immediate : in Boolean := False)
   is
   begin
      --  Create a Forth wrapper around an Ada word so that its address
      --  can be taken and passed to EXECUTE.

      Start_Definition (Name);
      Add_To_Compilation_Buffer (Word);
      Semicolon;
      if Immediate then
         Set_Immediate;
      end if;
   end Register_Ada_Word;

   -----------------------
   -- Register_Constant --
   -----------------------

   procedure Register_Constant
     (Name  : in String;
      Value : in Integer_32)
   is
   begin
      Start_Definition (Name);
      Add_To_Compilation_Buffer (Value);
      Semicolon;
   end Register_Constant;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32_Access)
   is
   begin
      Tick (Name);
      To_Body;
      pragma Warnings (Off);
      Var := To_Integer_32_Access (Memory (Pop) 'Access);
      pragma Warnings (On);
   end Remember_Variable;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32)
   is
   begin
      Tick (Name);
      To_Body;
      Var := Pop;
   end Remember_Variable;

   ------------
   -- Repeat --
   ------------

   procedure Repeat is
   begin
      Literal;
      Add_To_Compilation_Buffer (Jump'Access);
      loop
         declare
            To_Fix : constant Integer_32 := Pop;
         begin
            exit when To_Fix = -1;
            Patch_Jump (To_Fix, Compilation_Index);
         end;
      end loop;
   end Repeat;

   ----------
   -- Roll --
   ----------

   procedure Roll is
      Index    : constant Integer    := Data_Stack.Top - Integer (Pop);
      Moved    : constant Integer_32 := Data_Stack.Data (Index);
   begin
      Data_Stack.Data (Index .. Data_Stack.Top - 1) :=
        Data_Stack.Data (Index + 1 .. Data_Stack.Top);
      Data_Stack.Data (Data_Stack.Top) := Moved;
   end Roll;

   -----------
   -- Scale --
   -----------

   procedure Scale is
      C : constant Integer_64 := Integer_64 (Pop);
      B : constant Integer_64 := Integer_64 (Pop);
      A : constant Integer_64 := Integer_64 (Pop);
   begin
      Push (Integer_32 (A * B / C));
   end Scale;

   --------------
   -- ScaleMod --
   --------------

   procedure ScaleMod is
      C : constant Integer_64 := Integer_64 (Pop);
      B : constant Integer_64 := Integer_64 (Pop);
      A : constant Integer_64 := Integer_64 (Pop);
   begin
      Push (Integer_32 ((A * B) mod C));
      Push (Integer_32 (A * B / C));
   end ScaleMod;

   ---------------
   -- Semicolon --
   ---------------

   procedure Semicolon is
   begin
      Add_To_Compilation_Buffer (Forth_Exit);

      --  Current_Name can be null during definition or completion of
      --  a DOES> prefix.

      if Current_Name /= null then
         Register (Current_Name.all, Current_Action);
         Free (Current_Name);
      end if;

      Interpret_Mode;
   end Semicolon;

   -------------------
   -- Set_Immediate --
   -------------------

   procedure Set_Immediate is
   begin
      Set_Last_Immediate (Dict);
   end Set_Immediate;

   ------------------------
   -- Set_Last_Immediate --
   ------------------------

   procedure Set_Last_Immediate (Dict : in Dictionary_Access) is
   begin
      Dict (Dict'Last) .Action.Immediate := True;
   end Set_Last_Immediate;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks is
   begin
      for A in IN_Ptr.all .. TIB_Count.all - 1 loop
         declare
            C : constant Character := Character'Val (Cfetch (TIB + A));
         begin
            if C /= Character'Val (32) and then C /= Character'Val (9) then
               IN_Ptr.all := A;
               return;
            end if;
         end;
      end loop;
      IN_Ptr.all := TIB_Count.all;
   end Skip_Blanks;

   -------------
   -- Smaller --
   -------------

   procedure Smaller is
      B : constant Integer_32 := Pop;
   begin
      Push (Pop < B);
   end Smaller;

   ------------------
   -- Smallerequal --
   ------------------

   procedure Smallerequal is
      B : constant Integer_32 := Pop;
   begin
      Push (Pop <= B);
   end Smallerequal;

   -----------
   -- Space --
   -----------

   procedure Space is
   begin
      Bl;
      Emit;
   end Space;

   ------------
   -- Squote --
   ------------

   procedure Squote is
      Length : Integer_32;
      Addr   : Integer_32;
   begin
      Check_Compile_Only;
      Push (Character'Pos ('"'));
      Parse;
      Length := Pop;
      Addr := Pop;
      Add_To_Compilation_Buffer (Here.all);
      Add_To_Compilation_Buffer (Addr);
      for I in 1 .. Length loop
         Push (Integer_32 (Memory (Addr)));
         Ccomma;
         Addr := Addr + 1;
      end loop;
   end Squote;

   ----------------------
   -- Start_Definition --
   ----------------------

   procedure Start_Definition (Name : in String) is
   begin
      if Name /= "" then
         Current_Name              := new String'(Name);
      end if;
      Current_Action.Immediate  := False;
      Current_Action.Forth_Proc := Compilation_Index;
      Compile_Mode;
   end Start_Definition;

   -----------
   -- Store --
   -----------

   procedure Store
   is
      pragma Warnings (Off);
      Addr  : constant Integer_32_Access :=
        To_Integer_32_Access (Memory (Pop)'Access);
      pragma Warnings (On);
   begin
      Addr.all := Pop;
   end Store;

   -----------
   -- Store --
   -----------

   procedure Store (Addr : in Integer_32; Value : in Integer_32) is
   begin
      Push (Value);
      Push (Addr);
      Store;
   end Store;

   ----------
   -- Swap --
   ----------

   procedure Swap
   is
      A : constant Integer_32 := Pop;
      B : constant Integer_32 := Pop;
   begin
      Push (A);
      Push (B);
   end Swap;

   ----------
   -- Tick --
   ----------

   procedure Tick (Name : in String) is
      A : constant Action_Type := Find (Dict, Name);
   begin
      Push (A.Forth_Proc);
   end Tick;

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      Tick (Word);
   end Tick;

   -----------
   -- Times --
   -----------

   procedure Times is
   begin
      Push (Pop * Pop);
   end Times;

   -------------
   -- To_Body --
   -------------

   procedure To_Body is
   begin
      Push (Compilation_Buffer (Pop) .Value);
   end To_Body;

   ----------
   -- To_R --
   ----------

   procedure To_R is
   begin
      Push (Return_Stack, Pop);
   end To_R;

   ---------------
   -- To_String --
   ---------------

   function To_String return String is
      Length : constant Natural    := Natural (Pop);
      Addr   : Integer_32          := Pop;
      Result : String (1 .. Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Cfetch (Addr));
         Addr := Addr + 1;
      end loop;
      return Result;
   end To_String;

   -------------
   -- Two_Dup --
   -------------

   procedure Two_Dup is
      A : constant Integer_32 := Pop;
      B : constant Integer_32 := Pop;
   begin
      Push (B);
      Push (A);
      Push (B);
      Push (A);
   end Two_Dup;

   ----------------
   -- Two_From_R --
   ----------------

   procedure Two_From_R is
   begin
      From_R;
      From_R;
      Swap;
   end Two_From_R;

   --------------
   -- Two_R_At --
   --------------

   procedure Two_R_At is
   begin
      Push (Return_Stack.Data (Return_Stack.Top - 1));
      Push (Return_Stack.Data (Return_Stack.Top));
      Return_Stack.Top := Return_Stack.Top - 2;
   end Two_R_At;

   --------------
   -- Two_To_R --
   --------------

   procedure Two_To_R is
   begin
      Swap;
      To_R;
      To_R;
   end Two_To_R;

   ------------
   -- Unused --
   ------------

   procedure Unused is
   begin
      Push (Memory'Last - Here.all + 1);
   end Unused;

   ----------
   -- Word --
   ----------

   procedure Word is
   begin
      Skip_Blanks;
      Push (TIB + IN_Ptr.all);
      for A in IN_Ptr.all .. TIB_Count.all - 1 loop
         declare
            C : constant Character := Character'Val (Cfetch (TIB + A));
         begin
            if C = Character'Val (32) or else C = Character'Val (9) then
               Push (A - IN_Ptr.all);
               IN_Ptr.all := A + 1;
               return;
            end if;
         end;
      end loop;
      Push (TIB_Count.all - IN_Ptr.all);
      IN_Ptr.all := TIB_Count.all;
   end Word;

   ----------
   -- Word --
   ----------

   function Word return String is
   begin
      Word;
      return To_String;
   end Word;

begin
   Data_Stack   := new Stack_Type;
   Return_Stack := new Stack_Type;
   Dict         := new Dictionary_Array (1 .. 0);

   --  Store and register HERE at position 0 -- bootstrap STATE at position 4
   pragma Warnings (Off);
   State := To_Integer_32_Access (Memory (4)'Access);
   pragma Warnings (On);
   Store (0, 4);
   Start_Definition ("HERE");
   Add_To_Compilation_Buffer (0);
   Semicolon;
   Remember_Variable ("HERE", Here);
   Make_And_Remember_Variable ("STATE", State);

   --  Default existing variables
   Make_And_Remember_Variable ("BASE", Base, Initial_Value => 10);
   Make_And_Remember_Variable ("TIB", TIB, Size => 1024);
   Make_And_Remember_Variable ("TIB#", TIB_Count);
   Make_And_Remember_Variable (">IN", IN_Ptr);

   --  Default Ada words
   Register_Ada_Word ("AGAIN", Again'Access, Immediate => True);
   Register_Ada_Word ("AHEAD", Ahead'Access, Immediate => True);
   Register_Ada_Word ("ALIGN", Align'Access);
   Register_Ada_Word ("BL", Bl'Access);
   Register_Ada_Word ("BOUNDS", Bounds'Access);
   Register_Ada_Word ("CHAR", Char'Access);
   Register_Ada_Word ("C@", Cfetch'Access);
   Register_Ada_Word ("COMPILE,", Compile_Comma'Access);
   Register_Ada_Word ("C""", Cquote'Access, Immediate => True);
   Register_Ada_Word ("C!", Cstore'Access);
   Register_Ada_Word (":", Colon'Access);
   Register_Ada_Word (":NONAME", Colon_Noname'Access);
   Register_Ada_Word ("]", Compile_Mode'Access);
   Register_Ada_Word ("C,", Ccomma'Access);
   Register_Ada_Word (",", Comma'Access);
   Register_Ada_Word ("CR", Cr'Access);
   Register_Ada_Word ("CREATE", Create'Access);
   Register_Ada_Word ("/", Div'Access);
   Register_Ada_Word ("/MOD", DivMod'Access);
   Register_Ada_Word ("DOES>", Does'Access, Immediate => True);
   Register_Ada_Word ("DROP", Drop'Access);
   Register_Ada_Word ("DUP", Dup'Access);
   Register_Ada_Word ("DEPTH", Depth'Access);
   Register_Ada_Word (".", Dot'Access);
   Register_Ada_Word (".""", DotQuote'Access, Immediate => True);
   Register_Ada_Word (".S", DotS'Access);
   Register_Ada_Word ("EMIT", Emit'Access);
   Register_Ada_Word ("=", Equal'Access);
   Register_Ada_Word ("EXECUTE", Execute'Access);
   Register_Ada_Word ("@", Fetch'Access);
   Register_Ada_Word ("AND", Forth_And'Access);
   Register_Ada_Word ("BEGIN", Forth_Begin'Access, Immediate => True);
   Register_Ada_Word ("ELSE", Forth_Else'Access, Immediate => True);
   Register_Ada_Word ("[CHAR]", Ichar'Access, Immediate => True);
   Register_Ada_Word ("IF", Forth_If'Access, Immediate => True);
   Register_Ada_Word ("MOD", Forth_Mod'Access);
   Register_Ada_Word ("OR", Forth_Or'Access);
   Register_Ada_Word ("THEN", Forth_Then'Access, Immediate => True);
   Register_Ada_Word ("TYPE", Forth_Type'Access);
   Register_Ada_Word ("UNTIL", Forth_Until'Access, Immediate => True);
   Register_Ada_Word ("WHILE", Forth_While'Access, Immediate => True);
   Register_Ada_Word ("XOR", Forth_Xor'Access);
   Register_Ada_Word ("R>", From_R'Access);
   Register_Ada_Word (">", Greater'Access);
   Register_Ada_Word (">=", Greaterequal'Access);
   Register_Ada_Word ("INCLUDE", Include'Access);
   Register_Ada_Word ("[", Interpret_Mode'Access, Immediate => True);
   Register_Ada_Word ("LITERAL", Literal'Access, Immediate => True);
   Register_Ada_Word ("KEY", Key'Access);
   Register_Ada_Word ("-", Minus'Access);
   Register_Ada_Word ("-!", Minus'Access);
   Register_Ada_Word ("MS", Ms'Access);
   Register_Ada_Word ("<>", Notequal'Access);
   Register_Ada_Word ("1-", Oneminus'Access);
   Register_Ada_Word ("1+", Oneplus'Access);
   Register_Ada_Word ("OVER", Over'Access);
   Register_Ada_Word ("PARSE", Parse'Access);
   Register_Ada_Word ("PICK", Pick'Access);
   Register_Ada_Word ("+", Plus'Access);
   Register_Ada_Word ("POSTPONE", Postpone'Access, Immediate => True);
   Register_Ada_Word ("QUIT", Quit'Access);
   Register_Ada_Word ("R@", R_At'Access);
   Register_Ada_Word ("RECURSE", Recurse'Access, Immediate => True);
   Register_Ada_Word ("REFILL", Refill'Access);
   Register_Ada_Word ("REPEAT", Repeat'Access, Immediate => True);
   Register_Ada_Word ("ROLL", Roll'Access);
   Register_Ada_Word ("*/", Scale'Access);
   Register_Ada_Word ("*/MOD", ScaleMod'Access);
   Register_Ada_Word (";", Semicolon'Access, Immediate => True);
   Register_Ada_Word ("IMMEDIATE", Set_Immediate'Access);
   Register_Ada_Word ("SKIP-BLANKS", Skip_Blanks'Access);
   Register_Ada_Word ("<", Smaller'Access);
   Register_Ada_Word ("<=", Smallerequal'Access);
   Register_Ada_Word ("SPACE", Space'Access);
   Register_Ada_Word ("S""", Squote'Access, Immediate => True);
   Register_Ada_Word ("SWAP", Swap'Access);
   Register_Ada_Word ("!", Store'Access);
   Register_Ada_Word ("'", Tick'Access);
   Register_Ada_Word ("*", Times'Access);
   Register_Ada_Word (">BODY", To_Body'Access);
   Register_Ada_Word (">R", To_R'Access);
   Register_Ada_Word ("2DUP", Two_Dup'Access);
   Register_Ada_Word ("2R>", Two_From_R'Access);
   Register_Ada_Word ("2R@", Two_R_At'Access);
   Register_Ada_Word ("2>R", Two_To_R'Access);
   Register_Ada_Word ("UNUSED", Unused'Access);
   Register_Ada_Word ("WORD", Word'Access);
end Aforth;
