with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Aforth is

   package Integer_32_IO is new Ada.Text_IO.Integer_IO (Integer_32);
   use Integer_32_IO;

   procedure Free is
      new Ada.Unchecked_Deallocation (Dictionary_Array, Dictionary_Access);

   type Byte_Access is access all Unsigned_8;

   type Integer_32_Access is access all Integer_32;

   function To_Integer_32_Access is
      new Ada.Unchecked_Conversion (Byte_Access, Integer_32_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);

   Here      : Integer_32_Access;
   Base      : Integer_32_Access;
   TIB       : Integer_32;
   TIB_Count : Integer_32_Access;
   IN_Ptr    : Integer_32_Access;
   State     : Integer_32_Access;

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32_Access);

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32);

   procedure Make_And_Remember_Variable
     (Name          : in String;
      Var           : out Integer_32_Access;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0);

   procedure Make_And_Remember_Variable
     (Name          : in String;
      Var           : out Integer_32;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0);

   Current_Name   : String_Access;
   Current_Action : Action_Type (Forth_Word);
   Current_IP     : Integer_32 := -1;

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

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Action : in Action_Type) is
   begin
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

   --------
   -- Bl --
   --------

   procedure Bl is
   begin
      Push (32);
   end Bl;

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

   function Cfetch (Addr : Integer_32) Return Unsigned_8 is
   begin
      Push (Addr);
      Cfetch;
      return Unsigned_8 (Pop);
   end Cfetch;

   ------------------
   -- Compile_Mode --
   ------------------

   procedure Compile_Mode is
   begin
      State.all := 1;
   end Compile_Mode;

   -----------
   -- Colon --
   -----------

   procedure Colon is
   begin
      Start_Definition (Word);
      Compile_Mode;
   end Colon;

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
      To_Integer_32_Access (Memory (Here.all) 'Access) .all := Pop;
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
            if Current_Action.Kind = Forth_Word and then
              Current_Action.Forth_Proc = -1
            then
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
      Addr  : constant Integer_32_Access :=
        To_Integer_32_Access (Memory (Pop)'Access);
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
            return Dict (I) .Action;
         end if;
      end loop;
      Raise_Exception (Not_Found'Identity,
                       Name & " not found");
   end Find;

   -----------------
   -- Forth_Begin --
   -----------------

   procedure Forth_Begin is
   begin
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
      Push (Current_IP);
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

   procedure Forth_Until
   is
      Target : constant Integer_32 := Pop;
   begin
      Literal;
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_Until;

   ---------------
   -- Immediate --
   ---------------

   procedure Immediate is
   begin
      Set_Last_Immediate (Dict);
   end Immediate;

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
         declare
            W : constant String := Word;
            A : Action_Type;
            I : Integer_32;
         begin
            if W'Length = 0 then
               Put_Line ("IN> = " & In_Ptr.all'Img);
               Put_Line ("TIB# = " & TIB_Count.all'Img);
               pragma Assert (IN_Ptr.all >= TIB_Count.all);
               exit;
            end if;
            Put_Line ("Handling `" & W & "'");
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
      Register (Name, (Kind => Number, Immediate => False, Value => Here.all));
      if Size = 4 then
         Store (Here.all ,Initial_Value);
      elsif Initial_Value /= 0 then
         raise Program_Error;
      end if;
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
            In_Ptr.all := I + 1;
            return;
         end if;
      end loop;
      Push (TIB_Count.all - IN_Ptr.all);
      In_Ptr.all := TIB_Count.all;
   end Parse;

   ----------------
   -- Patch_Jump --
   ----------------

   procedure Patch_Jump (To_Patch : in Integer_32; Target : in Integer_32) is
   begin
      Compilation_Buffer (To_Patch) .Value := Target;
   end Patch_Jump;

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

   ------------
   -- Prompt --
   ------------

   procedure Prompt is
   begin
      if State.all = 0 then
         Cr;
         Put ("ok> ");
      else
         Put ("] ");
      end if;
      Flush;
   end Prompt;

   --------------
   -- Postpone --
   --------------

   procedure Postpone is
      W      : constant String := Word;
      Action : Action_Type;
   begin
      Action := Find (Dict, W);
      Add_To_Compilation_Buffer (Action);
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
   -- Quit --
   ----------

   procedure Quit is
   begin
      loop
         Data_Stack.Top := 0;
         Return_Stack.Top := 0;
         Interpret_Mode;
         begin
            loop
               Prompt;
               Refill;
               Main_Loop;
            end loop;
         exception
            when Ada.Io_Exceptions.End_Error =>
               return;
            when NF : Not_Found =>
               Put_Line ("*** Word not found: " & Exception_Message (NF));
            when Stack_Overflow =>
               Put_Line ("*** Stack overflow");
            when Stack_Underflow =>
               Put_Line ("*** Stack underflow");
            when E : others =>
               Put_Line ("*** Exception " & Exception_Name (E) &
                         " with message " &
                         Exception_Message (E));
         end;
      end loop;
   end Quit;

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
      Buffer : String (1 .. 1024);
      Last   : Natural;
   begin
      Get_Line (Buffer, Last);
      for I in 1 .. Last loop
         Memory (TIB + Integer_32 (I) - 1) := Character'Pos (Buffer (I));
      end loop;
      TIB_Count.all := Integer_32 (Last);
      IN_Ptr.all := 0;
   end Refill;

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
      Register (Name,
                (Kind => Ada_Word, Ada_Proc => Word, Immediate => Immediate));
   end Register_Ada_Word;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32_Access)
   is
   begin
      Var := To_Integer_32_Access (Memory (Find (Dict, Name) .Value) 'Access);
   end Remember_Variable;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (Name : in String;
      Var  : out Integer_32)
   is
   begin
      Var := Find (Dict, Name) .Value;
   end Remember_Variable;

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

   ---------------
   -- Semicolon --
   ---------------

   procedure Semicolon is
   begin
      Add_To_Compilation_Buffer
        (Action_Type'(Kind       => Forth_Word,
                      Immediate  => True,
                      Forth_Proc => -1));
      Register (Current_Name.all, Current_Action);
      Free (Current_Name);
      Interpret_Mode;
   end Semicolon;

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
      B : constant Integer_32 := TIB + IN_Ptr.all;
   begin
      for A in IN_Ptr.all .. TIB_Count.all - 1 loop
         declare
            C : constant Character := Character'Val (Cfetch (B + A));
         begin
            if C /= Character'Val (32) and then C /= Character'Val (9) then
               IN_Ptr.all := A;
               return;
            end if;
         end;
      end loop;
      IN_Ptr.all := TIB_Count.all;
   end Skip_Blanks;

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
   begin
      Push (Character'Pos ('"'));
      Parse;
      if State.all = 1 then
         declare
            Length : constant Integer_32 := Pop;
            Addr   :          Integer_32 := Pop;
         begin
            Add_To_Compilation_Buffer (Here.all);
            Add_To_Compilation_Buffer (Addr);
            for I in 1 .. Length loop
               Push (Integer_32 (Memory (Addr)));
               Ccomma;
               Addr := Addr + 1;
            end loop;
         end;
      end if;
   end Squote;

   ----------------------
   -- Start_Definition --
   ----------------------

   procedure Start_Definition (Name : in String) is
   begin
      Current_Name              := new String'(Name);
      Current_Action.Immediate  := False;
      Current_Action.Forth_Proc := Compilation_Index;
      State.all                 := 1;
   end Start_Definition;

   -----------
   -- Store --
   -----------

   procedure Store
   is
      Addr  : constant Integer_32_Access :=
        To_Integer_32_Access (Memory (Pop)'Access);
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

   -----------
   -- Times --
   -----------

   procedure Times is
   begin
      Push (Pop * Pop);
   end Times;

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
               In_Ptr.all := A + 1;
               return;
            end if;
         end;
      end loop;
      Push (TIB_Count.all - IN_Ptr.all);
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

   --  Store and register here
   Store (0, 4);
   Register ("HERE", (Kind => Number, Immediate => False, Value => 0));
   Remember_Variable ("HERE", Here);

   --  Default existing variables
   Make_And_Remember_Variable ("BASE", Base, Initial_Value => 10);
   Make_And_Remember_Variable ("TIB", TIB, Size => 1024);
   Make_And_Remember_Variable ("TIB#", TIB_Count);
   Make_And_Remember_Variable (">IN", IN_Ptr);
   Make_And_Remember_Variable ("STATE", State);

   --  Default Ada words
   Register_Ada_Word ("AGAIN", Again'Access, Immediate => True);
   Register_Ada_Word ("AHEAD", Ahead'Access, Immediate => True);
   Register_Ada_Word ("BL", Bl'Access);
   Register_Ada_Word ("C@", Cfetch'Access);
   Register_Ada_Word (":", Colon'Access);
   Register_Ada_Word ("]", Compile_Mode'Access);
   Register_Ada_Word ("c,", Ccomma'Access);
   Register_Ada_Word (",", Comma'Access);
   Register_Ada_Word ("CR", Cr'Access);
   Register_Ada_Word ("/", Div'Access);
   Register_Ada_Word ("/MOD", DivMod'Access);
   Register_Ada_Word ("DROP", Drop'Access);
   Register_Ada_Word ("DUP", Dup'Access);
   Register_Ada_Word ("DEPTH", Depth'Access);
   Register_Ada_Word (".", Dot'Access);
   Register_Ada_Word (".""", DotQuote'Access, Immediate => True);
   Register_Ada_Word (".S", DotS'Access);
   Register_Ada_Word ("EMIT", Emit'Access);
   Register_Ada_Word ("@", Fetch'Access);
   Register_Ada_Word ("begin", Forth_Begin'Access, Immediate => True);
   Register_Ada_Word ("else", Forth_Else'Access, Immediate => True);
   Register_Ada_Word ("if", Forth_If'Access, Immediate => True);
   Register_Ada_Word ("mod", Forth_Mod'Access);
   Register_Ada_Word ("then", Forth_Then'Access, Immediate => True);
   Register_Ada_Word ("type", Forth_Type'Access);
   Register_Ada_Word ("until", Forth_Until'Access, Immediate => True);
   Register_Ada_Word ("IMMEDIATE", Immediate'Access);
   Register_Ada_Word ("[", Interpret_Mode'Access, Immediate => True);
   Register_Ada_Word ("LITERAL", Literal'Access, Immediate => True);
   Register_Ada_Word ("-", Minus'Access);
   Register_Ada_Word ("+", Plus'Access);
   Register_Ada_Word ("PARSE", Parse'Access);
   Register_Ada_Word ("POSTPONE", Postpone'Access, Immediate => True);
   Register_Ada_Word ("PROMPT", Prompt'Access);
   Register_Ada_Word ("QUIT", Quit'Access);
   Register_Ada_Word ("RECURSE", Recurse'Access, Immediate => True);
   Register_Ada_Word ("REFILL", Refill'Access);
   Register_Ada_Word ("*/", Scale'Access);
   Register_Ada_Word (";", Semicolon'Access, Immediate => True);
   Register_Ada_Word ("SKIP-BLANKS", Skip_Blanks'Access);
   Register_Ada_Word ("SPACE", Space'Access);
   Register_Ada_Word ("s""", Squote'Access, Immediate => True);
   Register_Ada_Word ("SWAP", Swap'Access);
   Register_Ada_Word ("!", Store'Access);
   Register_Ada_Word ("*", Times'Access);
   Register_Ada_Word ("WORD", Word'Access);
end Aforth;
