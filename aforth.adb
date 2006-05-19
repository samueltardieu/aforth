with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Read_Line;

package body Aforth is

   package Integer_32_IO is new Ada.Text_IO.Integer_IO (Integer_32);
   use Integer_32_IO;

   procedure Free is
      new Ada.Unchecked_Deallocation (Dictionary_Array, Dictionary_Access);

   type Byte_Access is access all Unsigned_8;

   type Integer_32_Access is access all Integer_32;

   pragma Warnings (Off);
   function To_Integer_32_Access is
      new Ada.Unchecked_Conversion (Byte_Access, Integer_32_Access);
   pragma Warnings (On);

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);

   Already_Handled : exception;

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

   procedure Interpret;
   procedure Main_Loop (Display_Prompt : Boolean := False);

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

   ---------------
   -- Immediate --
   ---------------

   procedure Immediate is
   begin
      Set_Last_Immediate (Dict);
   end Immediate;

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
   begin
      begin
         Open (File, In_File, File_Name);
      exception
         when Name_Error =>
            Put_Line ("*** File not found: " & File_Name);
            raise Already_Handled;
      end;
      Set_Input (File);
      begin
         Main_Loop;
      exception
         when End_Error =>
            Close (File);
            Set_Input (Previous_Input.all);
            Memory (TIB .. TIB + Old_TIB_Count - 1) := Old_TIB;
            TIB_Count.all := Old_TIB_Count;
            IN_Ptr.all := Old_IN_Ptr;
         when others =>
            Close (File);
            Set_Input (Previous_Input.all);
            raise;
      end;
   end Include_File;

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

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (Display_Prompt : Boolean := False) is
   begin
      loop
         if Display_Prompt then
            if State.all = 0 then
               New_Line;
               Put ("ok> ");
               Flush;
            else
               Put ("] ");
               Flush;
            end if;
         end if;
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
         Store (Here.all ,Initial_Value);
      elsif Initial_Value /= 0 then
         raise Program_Error;
      end if;
      Register (Name, (Kind => Number, Immediate => False, Value => Here.all));
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

   ---------
   -- Nip --
   ---------

   procedure Nip is
      A : constant Integer_32 := Pop;
   begin
      Drop;
      Push (A);
   end Nip;

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
      A : constant Integer_32 := Pop;
      B : constant Integer_32 := Pop;
   begin
      Push (B);
      Push (A);
      Push (B);
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
   -- Plus --
   ----------

   procedure Plus is
   begin
      Push (Pop + Pop);
   end Plus;

   ---------------
   -- Plusstore --
   ---------------

   procedure Plusstore is
      Addr : constant Integer_32 := Pop;
      Cst  : constant Integer_32 := Pop;
   begin
      Store (Addr, Fetch (Addr) + Cst);
   end Plusstore;

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
            Main_Loop (Display_Prompt => True);
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
      Buffer : constant String  := Read_Line;
      Last   : constant Natural := Natural'Min (Buffer'Length, 1024);
   begin
      for I in 1 .. Integer'Min (Buffer'Length, 1024) loop
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
      pragma Warnings (Off);
      Var := To_Integer_32_Access (Memory (Find (Dict, Name) .Value) 'Access);
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
      Var := Find (Dict, Name) .Value;
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

   -------------
   -- Twodrop --
   -------------

   procedure Twodrop is
   begin
      Drop;
      Drop;
   end Twodrop;

   ------------
   -- Twodup --
   ------------

   procedure Twodup is
      A : constant Integer_32 := Pop;
      B : constant Integer_32 := Pop;
   begin
      Push (B);
      Push (A);
      Push (B);
      Push (A);
   end Twodup;

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

   ---------------
   -- Zeroequal --
   ---------------

   procedure Zeroequal is
   begin
      Push (Pop = 0);
   end Zeroequal;

   -----------------
   -- Zerogreater --
   -----------------

   procedure Zerogreater is
   begin
      Push (Pop > 0);
   end Zerogreater;

   ----------------------
   -- Zerogreaterequal --
   ----------------------

   procedure Zerogreaterequal is
   begin
      Push (Pop >= 0);
   end Zerogreaterequal;

   ------------------
   -- Zeronotequal --
   ------------------

   procedure Zeronotequal is
   begin
      Push (Pop /= 0);
   end Zeronotequal;

   -----------------
   -- Zerosmaller --
   -----------------

   procedure Zerosmaller is
   begin
      Push (Pop < 0);
   end Zerosmaller;

   ----------------------
   -- Zerosmallerequal --
   ----------------------

   procedure Zerosmallerequal is
   begin
      Push (Pop <= 0);
   end Zerosmallerequal;

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
   Register_Ada_Word ("ALIGN", Align'Access);
   Register_Ada_Word ("BL", Bl'Access);
   Register_Ada_Word ("BOUNDS", Bounds'Access);
   Register_Ada_Word ("CHAR", Char'Access);
   Register_Ada_Word ("C@", Cfetch'Access);
   Register_Ada_Word ("C!", Cstore'Access);
   Register_Ada_Word (":", Colon'Access);
   Register_Ada_Word ("]", Compile_Mode'Access);
   Register_Ada_Word ("C,", Ccomma'Access);
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
   Register_Ada_Word ("=", Equal'Access);
   Register_Ada_Word ("@", Fetch'Access);
   Register_Ada_Word ("BEGIN", Forth_Begin'Access, Immediate => True);
   Register_Ada_Word ("ELSE", Forth_Else'Access, Immediate => True);
   Register_Ada_Word ("[CHAR]", Ichar'Access, Immediate => True);
   Register_Ada_Word ("IF", Forth_If'Access, Immediate => True);
   Register_Ada_Word ("MOD", Forth_Mod'Access);
   Register_Ada_Word ("THEN", Forth_Then'Access, Immediate => True);
   Register_Ada_Word ("TYPE", Forth_Type'Access);
   Register_Ada_Word ("UNTIL", Forth_Until'Access, Immediate => True);
   Register_Ada_Word ("WHILE", Forth_While'Access, Immediate => True);
   Register_Ada_Word (">", Greater'Access);
   Register_Ada_Word (">=", Greaterequal'Access);
   Register_Ada_Word ("IMMEDIATE", Immediate'Access);
   Register_Ada_Word ("INCLUDE", Include'Access);
   Register_Ada_Word ("[", Interpret_Mode'Access, Immediate => True);
   Register_Ada_Word ("LITERAL", Literal'Access, Immediate => True);
   Register_Ada_Word ("-", Minus'Access);
   Register_Ada_Word ("-!", Minus'Access);
   Register_Ada_Word ("NIP", Nip'Access);
   Register_Ada_Word ("<>", Notequal'Access);
   Register_Ada_Word ("1-", Oneminus'Access);
   Register_Ada_Word ("1+", Oneplus'Access);
   Register_Ada_Word ("OVER", Over'Access);
   Register_Ada_Word ("PARSE", Parse'Access);
   Register_Ada_Word ("+", Plus'Access);
   Register_Ada_Word ("+!", Plusstore'Access);
   Register_Ada_Word ("POSTPONE", Postpone'Access, Immediate => True);
   Register_Ada_Word ("PROMPT", Prompt'Access);
   Register_Ada_Word ("QUIT", Quit'Access);
   Register_Ada_Word ("RECURSE", Recurse'Access, Immediate => True);
   Register_Ada_Word ("REFILL", Refill'Access);
   Register_Ada_Word ("REPEAT", Repeat'Access, Immediate => True);
   Register_Ada_Word ("*/", Scale'Access);
   Register_Ada_Word (";", Semicolon'Access, Immediate => True);
   Register_Ada_Word ("SKIP-BLANKS", Skip_Blanks'Access);
   Register_Ada_Word ("<", Smaller'Access);
   Register_Ada_Word ("<=", Smallerequal'Access);
   Register_Ada_Word ("SPACE", Space'Access);
   Register_Ada_Word ("S""", Squote'Access, Immediate => True);
   Register_Ada_Word ("SWAP", Swap'Access);
   Register_Ada_Word ("!", Store'Access);
   Register_Ada_Word ("*", Times'Access);
   Register_Ada_Word ("2DROP", Twodrop'Access);
   Register_Ada_Word ("2DUP", Twodup'Access);
   Register_Ada_Word ("WORD", Word'Access);
   Register_Ada_Word ("0=", Zeroequal'Access);
   Register_Ada_Word ("0>", Zerogreater'Access);
   Register_Ada_Word ("0>=", Zerogreaterequal'Access);
   Register_Ada_Word ("0<>", Zeronotequal'Access);
   Register_Ada_Word ("0<", Zerosmaller'Access);
   Register_Ada_Word ("0<=", Zerosmallerequal'Access);
end Aforth;
