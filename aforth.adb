with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Readline;

package body Aforth is

   --  Notes:
   --    - the compilation stack is the data stack

   type Action_Kind is (Ada_Word, Forth_Word, Number);

   type Action_Type (Kind : Action_Kind := Number) is record
      Immediate : Boolean;
      case Kind is
         when Ada_Word =>
            Ada_Proc   : Ada_Word_Access;
         when Forth_Word =>
            Forth_Proc : Cell;
            Inline     : Boolean;
         when Number =>
            Value      : Cell;
      end case;
   end record;

   procedure Register (Name   : String;
                       Action : Action_Type);

   Not_Found : exception;

   function Find (Name : String) return Action_Type;
   --  May raise Not_Found

   subtype Natural_Cell is Cell range 1 .. Cell'Last;
   package Compilation_Buffers is
      new Ada.Containers.Vectors (Natural_Cell, Action_Type);
   use Compilation_Buffers;

   Compilation_Buffer : Compilation_Buffers.Vector;

   procedure Add_To_Compilation_Buffer (Action : Action_Type);

   package Cell_IO is new Ada.Text_IO.Integer_IO (Cell);
   use Cell_IO;

   type Dictionary_Entry is record
      Name   : Unbounded_String;
      Action : Action_Type;
   end record;

   package Dictionaries is
     new Ada.Containers.Vectors (Positive, Dictionary_Entry);
   use Dictionaries;

   Dict : Dictionaries.Vector;

   type Byte_Array is array (Cell range <>) of aliased Unsigned_8;

   Memory : Byte_Array (0 .. 65535) := (others => 0);

   type Byte_Access is access all Unsigned_8;

   pragma Warnings (Off);
   function To_Cell_Access is
      new Ada.Unchecked_Conversion (Byte_Access, Cell_Access);
   pragma Warnings (On);

   function To_Unsigned_32 is
      new Ada.Unchecked_Conversion (Cell, Unsigned_32);
   function To_Cell is
      new Ada.Unchecked_Conversion (Unsigned_32, Cell);
   function To_Unsigned_64 is
      new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);
   function To_Integer_64 is
      new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

   package Stacks is
      new Ada.Containers.Vectors (Positive, Cell);
   use Stacks;
   Data_Stack   : aliased Stacks.Vector;
   Return_Stack : aliased Stacks.Vector;

   procedure Push (S : in out Stacks.Vector; X : Cell);
   --  May raise stack overflow

   function Pop (S : not null access Stacks.Vector) return Cell;
   --  May raise stack underflow

   Already_Handled : exception;

   Here      : Cell_Access;
   Base      : Cell_Access;
   TIB       : Cell;
   TIB_Count : Cell_Access;
   IN_Ptr    : Cell_Access;
   State     : Cell_Access;

   Forth_Exit : constant Action_Type := (Kind       => Forth_Word,
                                         Immediate  => True,
                                         Inline     => False,
                                         Forth_Proc => -1);

   Forward_Reference  : constant   := -100;
   Backward_Reference : constant   := -101;
   Do_Loop_Reference  : constant   := -102;
   Definition_Reference : constant := -103;

   procedure Remember_Variable
     (Name : String;
      Var  : out Cell_Access);

   procedure Remember_Variable
     (Name : String;
      Var  : out Cell);

   Current_Name   : Unbounded_String;
   Current_Action : Action_Type (Forth_Word);
   Current_IP     : Cell := -1;

   Use_RL         : Boolean := True;
   --  Should the current input method use Read_Line?

   procedure Start_Definition (Name : String := "");

   function To_String return String;

   procedure Execute_Action (Action : Action_Type);

   procedure Execute_Forth_Word (Addr : Cell);

   procedure Main_Loop;

   function Word return String;

   procedure Jump;
   procedure Jump_If_False;
   procedure Patch_Jump (To_Patch : Cell; Target : Cell);

   procedure Add_To_Compilation_Buffer (Ada_Proc : Ada_Word_Access);
   procedure Add_To_Compilation_Buffer (Value : Cell);

   procedure DoDoes;

   procedure Refill_Line (Buffer : String);

   procedure Check_Compile_Only;

   procedure Tick (Name : String);

   procedure Check_Control_Structure (Reference : Cell);

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Action : Action_Type) is
   begin
      Check_Compile_Only;

      --  Call or inline words

      if Action.Kind = Forth_Word and then Action.Inline then
         declare
            Index : Cell := Action.Forth_Proc;
         begin
            while Element (Compilation_Buffer, Index) /= Forth_Exit loop
               Add_To_Compilation_Buffer (Element (Compilation_Buffer, Index));
               Index := Index + 1;
            end loop;
         end;
      else
         Append (Compilation_Buffer, Action);
      end if;
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Ada_Proc : Ada_Word_Access) is
   begin
      Add_To_Compilation_Buffer
        (Action_Type'(Kind      => Ada_Word,
                      Immediate => True,
                      Ada_Proc  => Ada_Proc));
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Value : Cell) is
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

      --  The compilation stack contains the index of the address to
      --  patch when the AHEAD is resolved by a THEN.

   begin
      Push (Last_Index (Compilation_Buffer) + 1);
      Push (Forward_Reference);
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

   ------------
   -- Cfetch --
   ------------

   procedure Cfetch is
   begin
      Push (Cell (Memory (Pop)));
   end Cfetch;

   ------------
   -- Cfetch --
   ------------

   function Cfetch (Addr : Cell) return Cell is
   begin
      Push (Addr);
      Cfetch;
      return Pop;
   end Cfetch;

   ------------------------
   -- Check_Compile_Only --
   ------------------------

   procedure Check_Compile_Only is
   begin
      if State.all /= 1 then
         raise Compile_Only;
      end if;
   end Check_Compile_Only;

   -----------------------------
   -- Check_Control_Structure --
   -----------------------------

   procedure Check_Control_Structure (Reference : Cell) is
   begin
      if Pop /= Reference then
         raise Unbalanced_Control_Structure;
      end if;
   end Check_Control_Structure;

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
      Push (Last_Index (Compilation_Buffer) + 1);
      Start_Definition;
   end Colon_Noname;

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
      Addr : constant Cell := Pop;
   begin
      Memory (Addr) := Unsigned_8 (Pop);
   end Cstore;

   -----------
   -- Depth --
   -----------

   procedure Depth is
   begin
      Push (Cell (Length (Data_Stack)));
   end Depth;

   ------------
   -- DivMod --
   ------------

   procedure DivMod is
      B : constant Cell := Pop;
      A : constant Cell := Pop;
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

      pragma Assert (Last_Element (Compilation_Buffer) = Forth_Exit);
      Insert (Compilation_Buffer,
              Last_Index (Compilation_Buffer),
              Action_Type'(Kind       => Forth_Word,
                           Immediate  => True,
                           Inline     => False,
                           Forth_Proc => Pop));
   end DoDoes;

   ----------
   -- Does --
   ----------

   procedure Does is

      --  Terminate current word after asking to patch the latest created
      --  one. Compilation buffer after index, call to DoDoes and exit
      --  is Compilation_Index + 3.

      Does_Part : constant Cell := Last_Index (Compilation_Buffer) + 4;
   begin
      Add_To_Compilation_Buffer (Does_Part);
      Add_To_Compilation_Buffer (DoDoes'Access);
      Semicolon;

      --  Start an unnamed word corresponding to the DOES> part

      Start_Definition;
      pragma Assert (Last_Index (Compilation_Buffer) + 1 = Does_Part);
   end Does;

   ---------
   -- Dot --
   ---------

   procedure Dot is
   begin
      Put (Pop, Base => Integer (Base.all), Width => 0);
   end Dot;

   ----------
   -- Drop --
   ----------

   procedure Drop is
      Value : constant Cell := Pop;
      pragma Unreferenced (Value);
   begin
      null;
   end Drop;

   ----------
   -- Emit --
   ----------

   procedure Emit is
   begin
      Put (Character'Val (Pop));
   end Emit;

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

   procedure Execute_Action (Action : Action_Type) is
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

   procedure Execute_Forth_Word (Addr : Cell) is
   begin
      Push (Return_Stack, Current_IP);
      Current_IP := Addr;
      loop
         declare
            Current_Action : constant Action_Type :=
              Element (Compilation_Buffer, Current_IP);
         begin
            Current_IP := Current_IP + 1;
            if Current_Action = Forth_Exit then
               Current_IP := Pop (Return_Stack'Access);
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
      Addr  : constant Cell_Access := To_Cell_Access (Memory (Pop)'Access);
   begin
      Push (Addr.all);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   function Fetch (Addr : Cell) return Cell is
   begin
      Push (Addr);
      Fetch;
      return Pop;
   end Fetch;

   ----------
   -- Find --
   ----------

   function Find (Name : String) return Action_Type
   is
      Lower_Name : constant String := To_Lower (Name);
   begin
      for I in reverse First_Index (Dict) .. Last_Index (Dict) loop
         declare
            Current : Dictionary_Entry renames Element (Dict, I);
         begin
            if To_Lower (To_String (Current.Name)) = Lower_Name then
               pragma Assert (Current.Action.Kind = Forth_Word);
               return Current.Action;
            end if;
         end;
      end loop;
      raise Not_Found with Name;
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
      --    Backward_Reference

   begin
      Push (-1);
      Push (Last_Index (Compilation_Buffer) + 1);
      Push (Backward_Reference);
   end Forth_Begin;

   --------------
   -- Forth_Do --
   --------------

   procedure Forth_Do is

      --  The structure of a DO/?DO - LOOP/+LOOP on the compilation stack
      --  is:
      --    -1
      --    addr of the first ?DO/LEAVE
      --    addr of the second ?DO/LEAVE
      --    ...
      --    addr of the beginning of the loop
      --    Do_Loop_Reference

   begin
      Add_To_Compilation_Buffer (Two_To_R'Access);
      Push (-1);
      Push (Last_Index (Compilation_Buffer) + 1);
      Push (Do_Loop_Reference);
   end Forth_Do;

   --------------
   -- Forth_If --
   --------------

   procedure Forth_If is
   begin
      Push (Last_Index (Compilation_Buffer) + 1);
      Push (Forward_Reference);
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_If;

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
      Check_Control_Structure (Forward_Reference);
      Patch_Jump (To_Patch => Pop,
                  Target   => Last_Index (Compilation_Buffer) + 1);
   end Forth_Then;

   -----------------
   -- Forth_While --
   -----------------

   procedure Forth_While is
   begin
      Check_Control_Structure (Backward_Reference);
      Push (Last_Index (Compilation_Buffer) + 1);
      Swap;
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump_If_False'Access);
      Push (Backward_Reference);
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
      Push (Pop (Return_Stack'Access));
   end From_R;

   ------------------
   -- Greaterequal --
   ------------------

   procedure Greaterequal is
      B : constant Cell := Pop;
   begin
      Push (Pop >= B);
   end Greaterequal;

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

   procedure Include_File (File_Name : String)
   is
      Previous_Input : constant File_Access := Current_Input;
      File           : File_Type;
      Old_TIB_Count  : constant Cell  := TIB_Count.all;
      Old_IN_Ptr     : constant Cell  := IN_Ptr.all;
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
            I : Cell;
         begin
            if W'Length = 0 then
               exit;
            end if;
            if State.all = 0 then
               begin
                  A := Find (W);
                  A.Immediate := True;
                  Execute_Action (A);
               exception
                  when NF : Not_Found =>
                     begin
                        I := Cell'Value (W);
                     exception
                        when Constraint_Error =>
                           Reraise_Occurrence (NF);
                     end;
                     Push (I);
               end;
            else
               begin
                  A := Find (W);
                  if A.Immediate then
                     Execute_Action (A);
                  else
                     Add_To_Compilation_Buffer (A);
                  end if;
               exception
                  when NF : Not_Found =>
                     begin
                        I := Cell'Value (W);
                     exception
                        when Constraint_Error =>
                           Reraise_Occurrence (NF);
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

   procedure Interpret_Line (Line : String) is
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

   -------
   -- J --
   -------

   procedure J is
   begin
      Push (Element (Return_Stack, Last_Index (Return_Stack) - 1));
   end J;

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
      Target : constant Cell := Pop;
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
      Push (Cell (Character'Pos (C)));
   end Key;

   -----------
   -- Leave --
   -----------

   procedure Leave is
   begin
      --  Loop for Do_Loop_Reference on the stack

      for I in reverse First_Index (Data_Stack) .. Last_Index (Data_Stack) loop
         if Element (Data_Stack, I) = Do_Loop_Reference then

            --  Insert the leave information at the proper place

            Insert (Data_Stack, I, Last_Index (Compilation_Buffer) + 1);
            Add_To_Compilation_Buffer (0);
            Add_To_Compilation_Buffer (Jump'Access);
            return;
         end if;
      end loop;

      raise Unbalanced_Control_Structure;
   end Leave;

   -------------
   -- Literal --
   -------------

   procedure Literal is
   begin
      Add_To_Compilation_Buffer (Pop);
   end Literal;

   ------------
   -- Lshift --
   ------------

   procedure Lshift is
      U : constant Natural := Natural (Pop_Unsigned);
   begin
      Push (Pop * 2 ** U);
   end Lshift;

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
     (Name          : String;
      Var           : out Cell_Access;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
   is
   begin
      Make_Variable (Name, Size, Initial_Value);
      Remember_Variable (Name, Var);
   end Make_And_Remember_Variable;

   --------------------------------
   -- Make_And_Remember_Variable --
   --------------------------------

   procedure Make_And_Remember_Variable
     (Name          : String;
      Var           : out Cell;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
   is
   begin
      Make_Variable (Name, Size, Initial_Value);
      Remember_Variable (Name, Var);
   end Make_And_Remember_Variable;

   -------------------
   -- Make_Variable --
   -------------------

   procedure Make_Variable
     (Name          : String;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
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
   -- Mstar --
   -----------

   procedure Mstar is
   begin
      Push_64 (Integer_64 (Pop) * Integer_64 (Pop));
   end Mstar;

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

   procedure Patch_Jump (To_Patch : Cell; Target : Cell) is
      pragma Assert (To_Patch < Last_Index (Compilation_Buffer) + 1);
      pragma Assert (Target <= Last_Index (Compilation_Buffer) + 1);
      Current : Action_Type := Element (Compilation_Buffer, To_Patch);
   begin
      Current.Value := Target;
      Replace_Element (Compilation_Buffer, To_Patch, Current);
   end Patch_Jump;

   ----------
   -- Pick --
   ----------

   procedure Pick is
      How_Deep : constant Integer := Integer (Pop);
   begin
      Push (Element (Data_Stack, Last_Index (Data_Stack) - How_Deep));
   end Pick;

   ----------
   -- Plus --
   ----------

   procedure Plus is
   begin
      Push (Pop + Pop);
   end Plus;

   ---------------
   -- Plus_Loop --
   ---------------

   procedure Plus_Loop is
      To_Patch : Cell;
   begin
      Check_Control_Structure (Do_Loop_Reference);

      Add_To_Compilation_Buffer (From_R'Access);
      Add_To_Compilation_Buffer (Plus'Access);
      Add_To_Compilation_Buffer (From_R'Access);
      Add_To_Compilation_Buffer (Two_Dup'Access);
      Add_To_Compilation_Buffer (To_R'Access);
      Add_To_Compilation_Buffer (To_R'Access);
      Add_To_Compilation_Buffer (Greaterequal'Access);
      Add_To_Compilation_Buffer (Pop);
      Add_To_Compilation_Buffer (Jump_If_False'Access);

      --  Resolve forward references

      loop
         To_Patch := Pop;
         exit when To_Patch = -1;
         Patch_Jump (To_Patch => To_Patch,
                     Target   => Last_Index (Compilation_Buffer) + 1);
      end loop;

      Add_To_Compilation_Buffer (Unloop'Access);

   end Plus_Loop;

   ---------
   -- Pop --
   ---------

   function Pop (S : not null access Stacks.Vector) return Cell is
      Result : Cell;
   begin
      if S.Is_Empty then
         raise Stack_Underflow;
      end if;
      Result := Last_Element (S.all);
      Delete_Last (S.all);
      return Result;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop return Cell is
   begin
      return Pop (Data_Stack'Access);
   end Pop;

   ------------
   -- Pop_64 --
   ------------

   function Pop_64 return Integer_64 is
   begin
      return To_Integer_64 (Pop_Unsigned_64);
   end Pop_64;

   ------------------
   -- Pop_Unsigned --
   ------------------

   function Pop_Unsigned return Unsigned_32 is
   begin
      return To_Unsigned_32 (Pop);
   end Pop_Unsigned;

   ---------------------
   -- Pop_Unsigned_64 --
   ---------------------

   function Pop_Unsigned_64 return Unsigned_64 is
      High : constant Unsigned_64 := Unsigned_64 (Pop_Unsigned) * 2 ** 32;
   begin
      return High + Unsigned_64 (Pop_Unsigned);
   end Pop_Unsigned_64;

   --------------
   -- Postpone --
   --------------

   procedure Postpone is
      W      : constant String := Word;
      Action : Action_Type;
   begin
      Action := Find (W);
      if Action.Immediate then
         Add_To_Compilation_Buffer (Action);
      else
         Add_To_Compilation_Buffer (Action.Forth_Proc);
         Add_To_Compilation_Buffer (Compile_Comma'Access);
      end if;
   exception
      when Not_Found =>
         begin
            Add_To_Compilation_Buffer (Cell'Value (W));
         exception
            when Constraint_Error =>
               raise Not_Found with W;
         end;
   end Postpone;

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Stacks.Vector; X : Cell) is
   begin
      Append (S, X);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (X : Cell) is
   begin
      Push (Data_Stack, X);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (B : Boolean) is
   begin
      if B then
         Push (-1);
      else
         Push (0);
      end if;
   end Push;

   -------------
   -- Push_64 --
   -------------

   procedure Push_64 (X : Integer_64) is
   begin
      Push_Unsigned_64 (To_Unsigned_64 (X));
   end Push_64;

   -------------------
   -- Push_Unsigned --
   -------------------

   procedure Push_Unsigned (X : Unsigned_32) is
   begin
      Push (To_Cell (X));
   end Push_Unsigned;

   ----------------------
   -- Push_Unsigned_64 --
   ----------------------

   procedure Push_Unsigned_64 (X : Unsigned_64) is
   begin
      Push_Unsigned (Unsigned_32 (X mod (2 ** 32)));
      Push_Unsigned (Unsigned_32 (X / 2 ** 32));
   end Push_Unsigned_64;

   ----------
   -- Quit --
   ----------

   procedure Quit is
   begin
      loop
         Clear (Data_Stack);
         Clear (Return_Stack);
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
            when Compile_Only =>
               Put_Line ("*** Compile only");
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
      Push (Last_Element (Return_Stack));
   end R_At;

   -------------
   -- Recurse --
   -------------

   procedure Recurse is
   begin
      Add_To_Compilation_Buffer (Current_Action);
   end Recurse;

   ------------
   -- Refill --
   ------------

   procedure Refill is
   begin
      if Use_RL then
         if State.all = 0 then
            Cr;
            Refill_Line (Readline.Read_Line ("ok> "));
         else
            Refill_Line (Readline.Read_Line ("] "));
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

   procedure Refill_Line (Buffer : String) is
      Last : constant Natural := Natural'Min (Buffer'Length, 1024);
   begin
      for I in 1 .. Integer'Min (Buffer'Length, 1024) loop
         Memory (TIB + Cell (I) - 1) := Character'Pos (Buffer (I));
      end loop;
      TIB_Count.all := Cell (Last);
      IN_Ptr.all := 0;
   end Refill_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name   : String;
      Action : Action_Type)
   is
   begin
      Append (Dict, (Name   => To_Unbounded_String (Name),
                     Action => Action));
      Readline.Add_Word (Name);
   end Register;

   -----------------------
   -- Register_Ada_Word --
   -----------------------

   procedure Register_Ada_Word
     (Name      : String;
      Word      : Ada_Word_Access;
      Immediate : Boolean := False)
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
      Set_Inline;
   end Register_Ada_Word;

   -----------------------
   -- Register_Constant --
   -----------------------

   procedure Register_Constant
     (Name  : String;
      Value : Cell)
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
     (Name : String;
      Var  : out Cell_Access)
   is
   begin
      Tick (Name);
      To_Body;
      Var := To_Cell_Access (Memory (Pop) 'Access);
   end Remember_Variable;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (Name : String;
      Var  : out Cell)
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
      Check_Control_Structure (Backward_Reference);
      Literal;
      Add_To_Compilation_Buffer (Jump'Access);
      loop
         declare
            To_Fix : constant Cell := Pop;
         begin
            exit when To_Fix = -1;
            Patch_Jump (To_Fix, Last_Index (Compilation_Buffer) + 1);
         end;
      end loop;
   end Repeat;

   ----------
   -- Roll --
   ----------

   procedure Roll is
      Offset : constant Integer  := Integer (Pop);
      Index  : constant Positive := Last_Index (Data_Stack) - Offset;
   begin
      Append (Data_Stack, Element (Data_Stack, Index));
      Delete (Data_Stack, Index);
   end Roll;

   ------------
   -- Rshift --
   ------------

   procedure Rshift is
      U : constant Natural := Natural (Pop_Unsigned);
   begin
      Push (Pop / 2 ** U);
   end Rshift;

   ------------
   -- S_To_D --
   ------------

   procedure S_To_D is
   begin
      Push_64 (Integer_64 (Pop));
   end S_To_D;

   --------------
   -- ScaleMod --
   --------------

   procedure ScaleMod is
      C : constant Integer_64 := Integer_64 (Pop);
      B : constant Integer_64 := Integer_64 (Pop);
      A : constant Integer_64 := Integer_64 (Pop);
   begin
      Push (Cell ((A * B) mod C));
      Push (Cell (A * B / C));
   end ScaleMod;

   ---------
   -- See --
   ---------

   procedure See is
      Index  : Cell;
      Action : Action_Type;
      Found  : Boolean;
   begin
      Tick;
      Index := Pop;
      loop
         Found := False;
         Put (Cell'Image (Index) & ": ");
         Action := Element (Compilation_Buffer, Index);
         if Action = Forth_Exit then
            Put_Line ("EXIT");
            exit;
         end if;
         case Action.Kind is
            when Number =>
               declare
                  S : constant String := Cell'Image (Action.Value);
               begin
                  Found := True;
                  if Action.Value >= 0 then
                     Put_Line (S (2 .. S'Last));
                  else
                     Put_Line (S);
                  end if;
               end;
            when Forth_Word =>
               for I in reverse First_Index (Dict) .. Last_Index (Dict) loop
                  declare
                     Current : Dictionary_Entry renames Element (Dict, I);
                  begin
                     if Current.Action.Kind = Forth_Word and then
                       Current.Action.Forth_Proc = Action.Forth_Proc
                     then
                        Found := True;
                        Put_Line (To_String (Current.Name));
                        exit;
                     end if;
                  end;
               end loop;
            when Ada_Word =>
               if Action.Ada_Proc = Jump'Access then
                  Found := True;
                  Put_Line ("<JUMP>");
               elsif Action.Ada_Proc = Jump_If_False'Access then
                  Found := True;
                  Put_Line ("<JUMP IF FALSE>");
               elsif Action.Ada_Proc = DoDoes'Access then
                  Found := True;
                  Put_Line ("<DO DOES>");
               else
                  for I in reverse First_Index (Dict) .. Last_Index (Dict) loop
                     declare
                        Current : Dictionary_Entry renames Element (Dict, I);
                     begin
                        if Current.Action.Kind = Forth_Word then
                           declare
                              Idx : constant Cell :=
                                Current.Action.Forth_Proc;
                              A : constant Action_Type :=
                                Element (Compilation_Buffer, Idx);
                           begin
                              if A.Kind = Ada_Word and then
                                A.Ada_Proc = Action.Ada_Proc and then
                                Element (Compilation_Buffer, Idx + 1) =
                                  Forth_Exit
                              then
                                 Found := True;
                                 Put_Line (To_String (Current.Name) &
                                             " <Ada primitive>");
                                 exit;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;
               end if;
         end case;
         if not Found then
            Put_Line ("<unknown control word>");
         end if;
         Index := Index + 1;
      end loop;
   end See;

   ---------------
   -- Semicolon --
   ---------------

   procedure Semicolon is
   begin
      Check_Control_Structure (Definition_Reference);
      Add_To_Compilation_Buffer (Forth_Exit);

      --  Current_Name can be null during definition or completion of
      --  a DOES> prefix.

      if Current_Name /= "" then
         Register (To_String (Current_Name), Current_Action);
         Current_Name := To_Unbounded_String ("");
      end if;

      Interpret_Mode;
   end Semicolon;

   -------------------
   -- Set_Immediate --
   -------------------

   procedure Set_Immediate is
      Current : Dictionary_Entry := Last_Element (Dict);
   begin
      Current.Action.Immediate := True;
      Replace_Element (Dict, Last_Index (Dict), Current);
   end Set_Immediate;

   ----------------
   -- Set_Inline --
   ----------------

   procedure Set_Inline is
      Current : Dictionary_Entry := Last_Element (Dict);
   begin
      Current.Action.Inline := True;
      Replace_Element (Dict, Last_Index (Dict), Current);
   end Set_Inline;

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

   ------------------
   -- Sm_Slash_Rem --
   ------------------

   procedure Sm_Slash_Rem is
      N : constant Integer_64 := Integer_64 (Pop);
      D : constant Integer_64 := Pop_64;
   begin
      Push (Cell (D mod N));
      Push (Cell (D / N));
   end Sm_Slash_Rem;

   ----------------------
   -- Start_Definition --
   ----------------------

   procedure Start_Definition (Name : String := "") is
   begin
      if Name /= "" then
         Current_Name := To_Unbounded_String (Name);
      end if;
      Current_Action.Immediate  := False;
      Current_Action.Forth_Proc := Last_Index (Compilation_Buffer) + 1;
      Compile_Mode;
      Push (Definition_Reference);
   end Start_Definition;

   -----------
   -- Store --
   -----------

   procedure Store
   is
      Addr  : constant Cell_Access := To_Cell_Access (Memory (Pop)'Access);
   begin
      Addr.all := Pop;
   end Store;

   -----------
   -- Store --
   -----------

   procedure Store (Addr : Cell; Value : Cell) is
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
      A : constant Cell := Pop;
      B : constant Cell := Pop;
   begin
      Push (A);
      Push (B);
   end Swap;

   ----------
   -- Tick --
   ----------

   procedure Tick (Name : String) is
      A : constant Action_Type := Find (Name);
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
      Push (Element (Compilation_Buffer, Pop) .Value);
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
      Addr   : Cell          := Pop;
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
      A : constant Cell := Pop;
      B : constant Cell := Pop;
   begin
      Push (B);
      Push (A);
      Push (B);
      Push (A);
   end Two_Dup;

   --------------
   -- Two_R_At --
   --------------

   procedure Two_R_At is
   begin
      Push (Element (Return_Stack, Last_Index (Return_Stack) - 1));
      Push (Last_Element (Return_Stack));
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

   ---------------
   -- U_Smaller --
   ---------------

   procedure U_Smaller is
      R : constant Unsigned_32 := Pop_Unsigned;
   begin
      Push (Pop_Unsigned < R);
   end U_Smaller;

   ------------------
   -- Um_Slash_Mod --
   ------------------

   procedure Um_Slash_Mod is
      N : constant Unsigned_32 := Pop_Unsigned;
      D : constant Unsigned_32 := Pop_Unsigned;
   begin
      Push_Unsigned (D mod N);
      Push_Unsigned (D / N);
   end Um_Slash_Mod;

   -------------
   -- Um_Star --
   -------------

   procedure Um_Star is
   begin
      Push_Unsigned_64 (Unsigned_64 (Pop_Unsigned) *
                        Unsigned_64 (Pop_Unsigned));
   end Um_Star;

   ------------
   -- Unloop --
   ------------

   procedure Unloop is
   begin
      Delete_Last (Return_Stack);
      Delete_Last (Return_Stack);
   end Unloop;

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

   -----------
   -- Words --
   -----------

   procedure Words is
      Len : Natural := 0;
   begin
      for I in First_Index (Dict) .. Last_Index (Dict) loop
         declare
            Current : Dictionary_Entry renames Element (Dict, I);
         begin
            Len := Len + Length (Current.Name) + 1;
            if Len > 75 then
               New_Line;
               Len := Length (Current.Name);
            elsif I /= First_Index (Dict) then
               Put (' ');
            end if;
            Put (To_String (Current.Name));
         end;
      end loop;
   end Words;

begin
   --  Store and register HERE at position 0 -- bootstrap STATE at position 4
   State := To_Cell_Access (Memory (4)'Access);
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
   Register_Ada_Word ("C@", Cfetch'Access);
   Register_Ada_Word ("COMPILE,", Compile_Comma'Access);
   Register_Ada_Word ("C!", Cstore'Access);
   Register_Ada_Word (":", Colon'Access);
   Register_Ada_Word (":NONAME", Colon_Noname'Access);
   Register_Ada_Word ("]", Compile_Mode'Access);
   Register_Ada_Word ("CR", Cr'Access);
   Register_Ada_Word ("/MOD", DivMod'Access);
   Register_Ada_Word ("DOES>", Does'Access, Immediate => True);
   Register_Ada_Word ("DROP", Drop'Access);
   Register_Ada_Word ("DEPTH", Depth'Access);
   Register_Ada_Word (".", Dot'Access);
   Register_Ada_Word ("EMIT", Emit'Access);
   Register_Ada_Word ("EXECUTE", Execute'Access);
   Register_Ada_Word ("@", Fetch'Access);
   Register_Ada_Word ("AND", Forth_And'Access);
   Register_Ada_Word ("BEGIN", Forth_Begin'Access, Immediate => True);
   Register_Ada_Word ("DO", Forth_Do'Access, Immediate => True);
   Register_Ada_Word ("IF", Forth_If'Access, Immediate => True);
   Register_Ada_Word ("OR", Forth_Or'Access);
   Register_Ada_Word ("THEN", Forth_Then'Access, Immediate => True);
   Register_Ada_Word ("WHILE", Forth_While'Access, Immediate => True);
   Register_Ada_Word ("XOR", Forth_Xor'Access);
   Register_Ada_Word ("R>", From_R'Access);
   Register_Ada_Word (">=", Greaterequal'Access);
   Register_Ada_Word ("J", J'Access);
   Register_Ada_Word ("INCLUDE", Include'Access);
   Register_Ada_Word ("[", Interpret_Mode'Access, Immediate => True);
   Register_Ada_Word ("LEAVE", Leave'Access, Immediate => True);
   Register_Ada_Word ("LITERAL", Literal'Access, Immediate => True);
   Register_Ada_Word ("LSHIFT", Lshift'Access);
   Register_Ada_Word ("KEY", Key'Access);
   Register_Ada_Word ("M*", Mstar'Access);
   Register_Ada_Word ("PARSE", Parse'Access);
   Register_Ada_Word ("PICK", Pick'Access);
   Register_Ada_Word ("+", Plus'Access);
   Register_Ada_Word ("+LOOP", Plus_Loop'Access, Immediate => True);
   Register_Ada_Word ("POSTPONE", Postpone'Access, Immediate => True);
   Register_Ada_Word ("QUIT", Quit'Access);
   Register_Ada_Word ("R@", R_At'Access);
   Register_Ada_Word ("RECURSE", Recurse'Access, Immediate => True);
   Register_Ada_Word ("REFILL", Refill'Access);
   Register_Ada_Word ("REPEAT", Repeat'Access, Immediate => True);
   Register_Ada_Word ("ROLL", Roll'Access);
   Register_Ada_Word ("RSHIFT", Rshift'Access);
   Register_Ada_Word ("S>D", S_To_D'Access);
   Register_Ada_Word ("*/MOD", ScaleMod'Access);
   Register_Ada_Word ("SEE", See'Access);
   Register_Ada_Word (";", Semicolon'Access, Immediate => True);
   Register_Ada_Word ("IMMEDIATE", Set_Immediate'Access);
   Register_Ada_Word ("INLINE", Set_Inline'Access);
   Register_Ada_Word ("SKIP-BLANKS", Skip_Blanks'Access);
   Register_Ada_Word ("SM/REM", Sm_Slash_Rem'Access);
   Register_Ada_Word ("SWAP", Swap'Access);
   Register_Ada_Word ("!", Store'Access);
   Register_Ada_Word ("'", Tick'Access);
   Register_Ada_Word ("*", Times'Access);
   Register_Ada_Word (">BODY", To_Body'Access);
   Register_Ada_Word (">R", To_R'Access);
   Register_Ada_Word ("2DUP", Two_Dup'Access);
   Register_Ada_Word ("2R@", Two_R_At'Access);
   Register_Ada_Word ("2>R", Two_To_R'Access);
   Register_Ada_Word ("U<", U_Smaller'Access);
   Register_Ada_Word ("UM/MOD", Um_Slash_Mod'Access);
   Register_Ada_Word ("UM*", Um_Star'Access);
   Register_Ada_Word ("UNLOOP", Unloop'Access);
   Register_Ada_Word ("UNUSED", Unused'Access);
   Register_Ada_Word ("WORD", Word'Access);
   Register_Ada_Word ("WORDS", Words'Access);
   Readline.Variable_Bind ("completion-ignore-case", "on");
end Aforth;
