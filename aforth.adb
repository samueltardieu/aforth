with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Read_Line;

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
            Forth_Proc : Integer_32;
            Inline     : Boolean;
         when Number =>
            Value      : Integer_32;
      end case;
   end record;

   procedure Register (Name   : in String;
                       Action : in Action_Type);

   Not_Found : exception;

   function Find (Name : String) return Action_Type;
   --  May raise Not_Found

   Compilation_Buffer : array (Integer_32'(1) .. 16384) of Action_Type;
   Compilation_Index  : Integer_32 := 1;

   procedure Add_To_Compilation_Buffer (Action : in Action_Type);

   package Integer_32_IO is new Ada.Text_IO.Integer_IO (Integer_32);
   use Integer_32_IO;

   type String_Access is access String;

   type Dictionary_Entry is record
      Name   : String_Access;
      Action : Action_Type;
   end record;

   type Dictionary_Array is array (Positive range <>) of Dictionary_Entry;

   type Dictionary_Access is access Dictionary_Array;

   procedure Free is
      new Ada.Unchecked_Deallocation (Dictionary_Array, Dictionary_Access);

   Dict : Dictionary_Access;

   type Byte_Array is array (Integer_32 range <>) of aliased Unsigned_8;

   Memory : Byte_Array (0 .. 65535) := (others => 0);

   type Byte_Access is access all Unsigned_8;

   pragma Warnings (Off);
   function To_Integer_32_Access is
      new Ada.Unchecked_Conversion (Byte_Access, Integer_32_Access);
   pragma Warnings (On);

   function To_Unsigned_32 is
      new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function To_Integer_32 is
      new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);
   function To_Unsigned_64 is
      new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);
   function To_Integer_64 is
      new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Push (S : access Stack_Type; X : in Integer_32);
   --  May raise stack overflow

   function Pop (S : access Stack_Type) return Integer_32;
   --  May raise stack underflow

   Already_Handled : exception;

   Here      : Integer_32_Access;
   Base      : Integer_32_Access;
   TIB       : Integer_32;
   TIB_Count : Integer_32_Access;
   IN_Ptr    : Integer_32_Access;
   State     : Integer_32_Access;

   Forth_Exit : constant Action_Type := (Kind       => Forth_Word,
                                         Immediate  => True,
                                         Inline     => False,
                                         Forth_Proc => -1);

   Forward_Reference  : constant := -100;
   Backward_Reference : constant := -101;
   Do_Loop_Reference  : constant := -102;

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

   procedure Check_Control_Structure (Reference : in Integer_32);

   procedure Set_Last_Immediate (Dict : in Dictionary_Access);
   procedure Set_Last_Inline (Dict : in Dictionary_Access);

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (Action : in Action_Type) is
   begin
      Check_Compile_Only;

      --  Call or inline words

      if Action.Kind = Forth_Word and then Action.Inline then
         declare
            Index : Integer_32 := Action.Forth_Proc;
         begin
            while Compilation_Buffer (Index) /= Forth_Exit loop
               Add_To_Compilation_Buffer (Compilation_Buffer (Index));
               Index := Index + 1;
            end loop;
         end;
      else
         Compilation_Buffer (Compilation_Index) := Action;
         Compilation_Index := Compilation_Index + 1;
      end if;
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

      --  The compilation stack contains the index of the address to
      --  patch when the AHEAD is resolved by a THEN.

   begin
      Push (Compilation_Index);
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

   -----------------------------
   -- Check_Control_Structure --
   -----------------------------

   procedure Check_Control_Structure (Reference : in Integer_32) is
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
                     Inline     => False,
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

   function Find (Name : String) return Action_Type
   is
      Lower_Name : constant String := To_Lower (Name);
   begin
      for I in reverse Dict'Range loop
         if To_Lower (Dict (I) .Name.all) = Lower_Name then
            pragma Assert (Dict (I) .Action.Kind = Forth_Word);
            return Dict (I) .Action;
         end if;
      end loop;
      Raise_Exception (Not_Found'Identity, Name);
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
      Push (Compilation_Index);
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
      Push (Compilation_Index);
      Push (Do_Loop_Reference);
   end Forth_Do;

   ----------------
   -- Forth_Else --
   ----------------

   procedure Forth_Else is
   begin
      Ahead;
      Two_Swap;
      Forth_Then;
   end Forth_Else;

   --------------
   -- Forth_If --
   --------------

   procedure Forth_If is
   begin
      Push (Compilation_Index);
      Push (Forward_Reference);
      Add_To_Compilation_Buffer (0);
      Add_To_Compilation_Buffer (Jump_If_False'Access);
   end Forth_If;

   ----------------
   -- Forth_Loop --
   ----------------

   procedure Forth_Loop is
   begin
      Add_To_Compilation_Buffer (1);
      Plus_Loop;
   end Forth_Loop;

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
      Check_Control_Structure (Forward_Reference);
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
                  A := Find (W);
                  A.Immediate := True;
                  Execute_Action (A);
               exception
                  when NF : Not_Found =>
                     begin
                        I := Integer_32'Value (W);
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
                        I := Integer_32'Value (W);
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

   -------
   -- J --
   -------

   procedure J is
   begin
      Push (Return_Stack.Data (Return_Stack.Top - 2));
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

   -----------
   -- Leave --
   -----------

   procedure Leave is
   begin
      --  Loop for Do_Loop_Reference on the stack

      for I in reverse Data_Stack.Data'First .. Data_Stack.Top loop
         if Data_Stack.Data (I) = Do_Loop_Reference then

            --  Insert the leave information at the proper place

            Data_Stack.Data (I .. Data_Stack.Top + 1) :=
              Data_Stack.Data (I - 1 .. Data_Stack.Top);
            Data_Stack.Top := Data_Stack.Top + 1;
            Data_Stack.Data (I - 1) := Compilation_Index;
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

   -----------
   -- Mstar --
   -----------

   procedure Mstar is
   begin
      Push_64 (Integer_64 (Pop) * Integer_64 (Pop));
   end Mstar;

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
      pragma Assert (To_Patch < Compilation_Index);
      pragma Assert (Target <= Compilation_Index);
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

   ---------------
   -- Plus_Loop --
   ---------------

   procedure Plus_Loop is
      To_Patch : Integer_32;
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
         Patch_Jump (To_Patch => To_Patch, Target => Compilation_Index);
      end loop;

      Add_To_Compilation_Buffer (Unloop'Access);

   end Plus_Loop;

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

   -------------
   -- Push_64 --
   -------------

   procedure Push_64 (X : in Integer_64) is
   begin
      Push_Unsigned_64 (To_Unsigned_64 (X));
   end Push_64;

   -------------------
   -- Push_Unsigned --
   -------------------

   procedure Push_Unsigned (X : in Unsigned_32) is
   begin
      Push (To_Integer_32 (X));
   end Push_Unsigned;

   ----------------------
   -- Push_Unsigned_64 --
   ----------------------

   procedure Push_Unsigned_64 (X : in Unsigned_64) is
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
      Set_Inline;
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
      Check_Control_Structure (Backward_Reference);
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
      Index    : constant Integer    := Data_Stack.Top - Integer (Pop) - 1;
      Moved    : constant Integer_32 := Data_Stack.Data (Index);
   begin
      Data_Stack.Data (Index .. Data_Stack.Top - 1) :=
        Data_Stack.Data (Index + 1 .. Data_Stack.Top);
      Data_Stack.Data (Data_Stack.Top) := Moved;
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
      Push (Integer_32 ((A * B) mod C));
      Push (Integer_32 (A * B / C));
   end ScaleMod;

   ---------
   -- See --
   ---------

   procedure See is
      Index  : Integer_32;
      Action : Action_Type;
      Found  : Boolean;
   begin
      Tick;
      Index := Pop;
      loop
         Found := False;
         Put (Integer_32'Image (Index) & ": ");
         Action := Compilation_Buffer (Index);
         if Action = Forth_Exit then
            Put_Line ("EXIT");
            exit;
         end if;
         case Action.Kind is
            when Number =>
               declare
                  S : constant String := Integer_32'Image (Action.Value);
               begin
                  Found := True;
                  if Action.Value >= 0 then
                     Put_Line (S (2 .. S'Last));
                  else
                     Put_Line (S);
                  end if;
               end;
            when Forth_Word =>
               for I in reverse Dict'Range loop
                  if Dict (I) .Action.Kind = Forth_Word and then
                    Dict (I) .Action.Forth_Proc = Action.Forth_Proc
                  then
                     Found := True;
                     Put_Line (Dict (I) .Name.all);
                     exit;
                  end if;
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
                  for I in reverse Dict'Range loop
                     if Dict (I) .Action.Kind = Forth_Word then
                        declare
                           Idx : constant Integer_32 :=
                             Dict (I) .Action.Forth_Proc;
                           A : constant Action_Type :=
                             Compilation_Buffer (Idx);
                        begin
                           if A.Kind = Ada_Word and then
                             A.Ada_Proc = Action.Ada_Proc and then
                             Compilation_Buffer (Idx + 1) = Forth_Exit
                           then
                              Found := True;
                              Put_Line (Dict (I) .Name.all &
                                        " <Ada primitive>");
                              exit;
                           end if;
                        end;
                     end if;
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

   ----------------
   -- Set_Inline --
   ----------------

   procedure Set_Inline is
   begin
      Set_Last_Inline (Dict);
   end Set_Inline;

   ------------------------
   -- Set_Last_Immediate --
   ------------------------

   procedure Set_Last_Immediate (Dict : in Dictionary_Access) is
   begin
      Dict (Dict'Last) .Action.Immediate := True;
   end Set_Last_Immediate;

   ---------------------
   -- Set_Last_Inline --
   ---------------------

   procedure Set_Last_Inline (Dict : in Dictionary_Access) is
   begin
      Dict (Dict'Last) .Action.Inline := True;
   end Set_Last_Inline;

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
      Push (Integer_32 (D mod N));
      Push (Integer_32 (D / N));
   end Sm_Slash_Rem;

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
         Current_Name := new String'(Name);
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
   -- Two_Swap --
   --------------

   procedure Two_Swap is

      procedure Swap (X, Y : in out Integer_32);

      procedure Swap (X, Y : in out Integer_32) is
         Temp : constant Integer_32 := X;
      begin
         X := Y;
         Y := Temp;
      end Swap;

   begin
      Swap (Data_Stack.Data (Data_Stack.Top),
            Data_Stack.Data (Data_Stack.Top - 2));
      Swap (Data_Stack.Data (Data_Stack.Top - 1),
            Data_Stack.Data (Data_Stack.Top - 3));
   end Two_Swap;

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
      Return_Stack.Top := Return_Stack.Top - 2;
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
      for I in Dict.all'Range loop
         Len := Len + Dict (I) .Name'Length + 1;
         if Len > 75 then
            New_Line;
            Len := Dict (I) .Name'Length;
         elsif I /= Dict.all'First then
            Put (' ');
         end if;
         Put (Dict (I) .Name.all);
      end loop;
   end Words;

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
   Register_Ada_Word ("EMIT", Emit'Access);
   Register_Ada_Word ("=", Equal'Access);
   Register_Ada_Word ("EXECUTE", Execute'Access);
   Register_Ada_Word ("@", Fetch'Access);
   Register_Ada_Word ("AND", Forth_And'Access);
   Register_Ada_Word ("BEGIN", Forth_Begin'Access, Immediate => True);
   Register_Ada_Word ("DO", Forth_Do'Access, Immediate => True);
   Register_Ada_Word ("ELSE", Forth_Else'Access, Immediate => True);
   Register_Ada_Word ("[CHAR]", Ichar'Access, Immediate => True);
   Register_Ada_Word ("IF", Forth_If'Access, Immediate => True);
   Register_Ada_Word ("LOOP", Forth_Loop'Access, Immediate => True);
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
   Register_Ada_Word ("J", J'Access);
   Register_Ada_Word ("INCLUDE", Include'Access);
   Register_Ada_Word ("[", Interpret_Mode'Access, Immediate => True);
   Register_Ada_Word ("LEAVE", Leave'Access, Immediate => True);
   Register_Ada_Word ("LITERAL", Literal'Access, Immediate => True);
   Register_Ada_Word ("LSHIFT", Lshift'Access);
   Register_Ada_Word ("KEY", Key'Access);
   Register_Ada_Word ("-", Minus'Access);
   Register_Ada_Word ("-!", Minusstore'Access);
   Register_Ada_Word ("M*", Mstar'Access);
   Register_Ada_Word ("<>", Notequal'Access);
   Register_Ada_Word ("1-", Oneminus'Access);
   Register_Ada_Word ("1+", Oneplus'Access);
   Register_Ada_Word ("OVER", Over'Access);
   Register_Ada_Word ("PARSE", Parse'Access);
   Register_Ada_Word ("PICK", Pick'Access);
   Register_Ada_Word ("+", Plus'Access);
   Register_Ada_Word ("+LOOP", Plus_Loop'Access);
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
   Register_Ada_Word ("2SWAP", Two_Swap'Access);
   Register_Ada_Word ("2>R", Two_To_R'Access);
   Register_Ada_Word ("U<", U_Smaller'Access);
   Register_Ada_Word ("UM/MOD", Um_Slash_Mod'Access);
   Register_Ada_Word ("UM*", Um_Star'Access);
   Register_Ada_Word ("UNLOOP", Unloop'Access);
   Register_Ada_Word ("UNUSED", Unused'Access);
   Register_Ada_Word ("WORD", Word'Access);
   Register_Ada_Word ("WORDS", Words'Access);
end Aforth;
