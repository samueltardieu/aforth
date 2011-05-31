------------------------------------------------------------------------------
--                                                                          --
--                            AFORTH COMPONENTS                             --
--                                                                          --
--                    F O R T H . I N T E R P R E T E R                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2011 Samuel Tardieu <sam@rfc1149.net>        --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Real_Time;              use Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Forth.Builtins;
with Readline.Completion;
with Readline.Variables;

package body Forth.Interpreter is

   --  Notes:
   --    - the compilation stack is the data stack

   TIB_Length           : constant := 1024;

   Stack_Marker         : constant := -1;
   Forward_Reference    : constant := -100;
   Backward_Reference   : constant := -101;
   Do_Loop_Reference    : constant := -102;
   Definition_Reference : constant := -103;

   use Dictionaries, Compilation_Buffers;

   procedure Initialize (I : IT);
   --  Register builtin words (Ada and Forth primitives)

   procedure Register (I      : IT;
                       Name   : String;
                       Action : Action_Type);

   function Find (I : IT; Name : String) return Action_Type;
   --  May raise Word_Not_Found

   procedure Add_To_Compilation_Buffer (I : IT; Action : Action_Type);

   function Next_Index (V : Compilation_Buffers.Vector)
     return Natural_Cell;

   package Cell_IO is new Ada.Text_IO.Integer_IO (Cell);
   use Cell_IO;

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

   Forth_Exit : constant Action_Type := (Kind       => Forth_Word,
                                         Immediate  => True,
                                         Inline     => False,
                                         Forth_Proc => -1);

   procedure Remember_Variable
     (I    : IT;
      Name : String;
      Var  : out Cell_Access);

   procedure Remember_Variable
     (I    : IT;
      Name : String;
      Var  : out Cell);

   procedure Start_Definition (I : IT; Name : String := "");

   function To_String (I : IT) return String;

   procedure Execute_Action (I : IT; Action : Action_Type);

   procedure Execute_Forth_Word (I : IT; Addr : Cell);

   procedure Main_Loop (I : IT);

   function Word (I : IT) return String;

   procedure Jump (I : IT);
   procedure Jump_If_False (I : IT);
   procedure Patch_Jump (I : IT; To_Patch : Cell; Target : Cell);

   procedure Add_To_Compilation_Buffer (I : IT; Ada_Proc : Ada_Word_Access);
   procedure Add_To_Compilation_Buffer (I : IT; Value : Cell);

   procedure DoDoes (I : IT);

   procedure Refill_Line (I : IT; Buffer : String);

   procedure Check_Compile_Only (I : IT);

   procedure Tick (I : IT; Name : String);

   procedure Check_Control_Structure (I : IT; Reference : Cell);

   function Is_Blank (C : Character) return Boolean;

   function Parse_Number (I : IT; S : String) return Cell;
   --  Parse a number given the current base. This will raise Constraint_Error
   --  if the number cannot be parsed.

   function Peek (I : IT) return Cell;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (I : IT; Action : Action_Type) is
   begin
      Check_Compile_Only (I);

      --  Call or inline words

      if Action.Kind = Forth_Word and then Action.Inline then
         declare
            Index : Cell := Action.Forth_Proc;
         begin
            while Element (I.Compilation_Buffer, Index) /= Forth_Exit loop
               Add_To_Compilation_Buffer
                 (I, Element (I.Compilation_Buffer, Index));
               Index := Index + 1;
            end loop;
         end;
      else
         Append (I.Compilation_Buffer, Action);
      end if;
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (I : IT; Ada_Proc : Ada_Word_Access) is
   begin
      Add_To_Compilation_Buffer
        (I,
         Action_Type'(Kind      => Ada_Word,
                      Immediate => True,
                      Ada_Proc  => Ada_Proc));
   end Add_To_Compilation_Buffer;

   -------------------------------
   -- Add_To_Compilation_Buffer --
   -------------------------------

   procedure Add_To_Compilation_Buffer (I : IT; Value : Cell) is
   begin
      Add_To_Compilation_Buffer
        (I,
         Action_Type'(Kind      => Number,
                      Immediate => True,
                      Value     => Value));
   end Add_To_Compilation_Buffer;

   -----------
   -- Again --
   -----------

   procedure Again (I : IT) is
   begin
      Check_Control_Structure (I, Backward_Reference);
      Literal (I);
      Add_To_Compilation_Buffer (I, Jump'Access);
      Check_Control_Structure (I, Stack_Marker);
   end Again;

   -----------
   -- Ahead --
   -----------

   procedure Ahead (I : IT) is

      --  The compilation stack contains the index of the address to
      --  patch when the AHEAD is resolved by a THEN.

   begin
      Push (I, Next_Index (I.Compilation_Buffer));
      Push (I, Forward_Reference);
      Add_To_Compilation_Buffer (I, 0);
      Add_To_Compilation_Buffer (I, Jump'Access);
   end Ahead;

   -----------
   -- Align --
   -----------

   procedure Align (I : IT) is
   begin
      if I.Here.all mod 4 /= 0 then
         I.Here.all := I.Here.all + (4 - (I.Here.all mod 4));
      end if;
   end Align;

   ---------
   -- Bye --
   ---------

   procedure Bye (I : IT) is
   begin
      raise Bye_Exception;
   end Bye;

   ------------
   -- Cfetch --
   ------------

   procedure Cfetch (I : IT) is
   begin
      Push (I, Cell (I.Memory (Pop (I))));
   end Cfetch;

   ------------
   -- Cfetch --
   ------------

   function Cfetch (I : IT; Addr : Cell) return Cell is
   begin
      Push (I, Addr);
      Cfetch (I);
      return Pop (I);
   end Cfetch;

   ------------------------
   -- Check_Compile_Only --
   ------------------------

   procedure Check_Compile_Only (I : IT) is
   begin
      if I.State.all /= 1 then
         raise Compile_Only;
      end if;
   end Check_Compile_Only;

   -----------------------------
   -- Check_Control_Structure --
   -----------------------------

   procedure Check_Control_Structure (I : IT; Reference : Cell) is
   begin
      Check_Compile_Only (I);
      if Pop (I) /= Reference then
         raise Unbalanced_Control_Structure;
      end if;
   end Check_Control_Structure;

   -----------
   -- Colon --
   -----------

   procedure Colon (I : IT) is
   begin
      Start_Definition (I, Word (I));
   end Colon;

   ------------------
   -- Colon_Noname --
   ------------------

   procedure Colon_Noname (I : IT) is
   begin
      Push (I, Next_Index (I.Compilation_Buffer));
      Start_Definition (I);
   end Colon_Noname;

   -------------------
   -- Compile_Comma --
   -------------------

   procedure Compile_Comma (I : IT) is
   begin
      Add_To_Compilation_Buffer (I, Pop (I));
      Add_To_Compilation_Buffer (I, Execute'Access);
   end Compile_Comma;

   ------------------
   -- Compile_Exit --
   ------------------

   procedure Compile_Exit (I : IT) is
   begin
      Add_To_Compilation_Buffer (I, Forth_Exit);
   end Compile_Exit;

   ------------------
   -- Compile_Mode --
   ------------------

   procedure Compile_Mode (I : IT) is
   begin
      I.State.all := 1;
   end Compile_Mode;

   -----------
   -- Count --
   -----------

   procedure Count (I : IT) is
      Start : constant Cell := Pop (I);
   begin
      Push (I, Start + 1);
      Push (I, Cell (I.Memory (Start)));
   end Count;

   --------
   -- Cr --
   --------

   procedure Cr (I : IT) is
   begin
      Push (I, 13);
      Emit (I);
      Push (I, 10);
      Emit (I);
   end Cr;

   ------------
   -- Cstore --
   ------------

   procedure Cstore (I : IT) is
      Addr : constant Cell := Pop (I);
   begin
      I.Memory (Addr) := Unsigned_8 (Pop (I));
   end Cstore;

   -----------
   -- D_Abs --
   -----------

   procedure D_Abs (I : IT) is
   begin
      Push_64 (I, abs (Pop_64 (I)));
   end D_Abs;

   -------------
   -- D_Equal --
   -------------

   procedure D_Equal (I : IT) is
   begin
      Push (I, Pop_64 (I) = Pop_64 (I));
   end D_Equal;

   -----------
   -- D_Max --
   -----------

   procedure D_Max (I : IT) is
   begin
      Push_64 (I, Integer_64'Max (Pop_64 (I), Pop_64 (I)));
   end D_Max;

   -----------
   -- D_Min --
   -----------

   procedure D_Min (I : IT) is
   begin
      Push_64 (I, Integer_64'Min (Pop_64 (I), Pop_64 (I)));
   end D_Min;

   -------------
   -- D_Minus --
   -------------

   procedure D_Minus (I : IT) is
      X : constant Integer_64 := Pop_64 (I);
   begin
      Push_64 (I, Pop_64 (I) - X);
   end D_Minus;

   ------------
   -- D_Plus --
   ------------

   procedure D_Plus (I : IT) is
   begin
      Push_64 (I, Pop_64 (I) + Pop_64 (I));
   end D_Plus;

   ---------------
   -- D_Smaller --
   ---------------

   procedure D_Smaller (I : IT) is
      X : constant Integer_64 := Pop_64 (I);
   begin
      Push (I, Pop_64 (I) < X);
   end D_Smaller;

   ---------------
   -- D_Two_Div --
   ---------------

   procedure D_Two_Div (I : IT) is
      A : constant Integer_64 := Pop_64 (I);
      B : Unsigned_64 := To_Unsigned_64 (A) / 2;
   begin
      if A < 0 then
         B := B or (2 ** 63);
      end if;
      Push_Unsigned_64 (I, B);
   end D_Two_Div;

   -----------------
   -- D_Two_Times --
   -----------------

   procedure D_Two_Times (I : IT) is
   begin
      Push_Unsigned_64 (I, Pop_Unsigned_64 (I) * 2);
   end D_Two_Times;

   -----------
   -- Depth --
   -----------

   procedure Depth (I : IT) is
   begin
      Push (I, Cell (Length (I.Data_Stack)));
   end Depth;

   ------------
   -- DivMod --
   ------------

   procedure DivMod (I : IT) is
      B : constant Cell := Pop (I);
      A : constant Cell := Pop (I);
   begin
      Push (I, A rem B);
      Push (I, A / B);
   end DivMod;

   ------------
   -- DoDoes --
   ------------

   procedure DoDoes (I : IT) is
   begin
      --  Patch the latest exit by inserting a call to the current
      --  action.

      pragma Assert (Last_Element (I.Compilation_Buffer) = Forth_Exit);
      Insert (I.Compilation_Buffer,
              Last_Index (I.Compilation_Buffer),
              Action_Type'(Kind       => Forth_Word,
                           Immediate  => True,
                           Inline     => False,
                           Forth_Proc => Pop (I)));
   end DoDoes;

   ----------
   -- Does --
   ----------

   procedure Does (I : IT) is

      --  Terminate current word after asking to patch the latest created
      --  one. Compilation buffer after index, call to DoDoes and exit
      --  is Compilation_Index + 3.

      Does_Part : constant Cell := Last_Index (I.Compilation_Buffer) + 4;
   begin
      Add_To_Compilation_Buffer (I, Does_Part);
      Add_To_Compilation_Buffer (I, DoDoes'Access);
      Semicolon (I);

      --  Start an unnamed word corresponding to the DOES> part

      Start_Definition (I);
      pragma Assert (Next_Index (I.Compilation_Buffer) = Does_Part);
   end Does;

   ----------
   -- Drop --
   ----------

   procedure Drop (I : IT) is
      Value : constant Cell := Pop (I);
      pragma Unreferenced (Value);
   begin
      null;
   end Drop;

   ---------
   -- Dup --
   ---------

   procedure Dup (I : IT) is
   begin
      Push (I, Peek (I.Data_Stack));
   end Dup;

   ----------
   -- Emit --
   ----------

   procedure Emit (I : IT) is
   begin
      Put (Character'Val (Pop (I)));
   end Emit;

   -----------
   -- Equal --
   -----------

   procedure Equal (I : IT) is
   begin
      Push (I, Pop (I) = Pop (I));
   end Equal;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (I : IT) is
   begin
      Interpret_Line (I, To_String (I));
   end Evaluate;

   -------------
   -- Execute --
   -------------

   procedure Execute (I : IT) is
   begin
      Execute_Forth_Word (I, Pop (I));
   end Execute;

   --------------------
   -- Execute_Action --
   --------------------

   procedure Execute_Action (I : IT; Action : Action_Type) is
   begin
      case Action.Kind is
         when Ada_Word =>
            Action.Ada_Proc.all (I);
         when Forth_Word =>
            Execute_Forth_Word (I, Action.Forth_Proc);
         when Number =>
            Push (I, Action.Value);
      end case;
   end Execute_Action;

   ------------------------
   -- Execute_Forth_Word --
   ------------------------

   procedure Execute_Forth_Word (I : IT; Addr : Cell) is
   begin
      Push (I.Return_Stack, I.Current_IP);
      I.Current_IP := Addr;
      while not I.Interrupt loop
         declare
            Current_Action : constant Action_Type :=
              Element (I.Compilation_Buffer, I.Current_IP);
         begin
            I.Current_IP := I.Current_IP + 1;
            if Current_Action = Forth_Exit then
               I.Current_IP := Pop (I.Return_Stack);
               return;
            end if;
            Execute_Action (I, Current_Action);
         end;
      end loop;
   end Execute_Forth_Word;

   -----------
   -- Fetch --
   -----------

   procedure Fetch (I : IT) is
      pragma Warnings (Off);
      Addr  : constant Cell_Access :=
        To_Cell_Access (I.Memory (Pop (I))'Access);
      pragma Warnings (On);
   begin
      Push (I, Addr.all);
   end Fetch;

   -----------
   -- Fetch --
   -----------

   function Fetch (I : IT; Addr : Cell) return Cell is
   begin
      Push (I, Addr);
      Fetch (I);
      return Pop (I);
   end Fetch;

   ----------
   -- Find --
   ----------

   procedure Find (I : IT) is
      C : constant Cell := Peek (I);
      A : Action_Type;
   begin
      Count (I);
      A := Find (I, To_String (I));
      Push (I, A.Forth_Proc);
      if A.Immediate then
         Push (I, 1);
      else
         Push (I, -1);
      end if;
   exception
      when Word_Not_Found =>
         Push (I, C);
         Push (I, 0);
   end Find;

   ----------
   -- Find --
   ----------

   function Find (I : IT; Name : String) return Action_Type
   is
      Lower_Name : constant String := To_Lower (Name);
   begin
      for J in reverse First_Index (I.Dict) .. Last_Index (I.Dict) loop
         declare
            Current : Dictionary_Entry renames Element (I.Dict, J);
         begin
            if To_Lower (To_String (Current.Name)) = Lower_Name then
               pragma Assert (Current.Action.Kind = Forth_Word);
               return Current.Action;
            end if;
         end;
      end loop;
      Raise_Word_Not_Found (Name);
   end Find;

   ------------------
   -- Fm_Slash_Mod --
   ------------------

   procedure Fm_Slash_Mod (I : IT) is
      Divisor   : constant Integer_64 := Integer_64 (Pop (I));
      Dividend  : constant Integer_64 := Pop_64 (I);
      Remainder : constant Integer_64 := Dividend mod Divisor;
      Quotient  : constant Integer_64 := (Dividend - Remainder) / Divisor;
   begin
      Push (I, Cell (Remainder));
      Push_64 (I, Quotient);
      Drop (I);
   end Fm_Slash_Mod;

   ---------------
   -- Forth_And --
   ---------------

   procedure Forth_And (I : IT) is
   begin
      Push_Unsigned (I, Pop_Unsigned (I) and Pop_Unsigned (I));
   end Forth_And;

   -----------------
   -- Forth_Begin --
   -----------------

   procedure Forth_Begin (I : IT) is

      --  The structure of the BEGIN/WHILE/REPEAT loop on the compilation
      --  stack is:
      --    Stack_Marker
      --    addr of first WHILE to patch
      --    addr of second WHILE to patch
      --    ...
      --    addr of the beginning of the loop
      --    Backward_Reference

   begin
      Push (I, Stack_Marker);
      Push (I, Next_Index (I.Compilation_Buffer));
      Push (I, Backward_Reference);
   end Forth_Begin;

   --------------
   -- Forth_Do --
   --------------

   procedure Forth_Do (I : IT) is

      --  The structure of a DO - LOOP/+LOOP on the compilation stack
      --  is:
      --    Stack_Marker
      --    addr of the first DO/LEAVE
      --    addr of the second LEAVE
      --    addr of the third LEAVE
      --    ...
      --    addr of the beginning of the loop
      --    Do_Loop_Reference
      --  At run-time, on the return stack, we have:
      --    Loop_Limit
      --    Loop_Index

   begin
      Add_To_Compilation_Buffer (I, Two_To_R'Access);
      Push (I, Stack_Marker);
      Push (I, Next_Index (I.Compilation_Buffer));
      Push (I, Do_Loop_Reference);
   end Forth_Do;

   --------------
   -- Forth_If --
   --------------

   procedure Forth_If (I : IT) is
   begin
      Push (I, Next_Index (I.Compilation_Buffer));
      Push (I, Forward_Reference);
      Add_To_Compilation_Buffer (I, 0);
      Add_To_Compilation_Buffer (I, Jump_If_False'Access);
   end Forth_If;

   --------------
   -- Forth_Or --
   --------------

   procedure Forth_Or (I : IT) is
   begin
      Push_Unsigned (I, Pop_Unsigned (I) or Pop_Unsigned (I));
   end Forth_Or;

   ----------------
   -- Forth_Then --
   ----------------

   procedure Forth_Then (I : IT) is
   begin
      Check_Control_Structure (I, Forward_Reference);
      Patch_Jump (I,
                  To_Patch => Pop (I),
                  Target   => Next_Index (I.Compilation_Buffer));
   end Forth_Then;

   -----------------
   -- Forth_While --
   -----------------

   procedure Forth_While (I : IT) is
   begin
      Check_Control_Structure (I, Backward_Reference);
      Push (I, Next_Index (I.Compilation_Buffer));
      Swap (I);
      Add_To_Compilation_Buffer (I, 0);
      Add_To_Compilation_Buffer (I, Jump_If_False'Access);
      Push (I, Backward_Reference);
   end Forth_While;

   ---------------
   -- Forth_Xor --
   ---------------

   procedure Forth_Xor (I : IT) is
   begin
      Push_Unsigned (I, Pop_Unsigned (I) xor Pop_Unsigned (I));
   end Forth_Xor;

   ----------------------
   -- Free_Interpreter --
   ----------------------

   procedure Free_Interpreter (I : in out IT) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Interpreter_Body, Interpreter_Type);
   begin
      Free (I);
   end Free_Interpreter;

   ------------
   -- From_R --
   ------------

   procedure From_R (I : IT) is
   begin
      Push (I, Pop (I.Return_Stack));
   end From_R;

   ------------------
   -- Greaterequal --
   ------------------

   procedure Greaterequal (I : IT) is
      B : constant Cell := Pop (I);
   begin
      Push (I, Pop (I) >= B);
   end Greaterequal;

   -------------
   -- Include --
   -------------

   procedure Include (I : IT) is
   begin
      Include_File (I, Word (I));
   end Include;

   ------------------
   -- Include_File --
   ------------------

   procedure Include_File (I : IT; File_Name : String)
   is
      Previous_Input : constant File_Access := Current_Input;
      File           : File_Type;
      Old_TIB_Count  : constant Cell  := I.TIB_Count.all;
      Old_IN_Ptr     : constant Cell  := I.IN_Ptr.all;
      Old_TIB        : constant Byte_Array  :=
        I.Memory (I.TIB .. I.TIB + Old_TIB_Count - 1);
      Old_Use_RL     : constant Boolean     := I.Use_RL;
   begin
      begin
         Open (File, In_File, File_Name);
      exception
         when Name_Error =>
            Put_Line ("*** File not found: " & File_Name);
            raise;
      end;
      Set_Input (File);
      I.Use_RL := False;
      begin
         Main_Loop (I);
      exception
         when End_Error =>
            Close (File);
            Set_Input (Previous_Input.all);
            I.Memory (I.TIB .. I.TIB + Old_TIB_Count - 1) := Old_TIB;
            I.TIB_Count.all                               := Old_TIB_Count;
            I.IN_Ptr.all                                  := Old_IN_Ptr;
            I.Use_RL                                      := Old_Use_RL;
         when others =>
            Close (File);
            Set_Input (Previous_Input.all);
            I.Use_RL := Old_Use_RL;
            raise;
      end;
   end Include_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (I : IT) is
   begin
      --  Store and register HERE at position 0
      --  Bootstrap STATE at position 4
      pragma Warnings (Off);
      I.State := To_Cell_Access (I.Memory (4)'Access);
      pragma Warnings (On);
      Store (I, 0, 4);
      Start_Definition (I, "(HERE)");
      Add_To_Compilation_Buffer (I, 0);
      Semicolon (I);
      Remember_Variable (I, "(HERE)", I.Here);
      Make_And_Remember_Variable (I, "STATE", I.State);

      --  Default existing variables
      Make_And_Remember_Variable (I, "BASE", I.Base, Initial_Value => 10);
      Make_And_Remember_Variable (I, "TIB", I.TIB, Size => 1024);
      Make_And_Remember_Variable (I, "TIB#", I.TIB_Count);
      Make_And_Remember_Variable (I, ">IN", I.IN_Ptr);

      --  Default Ada words
      Register_Ada_Word (I, "AGAIN", Again'Access, Immediate => True);
      Register_Ada_Word (I, "AHEAD", Ahead'Access, Immediate => True);
      Register_Ada_Word (I, "ALIGN", Align'Access);
      Register_Ada_Word (I, "BYE", Bye'Access);
      Register_Ada_Word (I, "C@", Cfetch'Access);
      Register_Ada_Word (I, "COMPILE,", Compile_Comma'Access);
      Register_Ada_Word (I, "COUNT", Count'Access);
      Register_Ada_Word (I, "C!", Cstore'Access);
      Register_Ada_Word (I, ":", Colon'Access);
      Register_Ada_Word (I, ":NONAME", Colon_Noname'Access);
      Register_Ada_Word (I, "]", Compile_Mode'Access);
      Register_Ada_Word (I, "CR", Cr'Access);
      Register_Ada_Word (I, "DABS", D_Abs'Access);
      Register_Ada_Word (I, "D=", D_Equal'Access);
      Register_Ada_Word (I, "DMAX", D_Max'Access);
      Register_Ada_Word (I, "DMIN", D_Min'Access);
      Register_Ada_Word (I, "D-", D_Minus'Access);
      Register_Ada_Word (I, "D+", D_Plus'Access);
      Register_Ada_Word (I, "D<", D_Smaller'Access);
      Register_Ada_Word (I, "D2/", D_Two_Div'Access);
      Register_Ada_Word (I, "D2*", D_Two_Times'Access);
      Register_Ada_Word (I, "DEPTH", Depth'Access);
      Register_Ada_Word (I, "/MOD", DivMod'Access);
      Register_Ada_Word (I, "DOES>", Does'Access, Immediate => True);
      Register_Ada_Word (I, "DROP", Drop'Access);
      Register_Ada_Word (I, "DUP", Dup'Access);
      Register_Ada_Word (I, "EMIT", Emit'Access);
      Register_Ada_Word (I, "=", Equal'Access);
      Register_Ada_Word (I, "EVALUATE", Evaluate'Access);
      Register_Ada_Word (I, "EXECUTE", Execute'Access);
      Register_Ada_Word (I, "@", Fetch'Access);
      Register_Ada_Word (I, "FIND", Find'Access);
      Register_Ada_Word (I, "FM/MOD", Fm_Slash_Mod'Access);
      Register_Ada_Word (I, "AND", Forth_And'Access);
      Register_Ada_Word (I, "BEGIN", Forth_Begin'Access, Immediate => True);
      Register_Ada_Word (I, "DO", Forth_Do'Access, Immediate => True);
      Register_Ada_Word (I, "EXIT", Compile_Exit'Access, Immediate => True);
      Register_Ada_Word (I, "IF", Forth_If'Access, Immediate => True);
      Register_Ada_Word (I, "OR", Forth_Or'Access);
      Register_Ada_Word (I, "THEN", Forth_Then'Access, Immediate => True);
      Register_Ada_Word (I, "WHILE", Forth_While'Access, Immediate => True);
      Register_Ada_Word (I, "XOR", Forth_Xor'Access);
      Register_Ada_Word (I, "R>", From_R'Access);
      Register_Ada_Word (I, ">=", Greaterequal'Access);
      Register_Ada_Word (I, "J", J'Access);
      Register_Ada_Word (I, "INCLUDE", Include'Access);
      Register_Ada_Word (I, "[", Interpret_Mode'Access, Immediate => True);
      Register_Ada_Word (I, "LEAVE", Leave'Access, Immediate => True);
      Register_Ada_Word (I, "LITERAL", Literal'Access, Immediate => True);
      Register_Ada_Word (I, "LSHIFT", Lshift'Access);
      Register_Ada_Word (I, "KEY", Key'Access);
      Register_Ada_Word (I, "MS", MS'Access);
      Register_Ada_Word (I, "M*", Mstar'Access);
      Register_Ada_Word (I, "0<", Negative'Access);
      Register_Ada_Word (I, "PARSE", Parse'Access);
      Register_Ada_Word (I, "PARSE-WORD", Parse_Word'Access);
      Register_Ada_Word (I, "PICK", Pick'Access);
      Register_Ada_Word (I, "+", Plus'Access);
      Register_Ada_Word (I, "+LOOP", Plus_Loop'Access, Immediate => True);
      Register_Ada_Word (I, "POSTPONE", Postpone'Access, Immediate => True);
      Register_Ada_Word (I, "QUIT", Quit'Access);
      Register_Ada_Word (I, "R@", R_At'Access);
      Register_Ada_Word (I, "RECURSE", Recurse'Access, Immediate => True);
      Register_Ada_Word (I, "REFILL", Refill'Access);
      Register_Ada_Word (I, "REPEAT", Repeat'Access, Immediate => True);
      Register_Ada_Word (I, "ROLL", Roll'Access);
      Register_Ada_Word (I, "RSHIFT", Rshift'Access);
      Register_Ada_Word (I, "S>D", S_To_D'Access);
      Register_Ada_Word (I, "*/MOD", ScaleMod'Access);
      Register_Ada_Word (I, "SEE", See'Access);
      Register_Ada_Word (I, ";", Semicolon'Access, Immediate => True);
      Register_Ada_Word (I, "IMMEDIATE", Set_Immediate'Access);
      Register_Ada_Word (I, "INLINE", Set_Inline'Access);
      Register_Ada_Word (I, "SKIP-BLANKS", Skip_Blanks'Access);
      Register_Ada_Word (I, "SM/REM", Sm_Slash_Rem'Access);
      Register_Ada_Word (I, "SWAP", Swap'Access);
      Register_Ada_Word (I, "!", Store'Access);
      Register_Ada_Word (I, "'", Tick'Access);
      Register_Ada_Word (I, "*", Times'Access);
      Register_Ada_Word (I, ">BODY", To_Body'Access);
      Register_Ada_Word (I, ">R", To_R'Access);
      Register_Ada_Word (I, "2/", Two_Div'Access);
      Register_Ada_Word (I, "2DUP", Two_Dup'Access);
      Register_Ada_Word (I, "2R@", Two_R_At'Access);
      Register_Ada_Word (I, "2>R", Two_To_R'Access);
      Register_Ada_Word (I, "U<", U_Smaller'Access);
      Register_Ada_Word (I, "UM/MOD", Um_Slash_Mod'Access);
      Register_Ada_Word (I, "UM*", Um_Star'Access);
      Register_Ada_Word (I, "UNLOOP", Unloop'Access);
      Register_Ada_Word (I, "UNUSED", Unused'Access);
      Register_Ada_Word (I, "WORD", Word'Access);
      Register_Ada_Word (I, "WORDS", Words'Access);

      for J in Forth.Builtins.Builtins'Range loop
         Interpret_Line (I, Forth.Builtins.Builtins (J) .all);
      end loop;

      Readline.Variables.Variable_Bind ("completion-ignore-case", "on");
   end Initialize;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (C : Character) return Boolean is
   begin
      return C <= ' ';
   end Is_Blank;

   ---------------
   -- Interpret --
   ---------------

   procedure Interpret (I : IT) is
   begin
      while not I.Interrupt loop
         declare
            W : constant String := Word (I);
            A : Action_Type;
            C : Cell;
         begin
            if W'Length = 0 then
               exit;
            end if;
            if I.State.all = 0 then
               begin
                  A := Find (I, W);
                  A.Immediate := True;
                  Execute_Action (I, A);
               exception
                  when NF : Word_Not_Found =>
                     begin
                        C := Parse_Number (I, W);
                     exception
                        when Constraint_Error =>
                           Reraise_Occurrence (NF);
                     end;
                     Push (I, C);
                  when Compile_Only =>
                     raise Compile_Only with W;
               end;
            else
               begin
                  A := Find (I, W);
                  if A.Immediate then
                     Execute_Action (I, A);
                  else
                     Add_To_Compilation_Buffer (I, A);
                  end if;
               exception
                  when NF : Word_Not_Found =>
                     begin
                        C := Parse_Number (I, W);
                     exception
                        when Constraint_Error =>
                           Reraise_Occurrence (NF);
                     end;
                     Add_To_Compilation_Buffer (I, C);
                  when Compile_Only =>
                     raise Compile_Only with W;
               end;
            end if;
         end;
      end loop;
   end Interpret;

   --------------------
   -- Interpret_Line --
   --------------------

   procedure Interpret_Line (I : IT; Line : String) is
      Saved_Count   : constant Cell := I.TIB_Count.all;
      Saved_Content : constant Byte_Array (1 .. TIB_Length) :=
        I.Memory (I.TIB .. I.TIB + TIB_Length - 1);
      Saved_Ptr     : constant Cell := I.IN_Ptr.all;
   begin
      I.Interrupt := False;
      Refill_Line (I, Line);
      Interpret (I);
      I.Memory (I.TIB .. I.TIB + TIB_Length - 1) := Saved_Content;
      I.TIB_Count.all := Saved_Count;
      I.IN_Ptr.all := Saved_Ptr;
   end Interpret_Line;

   --------------------
   -- Interpret_Mode --
   --------------------

   procedure Interpret_Mode (I : IT) is
   begin
      I.State.all := 0;
   end Interpret_Mode;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (I : IT) is
   begin
      I.Interrupt := True;
   end Interrupt;

   -------
   -- J --
   -------

   procedure J (I : IT) is
   begin
      if Length (I.Return_Stack) < 3 then
         raise Stack_Underflow;
      end if;
      Push (I, Element (I.Return_Stack, Length (I.Return_Stack) - 2));
   end J;

   ----------
   -- Jump --
   ----------

   procedure Jump (I : IT) is
   begin
      I.Current_IP := Pop (I);
   end Jump;

   -------------------
   -- Jump_If_False --
   -------------------

   procedure Jump_If_False (I : IT) is
      Target : constant Cell := Pop (I);
   begin
      if Pop (I) = 0 then
         I.Current_IP := Target;
      end if;
   end Jump_If_False;

   ---------
   -- Key --
   ---------

   procedure Key (I : IT) is
      C : Character;
   begin
      Get_Immediate (C);
      Push (I, Cell (Character'Pos (C)));
   end Key;

   -----------
   -- Leave --
   -----------

   procedure Leave (I : IT) is
   begin
      --  Look for Do_Loop_Reference on the stack

      for J in reverse 1 .. Length (I.Data_Stack) loop
         if Element (I.Data_Stack, J) = Do_Loop_Reference then

            --  Insert the leave information at the proper place

            Insert (I.Data_Stack, J - 1, Next_Index (I.Compilation_Buffer));
            Add_To_Compilation_Buffer (I, 0);
            Add_To_Compilation_Buffer (I, Jump'Access);
            return;
         end if;
      end loop;

      raise Unbalanced_Control_Structure;
   end Leave;

   -------------
   -- Literal --
   -------------

   procedure Literal (I : IT) is
   begin
      Add_To_Compilation_Buffer (I, Pop (I));
   end Literal;

   ------------
   -- Lshift --
   ------------

   procedure Lshift (I : IT) is
      U : constant Natural := Natural (Pop_Unsigned (I));
   begin
      Push (I, Pop (I) * 2 ** U);
   end Lshift;

   ---------------
   -- Main_Loop --
   ---------------

   procedure Main_Loop (I : IT) is
   begin
      loop
         Refill (I);
         Interpret (I);
      end loop;
   end Main_Loop;

   --------------------------------
   -- Make_And_Remember_Variable --
   --------------------------------

   procedure Make_And_Remember_Variable
     (I             : IT;
      Name          : String;
      Var           : out Cell_Access;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
   is
   begin
      Make_Variable (I, Name, Size, Initial_Value);
      Remember_Variable (I, Name, Var);
   end Make_And_Remember_Variable;

   --------------------------------
   -- Make_And_Remember_Variable --
   --------------------------------

   procedure Make_And_Remember_Variable
     (I             : IT;
      Name          : String;
      Var           : out Cell;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
   is
   begin
      Make_Variable (I, Name, Size, Initial_Value);
      Remember_Variable (I, Name, Var);
   end Make_And_Remember_Variable;

   -------------------
   -- Make_Variable --
   -------------------

   procedure Make_Variable
     (I             : IT;
      Name          : String;
      Size          : Cell := 4;
      Initial_Value : Cell := 0)
   is
   begin
      if Size = 4 then
         Align (I);
         Store (I, I.Here.all, Initial_Value);
      elsif Initial_Value /= 0 then
         raise Program_Error;
      end if;
      Start_Definition (I, Name);
      Add_To_Compilation_Buffer (I, I.Here.all);
      Semicolon (I);
      I.Here.all := I.Here.all + Size;
   end Make_Variable;

   --------
   -- MS --
   --------

   procedure MS (I : IT) is
   begin
      delay until Clock + Milliseconds (Integer (Pop (I)));
   end MS;

   -----------
   -- Mstar --
   -----------

   procedure Mstar (I : IT) is
   begin
      Push_64 (I, Integer_64 (Pop (I)) * Integer_64 (Pop (I)));
   end Mstar;

   --------------
   -- Negative --
   --------------

   procedure Negative (I : IT) is
   begin
      Push (I, Pop (I) < 0);
   end Negative;

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index (V : Compilation_Buffers.Vector) return Natural_Cell is
   begin
      return Last_Index (V) + 1;
   end Next_Index;

   ---------------------
   -- New_Interpreter --
   ---------------------

   function New_Interpreter
     (Memory_Size : Cell := 65536;
      Stack_Size  : Cell := 256)
     return IT is
   begin
      return I : constant IT := new Interpreter_Body (Memory_Size - 1) do
         New_Stack (I.Data_Stack, Stack_Size);
         New_Stack (I.Return_Stack, Stack_Size);
         Initialize (I);
      end return;
   end New_Interpreter;

   -----------
   -- Parse --
   -----------

   procedure Parse (I : IT)
   is
      Char : constant Unsigned_8 := Unsigned_8 (Pop (I));
   begin
      Push (I, I.TIB + I.IN_Ptr.all);
      for J in I.IN_Ptr.all .. I.TIB_Count.all - 1 loop
         if I.Memory (I.TIB + J) = Char then
            Push (I, J - I.IN_Ptr.all);
            I.IN_Ptr.all := J + 1;
            return;
         end if;
      end loop;
      Push (I, I.TIB_Count.all - I.IN_Ptr.all);
      I.IN_Ptr.all := I.TIB_Count.all;
   end Parse;

   ------------------
   -- Parse_Number --
   ------------------

   function Parse_Number (I : IT; S : String) return Cell
   is
      B           : constant Unsigned_32 := Unsigned_32 (I.Base.all);
      Negative    : Boolean := False;
      Sign_Parsed : Boolean := False;
      Result      : Unsigned_32 := 0;
   begin
      for I in S'Range loop
         declare
            C : Character renames S (I);
         begin
            if C = '+' then
               if Sign_Parsed then
                  raise Constraint_Error;
               end if;
            elsif C = '-' then
               if Sign_Parsed then
                  raise Constraint_Error;
               end if;
               Negative := not Negative;
            else
               declare
                  Digit : Unsigned_32;
               begin
                  Sign_Parsed := True;
                  if C >= '0' and C <= '9' then
                     Digit := Character'Pos (C) - Character'Pos ('0');
                  elsif C >= 'A' and C <= 'Z' then
                     Digit := 10 + Character'Pos (C) - Character'Pos ('A');
                  elsif C >= 'a' and C <= 'z' then
                     Digit := 10 + Character'Pos (C) - Character'Pos ('a');
                  else
                     raise Constraint_Error;
                  end if;
                  if Digit >= B then
                     raise Constraint_Error;
                  end if;
                  Result := Result * B + Digit;
               end;
            end if;
         end;
      end loop;
      if Negative then
         return -To_Cell (Result);
      else
         return To_Cell (Result);
      end if;
   end Parse_Number;

   ----------------
   -- Parse_Word --
   ----------------

   procedure Parse_Word (I : IT) is
      Origin : Cell;
   begin
      Skip_Blanks (I);
      Origin := I.IN_Ptr.all;
      Push (I, I.TIB + Origin);
      while I.IN_Ptr.all < I.TIB_Count.all loop
         declare
            C : constant Character :=
              Character'Val (I.Memory (I.TIB + I.IN_Ptr.all));
         begin
            I.IN_Ptr.all := I.IN_Ptr.all + 1;
            if Is_Blank (C) then
               Push (I, I.IN_Ptr.all - Origin - 1);
               return;
            end if;
         end;
      end loop;
      Push (I, I.IN_Ptr.all - Origin);
   end Parse_Word;

   ----------------
   -- Patch_Jump --
   ----------------

   procedure Patch_Jump (I : IT; To_Patch : Cell; Target : Cell) is
      pragma Assert (To_Patch < Next_Index (I.Compilation_Buffer));
      pragma Assert (Target <= Next_Index (I.Compilation_Buffer));
      Current : Action_Type := Element (I.Compilation_Buffer, To_Patch);
   begin
      Current.Value := Target;
      Replace_Element (I.Compilation_Buffer, To_Patch, Current);
   end Patch_Jump;

   ----------
   -- Peek --
   -----------

   function Peek (I : IT) return Cell is
   begin
      return Peek (I.Data_Stack);
   end Peek;

   ----------
   -- Pick --
   ----------

   procedure Pick (I : IT) is
      How_Deep : constant Integer := Integer (Pop (I));
   begin
      if How_Deep >= Length (I.Data_Stack) then
         raise Stack_Underflow;
      end if;
      Push (I, Element (I.Data_Stack, Length (I.Data_Stack) - How_Deep));
   end Pick;

   ----------
   -- Plus --
   ----------

   procedure Plus (I : IT) is
   begin
      Push (I, Pop (I) + Pop (I));
   end Plus;

   ---------------
   -- Plus_Loop --
   ---------------

   procedure Plus_Loop (I : IT) is
      To_Patch : Cell;
   begin
      Check_Control_Structure (I, Do_Loop_Reference);

      --  The standard says: "Add n to the loop index. If the loop
      --  index did not cross the boundary between the loop limit
      --  minus one and the loop limit, continue execution at the
      --  beginning of the loop. Otherwise, discard the current loop
      --  control parameters and continue execution immediately
      --  following the loop."
      --
      --  In Forth, that is:
      --    dup >r + >r 2dup >r >r >= swap 0< xor
      --    not if [beginning] then unloop

      Add_To_Compilation_Buffer (I, Dup'Access);
      Add_To_Compilation_Buffer (I, From_R'Access);
      Add_To_Compilation_Buffer (I, Plus'Access);
      Add_To_Compilation_Buffer (I, From_R'Access);
      Add_To_Compilation_Buffer (I, Two_Dup'Access);
      Add_To_Compilation_Buffer (I, To_R'Access);
      Add_To_Compilation_Buffer (I, To_R'Access);
      Add_To_Compilation_Buffer (I, Greaterequal'Access);
      Add_To_Compilation_Buffer (I, Swap'Access);
      Add_To_Compilation_Buffer (I, Negative'Access);
      Add_To_Compilation_Buffer (I, Forth_Xor'Access);
      Add_To_Compilation_Buffer (I, Pop (I));
      Add_To_Compilation_Buffer (I, Jump_If_False'Access);
      Add_To_Compilation_Buffer (I, Unloop'Access);

      --  Resolve forward references

      loop
         To_Patch := Pop (I);
         exit when To_Patch = Stack_Marker;
         Patch_Jump (I,
                     To_Patch => To_Patch,
                     Target   => Next_Index (I.Compilation_Buffer));
      end loop;
   end Plus_Loop;

   ---------
   -- Pop --
   ---------

   function Pop (I : IT) return Cell is
   begin
      return Pop (I.Data_Stack);
   end Pop;

   ------------
   -- Pop_64 --
   ------------

   function Pop_64 (I : IT) return Integer_64 is
   begin
      return To_Integer_64 (Pop_Unsigned_64 (I));
   end Pop_64;

   ------------------
   -- Pop_Unsigned --
   ------------------

   function Pop_Unsigned (I : IT) return Unsigned_32 is
   begin
      return To_Unsigned_32 (Pop (I));
   end Pop_Unsigned;

   ---------------------
   -- Pop_Unsigned_64 --
   ---------------------

   function Pop_Unsigned_64 (I : IT) return Unsigned_64 is
      High : constant Unsigned_64 := Unsigned_64 (Pop_Unsigned (I)) * 2 ** 32;
   begin
      return High + Unsigned_64 (Pop_Unsigned (I));
   end Pop_Unsigned_64;

   --------------
   -- Postpone --
   --------------

   procedure Postpone (I : IT) is
      W      : constant String := Word (I);
      Action : Action_Type;
   begin
      Action := Find (I, W);
      if Action.Immediate then
         Add_To_Compilation_Buffer (I, Action);
      else
         Add_To_Compilation_Buffer (I, Action.Forth_Proc);
         Add_To_Compilation_Buffer (I, Compile_Comma'Access);
      end if;
   exception
      when Word_Not_Found =>
         begin
            Add_To_Compilation_Buffer (I, Parse_Number (I, W));
         exception
            when Constraint_Error =>
               Raise_Word_Not_Found (W);
         end;
   end Postpone;

   ----------
   -- Push --
   ----------

   procedure Push (I : IT; X : Cell) is
   begin
      Push (I.Data_Stack, X);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (I : IT; B : Boolean) is
   begin
      if B then
         Push (I, -1);
      else
         Push (I, 0);
      end if;
   end Push;

   -------------
   -- Push_64 --
   -------------

   procedure Push_64 (I : IT; X : Integer_64) is
   begin
      Push_Unsigned_64 (I, To_Unsigned_64 (X));
   end Push_64;

   -------------------
   -- Push_Unsigned --
   -------------------

   procedure Push_Unsigned (I : IT; X : Unsigned_32) is
   begin
      Push (I, To_Cell (X));
   end Push_Unsigned;

   ----------------------
   -- Push_Unsigned_64 --
   ----------------------

   procedure Push_Unsigned_64 (I : IT; X : Unsigned_64) is
   begin
      Push_Unsigned (I, Unsigned_32 (X mod (2 ** 32)));
      Push_Unsigned (I, Unsigned_32 (X / 2 ** 32));
   end Push_Unsigned_64;

   ----------
   -- Quit --
   ----------

   procedure Quit (I : IT) is
   begin
      loop
         Clear (I.Data_Stack);
         Clear (I.Return_Stack);
         Interpret_Mode (I);
         begin
            Main_Loop (I);
         exception
            when Bye_Exception =>
               return;
            when End_Error =>
               return;
            when NF : Word_Not_Found =>
               Put_Line ("*** Word not found: " & Exception_Message (NF));
            when Stack_Overflow =>
               Put_Line ("*** Stack overflow");
            when Stack_Underflow =>
               Put_Line ("*** Stack underflow");
            when CO : Compile_Only =>
               Put_Line ("*** Compile only: " & Exception_Message (CO));
            when Name_Error =>
               --  This exception has already been handled and is getting
               --  reraised.
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

   procedure R_At (I : IT) is
   begin
      Push (I, Peek (I.Return_Stack));
   end R_At;

   -------------
   -- Recurse --
   -------------

   procedure Recurse (I : IT) is
   begin
      Add_To_Compilation_Buffer (I, I.Current_Action);
   end Recurse;

   ------------
   -- Refill --
   ------------

   procedure Refill (I : IT) is
   begin
      if I.Use_RL then
         if I.State.all = 0 then
            Cr (I);
            Refill_Line (I, Readline.Read_Line ("ok> "));
         else
            Refill_Line (I, Readline.Read_Line ("] "));
         end if;
      else
         declare
            Buffer : String (1 .. TIB_Length);
            Last   : Natural;
         begin
            Get_Line (Buffer, Last);
            Refill_Line (I, Buffer (1 .. Last));
         end;
      end if;
   end Refill;

   -----------------
   -- Refill_Line --
   -----------------

   procedure Refill_Line (I : IT; Buffer : String) is
      Last : constant Natural := Natural'Min (Buffer'Length, TIB_Length);
   begin
      for J in 1 .. Integer'Min (Buffer'Length, TIB_Length) loop
         I.Memory (I.TIB + Cell (J) - 1) := Character'Pos (Buffer (J));
      end loop;
      I.TIB_Count.all := Cell (Last);
      I.IN_Ptr.all := 0;
   end Refill_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (I      : IT;
      Name   : String;
      Action : Action_Type)
   is
   begin
      Append (I.Dict, (Name   => To_Unbounded_String (Name),
                       Action => Action));
      Readline.Completion.Add_Word (Name);
   end Register;

   -----------------------
   -- Register_Ada_Word --
   -----------------------

   procedure Register_Ada_Word
     (I         : IT;
      Name      : String;
      Word      : Ada_Word_Access;
      Immediate : Boolean := False)
   is
   begin
      --  Create a Forth wrapper around an Ada word so that its address
      --  can be taken and passed to EXECUTE.

      Start_Definition (I, Name);
      Add_To_Compilation_Buffer (I, Word);
      Semicolon (I);
      if Immediate then
         Set_Immediate (I);
      end if;
      Set_Inline (I);
   end Register_Ada_Word;

   -----------------------
   -- Register_Constant --
   -----------------------

   procedure Register_Constant
     (I     : IT;
      Name  : String;
      Value : Cell)
   is
   begin
      Start_Definition (I, Name);
      Add_To_Compilation_Buffer (I, Value);
      Semicolon (I);
   end Register_Constant;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (I    : IT;
      Name : String;
      Var  : out Cell_Access)
   is
   begin
      Tick (I, Name);
      To_Body (I);
      pragma Warnings (Off);
      Var := To_Cell_Access (I.Memory (Pop (I)) 'Access);
      pragma Warnings (On);
   end Remember_Variable;

   -----------------------
   -- Remember_Variable --
   -----------------------

   procedure Remember_Variable
     (I    : IT;
      Name : String;
      Var  : out Cell)
   is
   begin
      Tick (I, Name);
      To_Body (I);
      Var := Pop (I);
   end Remember_Variable;

   ------------
   -- Repeat --
   ------------

   procedure Repeat (I : IT) is
   begin
      Check_Control_Structure (I, Backward_Reference);
      Literal (I);
      Add_To_Compilation_Buffer (I, Jump'Access);
      loop
         declare
            To_Fix : constant Cell := Pop (I);
         begin
            exit when To_Fix = Stack_Marker;
            Patch_Jump (I, To_Fix, Next_Index (I.Compilation_Buffer));
         end;
      end loop;
   end Repeat;

   ----------
   -- Roll --
   ----------

   procedure Roll (I : IT) is
      Offset : constant Integer  := Integer (Pop (I));
      Index  : constant Positive := Length (I.Data_Stack) - Offset;
   begin
      Push (I.Data_Stack, Element (I.Data_Stack, Index));
      Delete (I.Data_Stack, Index);
   end Roll;

   ------------
   -- Rshift --
   ------------

   procedure Rshift (I : IT) is
      U : constant Natural := Natural (Pop_Unsigned (I));
   begin
      Push_Unsigned (I, Pop_Unsigned (I) / 2 ** U);
   end Rshift;

   ------------
   -- S_To_D --
   ------------

   procedure S_To_D (I : IT) is
   begin
      Push_64 (I, Integer_64 (Pop (I)));
   end S_To_D;

   --------------
   -- ScaleMod --
   --------------

   procedure ScaleMod (I : IT) is
   begin
      To_R (I);
      Mstar (I);
      From_R (I);
      Sm_Slash_Rem (I);
   end ScaleMod;

   ---------
   -- See --
   ---------

   procedure See (I : IT) is
      Index  : Cell;
      Action : Action_Type;
      Found  : Boolean;
   begin
      Tick (I);
      Index := Pop (I);
      loop
         Found := False;
         Put (Cell'Image (Index) & ": ");
         Action := Element (I.Compilation_Buffer, Index);
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
               for J in
                 reverse First_Index (I.Dict) .. Last_Index (I.Dict) loop
                  declare
                     Current : Dictionary_Entry renames Element (I.Dict, J);
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
                  for J in
                    reverse First_Index (I.Dict) .. Last_Index (I.Dict) loop
                     declare
                        Current : Dictionary_Entry renames Element (I.Dict, J);
                     begin
                        if Current.Action.Kind = Forth_Word then
                           declare
                              Idx : constant Cell :=
                                Current.Action.Forth_Proc;
                              A : constant Action_Type :=
                                Element (I.Compilation_Buffer, Idx);
                           begin
                              if A.Kind = Ada_Word and then
                                A.Ada_Proc = Action.Ada_Proc and then
                                Element (I.Compilation_Buffer, Idx + 1) =
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

   procedure Semicolon (I : IT) is
   begin
      Check_Control_Structure (I, Definition_Reference);
      Add_To_Compilation_Buffer (I, Forth_Exit);

      --  Current_Name can be null during definition or completion of
      --  a DOES> prefix.

      if I.Current_Name /= "" then
         Register (I, To_String (I.Current_Name), I.Current_Action);
         I.Current_Name := To_Unbounded_String ("");
      end if;

      Interpret_Mode (I);
   end Semicolon;

   -------------------
   -- Set_Immediate --
   -------------------

   procedure Set_Immediate (I : IT) is
      Current : Dictionary_Entry := Last_Element (I.Dict);
   begin
      Current.Action.Immediate := True;
      Replace_Element (I.Dict, Last_Index (I.Dict), Current);
   end Set_Immediate;

   ----------------
   -- Set_Inline --
   ----------------

   procedure Set_Inline (I : IT) is
      Current : Dictionary_Entry := Last_Element (I.Dict);
   begin
      Current.Action.Inline := True;
      Replace_Element (I.Dict, Last_Index (I.Dict), Current);
   end Set_Inline;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (I : IT) is
   begin
      while I.IN_Ptr.all < I.TIB_Count.all loop
         exit when
           not Is_Blank (Character'Val (I.Memory (I.TIB + I.IN_Ptr.all)));
         I.IN_Ptr.all := I.IN_Ptr.all + 1;
      end loop;
   end Skip_Blanks;

   ------------------
   -- Sm_Slash_Rem --
   ------------------

   procedure Sm_Slash_Rem (I : IT) is
      N : constant Integer_64 := Integer_64 (Pop (I));
      D : constant Integer_64 := Pop_64 (I);
      R : constant Integer_64 := D rem N;
   begin
      Push (I, Cell (R));
      Push_64 (I, (D - R) / N);
      Drop (I);
   end Sm_Slash_Rem;

   ----------------------
   -- Start_Definition --
   ----------------------

   procedure Start_Definition (I : IT; Name : String := "") is
   begin
      if Name /= "" then
         I.Current_Name := To_Unbounded_String (Name);
      end if;
      I.Current_Action.Immediate  := False;
      I.Current_Action.Forth_Proc := Next_Index (I.Compilation_Buffer);
      Compile_Mode (I);
      Push (I, Definition_Reference);
   end Start_Definition;

   -----------
   -- Store --
   -----------

   procedure Store (I : IT)
   is
      pragma Warnings (Off);
      Addr  : constant Cell_Access :=
        To_Cell_Access (I.Memory (Pop (I))'Access);
      pragma Warnings (On);
   begin
      Addr.all := Pop (I);
   end Store;

   -----------
   -- Store --
   -----------

   procedure Store (I : IT; Addr : Cell; Value : Cell) is
   begin
      Push (I, Value);
      Push (I, Addr);
      Store (I);
   end Store;

   ----------
   -- Swap --
   ----------

   procedure Swap (I : IT)
   is
      A : constant Cell := Pop (I);
      B : constant Cell := Pop (I);
   begin
      Push (I, A);
      Push (I, B);
   end Swap;

   ----------
   -- Tick --
   ----------

   procedure Tick (I : IT; Name : String) is
      A : constant Action_Type := Find (I, Name);
   begin
      Push (I, A.Forth_Proc);
   end Tick;

   ----------
   -- Tick --
   ----------

   procedure Tick (I : IT) is
   begin
      Tick (I, Word (I));
   end Tick;

   -----------
   -- Times --
   -----------

   procedure Times (I : IT) is
   begin
      Push (I, Pop (I) * Pop (I));
   end Times;

   -------------
   -- To_Body --
   -------------

   procedure To_Body (I : IT) is
   begin
      Push (I, Element (I.Compilation_Buffer, Pop (I)) .Value);
   end To_Body;

   ----------
   -- To_R --
   ----------

   procedure To_R (I : IT) is
   begin
      Push (I.Return_Stack, Pop (I));
   end To_R;

   ---------------
   -- To_String --
   ---------------

   function To_String (I : IT) return String is
      Length : constant Natural    := Natural (Pop (I));
      Addr   : Cell          := Pop (I);
      Result : String (1 .. Length);
   begin
      for J in Result'Range loop
         Result (J) := Character'Val (Cfetch (I, Addr));
         Addr := Addr + 1;
      end loop;
      return Result;
   end To_String;

   -------------
   -- Two_Div --
   -------------

   procedure Two_Div (I : IT) is
      A : constant Cell := Pop (I);
      B : Unsigned_32   := To_Unsigned_32 (A) / 2;
   begin
      if A < 0 then
         B := B or (2 ** 31);
      end if;
      Push_Unsigned (I, B);
   end Two_Div;

   -------------
   -- Two_Dup --
   -------------

   procedure Two_Dup (I : IT) is
      A : constant Cell := Pop (I);
      B : constant Cell := Pop (I);
   begin
      Push (I, B);
      Push (I, A);
      Push (I, B);
      Push (I, A);
   end Two_Dup;

   --------------
   -- Two_R_At --
   --------------

   procedure Two_R_At (I : IT) is
   begin
      Push (I, Element (I.Return_Stack, Length (I.Return_Stack) - 1));
      Push (I, Peek (I.Return_Stack));
   end Two_R_At;

   --------------
   -- Two_To_R --
   --------------

   procedure Two_To_R (I : IT) is
   begin
      Swap (I);
      To_R (I);
      To_R (I);
   end Two_To_R;

   ---------------
   -- U_Smaller --
   ---------------

   procedure U_Smaller (I : IT) is
      R : constant Unsigned_32 := Pop_Unsigned (I);
   begin
      Push (I, Pop_Unsigned (I) < R);
   end U_Smaller;

   ------------------
   -- Um_Slash_Mod --
   ------------------

   procedure Um_Slash_Mod (I : IT) is
      N : constant Unsigned_64 := Unsigned_64 (Pop_Unsigned (I));
      D : constant Unsigned_64 := Pop_Unsigned_64 (I);
   begin
      Push_Unsigned (I, Unsigned_32 (D mod N));
      Push_Unsigned_64 (I, D / N);
      Drop (I);
   end Um_Slash_Mod;

   -------------
   -- Um_Star --
   -------------

   procedure Um_Star (I : IT) is
   begin
      Push_Unsigned_64 (I, Unsigned_64 (Pop_Unsigned (I)) *
                          Unsigned_64 (Pop_Unsigned (I)));
   end Um_Star;

   ------------
   -- Unloop --
   ------------

   procedure Unloop (I : IT) is
   begin
      Delete_Last (I.Return_Stack);
      Delete_Last (I.Return_Stack);
   end Unloop;

   ------------
   -- Unused --
   ------------

   procedure Unused (I : IT) is
   begin
      Push (I, I.Memory'Last - I.Here.all + 1);
   end Unused;

   ----------
   -- Word --
   ----------

   procedure Word (I : IT) is
      Length : Cell;
      Addr   : Cell;
   begin
      Parse (I);
      Length := Pop (I);
      Addr   := Pop (I);
      I.Memory (Addr - 1) := Unsigned_8 (Length);
      Push (I, Addr - 1);
   end Word;

   ----------
   -- Word --
   ----------

   function Word (I : IT) return String is
   begin
      Parse_Word (I);
      return To_String (I);
   end Word;

   -----------
   -- Words --
   -----------

   procedure Words (I : IT) is
      Len : Natural := 0;
   begin
      for J in First_Index (I.Dict) .. Last_Index (I.Dict) loop
         declare
            Current : Dictionary_Entry renames Element (I.Dict, J);
         begin
            Len := Len + Length (Current.Name) + 1;
            if Len > 75 then
               New_Line;
               Len := Length (Current.Name);
            elsif J /= First_Index (I.Dict) then
               Put (' ');
            end if;
            Put (To_String (Current.Name));
         end;
      end loop;
   end Words;

end Forth.Interpreter;
