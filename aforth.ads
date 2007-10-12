with Interfaces; use Interfaces;

package Aforth is

   pragma Elaborate_Body;

   type Integer_32_Array is array (Positive range <>) of Integer_32;

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   procedure Push (X : in Integer_32);
   procedure Push_Unsigned (X : in Unsigned_32);
   procedure Push_Unsigned_64 (X : in Unsigned_64);
   procedure Push_64 (X : in Integer_64);
   procedure Push (B : in Boolean);
   function Pop return Integer_32;
   function Pop_Unsigned return Unsigned_32;
   function Pop_64 return Integer_64;
   function Pop_Unsigned_64 return Unsigned_64;
   --  Shortcut operating on Data_Stack

   type Stack_Type is limited private;
   type Stack_Access is access Stack_Type;

   Data_Stack   : Stack_Access;
   Return_Stack : Stack_Access;

   type Integer_32_Access is access all Integer_32;

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

   function Fetch (Addr : Integer_32) return Integer_32;
   function Cfetch (Addr : Integer_32) return Integer_32;
   procedure Store (Addr : in Integer_32; Value : in Integer_32);

   procedure Make_Variable
     (Name          : in String;
      Size          : in Integer_32 := 4;
      Initial_Value : in Integer_32 := 0);

   type Ada_Word_Access is access procedure;

   procedure Register_Ada_Word
     (Name      : in String;
      Word      : in Ada_Word_Access;
      Immediate : in Boolean := False);

   procedure Register_Constant
     (Name  : in String;
      Value : in Integer_32);

   procedure Include_File (File_Name : in String);

   procedure Interpret_Line (Line : in String);

   Compile_Only                 : exception;
   Unbalanced_Control_Structure : exception;

   --  Predefined Ada words
   procedure Again;
   procedure Ahead;
   procedure Align;
   procedure Bl;
   procedure Bounds;
   procedure Char;
   procedure Ccomma;
   procedure Cfetch;
   procedure Colon;
   procedure Colon_Noname;
   procedure Comma;
   procedure Compile_Comma;
   procedure Compile_Mode;
   procedure Cquote;
   procedure Cr;
   procedure Create;
   procedure Cstore;
   procedure Depth;
   procedure DivMod;
   procedure Does;
   procedure Dot;
   procedure Drop;
   procedure Dup;
   procedure Emit;
   procedure Equal;
   procedure Execute;
   procedure Fetch;
   procedure Forth_And;
   procedure Forth_Begin;
   procedure Forth_Do;
   procedure Forth_Else;
   procedure Forth_If;
   procedure Forth_Or;
   procedure Forth_Then;
   procedure Forth_Type;
   procedure Forth_While;
   procedure Forth_Xor;
   procedure From_R;
   procedure Greater;
   procedure Greaterequal;
   procedure Include;
   procedure Interpret;
   procedure Interpret_Mode;
   procedure J;
   procedure Key;
   procedure Leave;
   procedure Literal;
   procedure Lshift;
   procedure Minus;
   procedure Minusstore;
   procedure Mstar;
   procedure Over;
   procedure Parse;
   procedure Pick;
   procedure Plus;
   procedure Plus_Loop;
   procedure Postpone;
   procedure Quit;
   procedure R_At;
   procedure Recurse;
   procedure Refill;
   procedure Repeat;
   procedure Roll;
   procedure Rshift;
   procedure S_To_D;
   procedure ScaleMod;
   procedure See;
   procedure Semicolon;
   procedure Set_Immediate;
   procedure Set_Inline;
   procedure Skip_Blanks;
   procedure Sm_Slash_Rem;
   procedure Smaller;
   procedure Smallerequal;
   procedure Space;
   procedure Squote;
   procedure Store;
   procedure Swap;
   procedure Tick;
   procedure Times;
   procedure To_Body;
   procedure To_R;
   procedure Two_Dup;
   procedure Two_From_R;
   procedure Two_R_At;
   procedure Two_Swap;
   procedure Two_To_R;
   procedure U_Smaller;
   procedure Um_Slash_Mod;
   procedure Um_Star;
   procedure Unloop;
   procedure Unused;
   procedure Word;
   procedure Words;

private

   Stack_Max_Depth : constant := 50;

   type Stack_Type is limited record
      Data : Integer_32_Array (1 .. Stack_Max_Depth);
      Top  : Natural range 0 .. Stack_Max_Depth := 0;
   end record;

end Aforth;
