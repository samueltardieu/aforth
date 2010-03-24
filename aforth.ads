with Interfaces; use Interfaces;

package Aforth is

   pragma Elaborate_Body;

   type Cell is new Integer_32;

   type Cell_Array is array (Positive range <>) of Cell;

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   procedure Push (X : Cell);
   procedure Push_Unsigned (X : Unsigned_32);
   procedure Push_Unsigned_64 (X : Unsigned_64);
   procedure Push_64 (X : Integer_64);
   procedure Push (B : Boolean);
   function Pop return Cell;
   function Pop_Unsigned return Unsigned_32;
   function Pop_64 return Integer_64;
   function Pop_Unsigned_64 return Unsigned_64;
   --  Shortcut operating on Data_Stack

   type Stack_Type is limited private;
   type Stack_Access is access Stack_Type;

   Data_Stack   : Stack_Access;
   Return_Stack : Stack_Access;

   type Cell_Access is access all Cell;

   procedure Make_And_Remember_Variable
     (Name          : String;
      Var           : out Cell_Access;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   procedure Make_And_Remember_Variable
     (Name          : String;
      Var           : out Cell;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   function Fetch (Addr : Cell) return Cell;
   function Cfetch (Addr : Cell) return Cell;
   procedure Store (Addr : Cell; Value : Cell);

   procedure Make_Variable
     (Name          : String;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   type Ada_Word_Access is access procedure;

   procedure Register_Ada_Word
     (Name      : String;
      Word      : Ada_Word_Access;
      Immediate : Boolean := False);

   procedure Register_Constant
     (Name  : String;
      Value : Cell);

   procedure Include_File (File_Name : String);

   procedure Interpret_Line (Line : String);

   Compile_Only                 : exception;
   Unbalanced_Control_Structure : exception;

   --  Predefined Ada words
   procedure Again;
   procedure Ahead;
   procedure Align;
   procedure Cfetch;
   procedure Colon;
   procedure Colon_Noname;
   procedure Comma;
   procedure Compile_Comma;
   procedure Compile_Mode;
   procedure Cr;
   procedure Cstore;
   procedure Depth;
   procedure DivMod;
   procedure Does;
   procedure Dot;
   procedure Drop;
   procedure Emit;
   procedure Execute;
   procedure Fetch;
   procedure Forth_And;
   procedure Forth_Begin;
   procedure Forth_Do;
   procedure Forth_If;
   procedure Forth_Or;
   procedure Forth_Then;
   procedure Forth_While;
   procedure Forth_Xor;
   procedure From_R;
   procedure Greaterequal;
   procedure Include;
   procedure Interpret;
   procedure Interpret_Mode;
   procedure J;
   procedure Key;
   procedure Leave;
   procedure Literal;
   procedure Lshift;
   procedure MS;
   procedure Mstar;
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
   procedure Store;
   procedure Swap;
   procedure Tick;
   procedure Times;
   procedure To_Body;
   procedure To_R;
   procedure Two_Dup;
   procedure Two_R_At;
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
      Data : Cell_Array (1 .. Stack_Max_Depth);
      Top  : Natural range 0 .. Stack_Max_Depth := 0;
   end record;

end Aforth;
