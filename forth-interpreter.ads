with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Forth.Stacks;
with Forth.Types;            use Forth.Types;
with Interfaces;             use Interfaces;

generic package Forth.Interpreter is

   pragma Elaborate_Body;

   type Cell_Access is access all Cell;
   type Ada_Word_Access is access procedure;
   --  Those two types have to be declared inside the generic package

   type Interpreter_Type is private;

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

   procedure Register_Ada_Word
     (Name      : String;
      Word      : Ada_Word_Access;
      Immediate : Boolean := False);

   procedure Register_Constant
     (Name  : String;
      Value : Cell);

   Bye_Exception : exception;

   procedure Include_File (File_Name : String);
   --  This may raise Ada.IO_Exceptions.Name_Error if the file cannot be found,
   --  or Bye_Exception if the "BYE" word is used while reading the file.

   procedure Interpret_Line (Line : String);

   --  Predefined Ada words
   procedure Again;
   procedure Ahead;
   procedure Align;
   procedure Bye;
   procedure Cfetch;
   procedure Colon;
   procedure Colon_Noname;
   procedure Compile_Comma;
   procedure Compile_Exit;
   procedure Compile_Mode;
   procedure Count;
   procedure Cr;
   procedure Cstore;
   procedure D_Abs;
   procedure D_Equal;
   procedure D_Max;
   procedure D_Min;
   procedure D_Minus;
   procedure D_Plus;
   procedure D_Smaller;
   procedure D_Two_Div;
   procedure D_Two_Times;
   procedure Depth;
   procedure DivMod;
   procedure Does;
   procedure Drop;
   procedure Dup;
   procedure Emit;
   procedure Equal;
   procedure Evaluate;
   procedure Execute;
   procedure Fetch;
   procedure Find;
   procedure Fm_Slash_Mod;
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
   procedure Negative;
   procedure Parse;
   procedure Parse_Word;
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
   procedure Two_Div;
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

   use Forth.Stacks;

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

   subtype Natural_Cell is Cell range 1 .. Cell'Last;
   package Compilation_Buffers is
      new Ada.Containers.Vectors (Natural_Cell, Action_Type);
   use Compilation_Buffers;

   Compilation_Buffer : Compilation_Buffers.Vector;

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

   package Stacks is
      new Ada.Containers.Vectors (Positive, Cell);
   use Stacks;
   Data_Stack   : constant Stack_Type := New_Stack;
   Return_Stack : constant Stack_Type := New_Stack;

   Here      : Cell_Access;
   Base      : Cell_Access;
   TIB       : Cell;
   TIB_Count : Cell_Access;
   IN_Ptr    : Cell_Access;
   State     : Cell_Access;

   TIB_Length : constant := 1024;

   Stack_Marker       : constant   := -1;
   Forward_Reference  : constant   := -100;
   Backward_Reference : constant   := -101;
   Do_Loop_Reference  : constant   := -102;
   Definition_Reference : constant := -103;

   Current_Name   : Unbounded_String;
   Current_Action : Action_Type (Forth_Word);
   Current_IP     : Cell := -1;

   Use_RL         : Boolean := True;
   --  Should the current input method use Read_Line?

   type Interpreter_Type is record
      Data_Stack         : Stack_Type := New_Stack;
      Return_Stack       : Stack_Type := New_Stack;
      Compilation_Buffer : Compilation_Buffers.Vector;
      Dict               : Dictionaries.Vector;
      Memory             : Byte_Array (0 .. 65535);
      Here               : Cell_Access;
      Base               : Cell_Access;
      TIB                : Cell;
      TIB_Count          : Cell_Access;
      IN_Ptr             : Cell_Access;
      State              : Cell_Access;
      Current_Name       : Unbounded_String;
      Current_Action     : Action_Type (Forth_Word);
      Current_IP         : Cell := -1;
      Use_RL             : Boolean := True;
   end record;

end Forth.Interpreter;
