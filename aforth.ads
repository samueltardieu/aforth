with Interfaces; use Interfaces;

package Aforth is

   pragma Elaborate_Body;

   type Action_Kind is (Ada_Word, Forth_Word, Number);

   type Ada_Word_Access is access procedure;

   type Forth_Word_Array;

   type Forth_Word_Access is access Forth_Word_Array;

   type Action_Type (Kind : Action_Kind := Number) is record
      Immediate : Boolean;
      case Kind is
         when Ada_Word =>
            Ada_Proc   : Ada_Word_Access;
         when Forth_Word =>
            Forth_Proc : Integer_32;
         when Number =>
            Value      : Integer_32;
      end case;
   end record;

   type Forth_Word_Array is array (Positive range <>) of Action_Type;

   type String_Access is access String;

   type Dictionary_Entry is record
      Name   : String_Access;
      Action : Action_Type;
   end record;

   type Dictionary_Array is array (Positive range <>) of Dictionary_Entry;

   type Dictionary_Access is access Dictionary_Array;

   procedure Register (Name   : in String;
                       Action : in Action_Type);

   procedure Set_Last_Immediate (Dict : in Dictionary_Access);

   Not_Found : exception;

   function Find (Dict : Dictionary_Access; Name : String) return Action_Type;
   --  May raise Not_Found

   type Integer_32_Array is array (Positive range <>) of Integer_32;

   Stack_Overflow  : exception;
   Stack_Underflow : exception;

   type Stack_Type is record
      Data : Integer_32_Array (1 .. 50);
      Top  : Natural range 0 .. 50 := 0;
   end record;

   procedure Push (S : access Stack_Type; X : in Integer_32);
   --  May raise stack overflow

   function Pop (S : access Stack_Type) return Integer_32;
   --  May raise stack underflow

   procedure Push (X : in Integer_32);
   procedure Push (B : in Boolean);
   function Pop return Integer_32;
   --  Shortcut operating on Data_Stack

   type Stack_Access is access Stack_Type;

   Data_Stack   : Stack_Access;
   Return_Stack : Stack_Access;
   Dict         : Dictionary_Access;

   type Byte_Array is array (Integer_32 range <>) of aliased Unsigned_8;

   Memory : Byte_Array (0 .. 65535) := (others => 0);

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

   procedure Register_Ada_Word
     (Name      : in String;
      Word      : in Ada_Word_Access;
      Immediate : in Boolean := False);

   procedure Register_Constant
     (Name  : in String;
      Value : in Integer_32);

   Compilation_Buffer : array (Integer_32'(1) .. 16384) of Action_Type;
   Compilation_Index  : Integer_32 := 1;

   procedure Add_To_Compilation_Buffer (Action : in Action_Type);

   procedure Include_File (File_Name : in String);

   procedure Refill_Line (Buffer : in String);

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
   procedure Comma;
   procedure Compile_Mode;
   procedure Cr;
   procedure Cstore;
   procedure Depth;
   procedure Div;
   procedure DivMod;
   procedure Dot;
   procedure DotQuote;
   procedure DotS;
   procedure Drop;
   procedure Dup;
   procedure Emit;
   procedure Equal;
   procedure Fetch;
   procedure Forth_Begin;
   procedure Forth_Else;
   procedure Forth_If;
   procedure Forth_Mod;
   procedure Forth_Then;
   procedure Forth_Type;
   procedure Forth_Until;
   procedure Forth_While;
   procedure Greater;
   procedure Greaterequal;
   procedure Ichar;
   procedure Immediate;
   procedure Include;
   procedure Interpret_Mode;
   procedure Literal;
   procedure Minus;
   procedure Minusstore;
   procedure Ms;
   procedure Nip;
   procedure Notequal;
   procedure Oneminus;
   procedure Oneplus;
   procedure Over;
   procedure Parse;
   procedure Plus;
   procedure Plusstore;
   procedure Postpone;
   procedure Quit;
   procedure Recurse;
   procedure Refill;
   procedure Repeat;
   procedure Scale;
   procedure Semicolon;
   procedure Skip_Blanks;
   procedure Smaller;
   procedure Smallerequal;
   procedure Space;
   procedure Squote;
   procedure Store;
   procedure Swap;
   procedure Times;
   procedure Twodrop;
   procedure Twodup;
   procedure Word;
   procedure Zeroequal;
   procedure Zerogreater;
   procedure Zerogreaterequal;
   procedure Zeronotequal;
   procedure Zerosmaller;
   procedure Zerosmallerequal;

end Aforth;
