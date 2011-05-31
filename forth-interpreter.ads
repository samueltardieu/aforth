------------------------------------------------------------------------------
--                                                                          --
--                            AFORTH COMPONENTS                             --
--                                                                          --
--                    F O R T H . I N T E R P R E T E R                     --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Forth.Stacks;
with Forth.Types;            use Forth.Types;
with Interfaces;             use Interfaces;

package Forth.Interpreter is

   pragma Elaborate_Body;

   type Interpreter_Type is private;

   subtype IT is Interpreter_Type;
   --  Shortcut

   type Cell_Access is access all Cell;
   pragma No_Strict_Aliasing (Cell_Access);

   type Ada_Word_Access is access procedure (I : IT);

   function New_Interpreter
     (Memory_Size : Cell := 65536;
      Stack_Size  : Cell := 256)
     return IT;
   --  Memory size is in bytes, stack size is in cells. Both data and return
   --  stacks are bounded to avoid runaway memory exhaustion.

   procedure Free_Interpreter (I : in out IT);
   --  Reclaim the memory used by the interpreter. After this call, the
   --  interpreter cannot be used anymore.

   procedure Push (I : IT; X : Cell);
   procedure Push_Unsigned (I : IT; X : Unsigned_32);
   procedure Push_Unsigned_64 (I : IT; X : Unsigned_64);
   procedure Push_64 (I : IT; X : Integer_64);
   procedure Push (I : IT; B : Boolean);
   function Pop (I : IT) return Cell;
   function Pop_Unsigned (I : IT) return Unsigned_32;
   function Pop_64 (I : IT) return Integer_64;
   function Pop_Unsigned_64 (I : IT) return Unsigned_64;
   --  Shortcut operating on Data_Stack

   procedure Make_And_Remember_Variable
     (I             : IT;
      Name          : String;
      Var           : out Cell_Access;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   procedure Make_And_Remember_Variable
     (I             : IT;
      Name          : String;
      Var           : out Cell;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   function Fetch (I : IT; Addr : Cell) return Cell;
   function Cfetch (I : IT; Addr : Cell) return Cell;
   procedure Store (I : IT; Addr : Cell; Value : Cell);

   procedure Make_Variable
     (I             : IT;
      Name          : String;
      Size          : Cell := 4;
      Initial_Value : Cell := 0);

   procedure Register_Ada_Word
     (I         : IT;
      Name      : String;
      Word      : Ada_Word_Access;
      Immediate : Boolean := False);

   procedure Register_Constant
     (I     : IT;
      Name  : String;
      Value : Cell);

   procedure Include_File (I : IT; File_Name : String);
   --  This may raise Ada.IO_Exceptions.Name_Error if the file cannot be found,
   --  or Bye_Exception if the "BYE" word is used while reading the file.

   procedure Interpret_Line (I : IT; Line : String);

   procedure Interrupt (I : IT);

   --  Predefined Ada words
   procedure Again (I : IT);
   procedure Ahead (I : IT);
   procedure Align (I : IT);
   procedure Bye (I : IT);
   procedure Cfetch (I : IT);
   procedure Colon (I : IT);
   procedure Colon_Noname (I : IT);
   procedure Compile_Comma (I : IT);
   procedure Compile_Exit (I : IT);
   procedure Compile_Mode (I : IT);
   procedure Count (I : IT);
   procedure Cr (I : IT);
   procedure Cstore (I : IT);
   procedure D_Abs (I : IT);
   procedure D_Equal (I : IT);
   procedure D_Max (I : IT);
   procedure D_Min (I : IT);
   procedure D_Minus (I : IT);
   procedure D_Plus (I : IT);
   procedure D_Smaller (I : IT);
   procedure D_Two_Div (I : IT);
   procedure D_Two_Times (I : IT);
   procedure Depth (I : IT);
   procedure DivMod (I : IT);
   procedure Does (I : IT);
   procedure Drop (I : IT);
   procedure Dup (I : IT);
   procedure Emit (I : IT);
   procedure Equal (I : IT);
   procedure Evaluate (I : IT);
   procedure Execute (I : IT);
   procedure Fetch (I : IT);
   procedure Find (I : IT);
   procedure Fm_Slash_Mod (I : IT);
   procedure Forth_And (I : IT);
   procedure Forth_Begin (I : IT);
   procedure Forth_Do (I : IT);
   procedure Forth_If (I : IT);
   procedure Forth_Or (I : IT);
   procedure Forth_Then (I : IT);
   procedure Forth_While (I : IT);
   procedure Forth_Xor (I : IT);
   procedure From_R (I : IT);
   procedure Greaterequal (I : IT);
   procedure Include (I : IT);
   procedure Interpret (I : IT);
   procedure Interpret_Mode (I : IT);
   procedure J (I : IT);
   procedure Key (I : IT);
   procedure Leave (I : IT);
   procedure Literal (I : IT);
   procedure Lshift (I : IT);
   procedure MS (I : IT);
   procedure Mstar (I : IT);
   procedure Negative (I : IT);
   procedure Parse (I : IT);
   procedure Parse_Word (I : IT);
   procedure Pick (I : IT);
   procedure Plus (I : IT);
   procedure Plus_Loop (I : IT);
   procedure Postpone (I : IT);
   procedure Quit (I : IT);
   procedure R_At (I : IT);
   procedure Recurse (I : IT);
   procedure Refill (I : IT);
   procedure Repeat (I : IT);
   procedure Roll (I : IT);
   procedure Rshift (I : IT);
   procedure S_To_D (I : IT);
   procedure ScaleMod (I : IT);
   procedure See (I : IT);
   procedure Semicolon (I : IT);
   procedure Set_Immediate (I : IT);
   procedure Set_Inline (I : IT);
   procedure Skip_Blanks (I : IT);
   procedure Sm_Slash_Rem (I : IT);
   procedure Store (I : IT);
   procedure Swap (I : IT);
   procedure Tick (I : IT);
   procedure Times (I : IT);
   procedure To_Body (I : IT);
   procedure To_R (I : IT);
   procedure Two_Div (I : IT);
   procedure Two_Dup (I : IT);
   procedure Two_R_At (I : IT);
   procedure Two_To_R (I : IT);
   procedure U_Smaller (I : IT);
   procedure Um_Slash_Mod (I : IT);
   procedure Um_Star (I : IT);
   procedure Unloop (I : IT);
   procedure Unused (I : IT);
   procedure Word (I : IT);
   procedure Words (I : IT);

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
            Inline     : Boolean := False;
         when Number =>
            Value      : Cell;
      end case;
   end record;

   subtype Natural_Cell is Cell range 1 .. Cell'Last;
   package Compilation_Buffers is
      new Ada.Containers.Vectors (Natural_Cell, Action_Type);

   type Dictionary_Entry is record
      Name   : Unbounded_String;
      Action : Action_Type;
   end record;

   package Dictionaries is
     new Ada.Containers.Vectors (Positive, Dictionary_Entry);

   type Byte_Array is array (Cell range <>) of aliased Unsigned_8;

   type Byte_Access is access all Unsigned_8;

   type Interpreter_Body (Last_Address : Cell) is record
      Data_Stack         : Stack_Type;
      Return_Stack       : Stack_Type;
      Compilation_Buffer : Compilation_Buffers.Vector;
      Dict               : Dictionaries.Vector;
      Memory             : Byte_Array (0 .. Last_Address);
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
      Interrupt          : Boolean := False;
   end record;

   type Interpreter_Type is access Interpreter_Body;

end Forth.Interpreter;
