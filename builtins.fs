: HERE (HERE) @ ;
: \ TIB# @ >IN ! ; IMMEDIATE
: CELL 4 ; INLINE
: OVER 1 PICK ;
: TUCK SWAP OVER ;
: +! TUCK @ + SWAP ! ;
: , HERE ! CELL (HERE) +! ;
: CREATE : HERE POSTPONE LITERAL POSTPONE ; ;
: CONSTANT ALIGN CREATE , DOES> @ ;
: VARIABLE ALIGN CREATE 0 , ;
: 2VARIABLE ALIGN CREATE 0 , 0 , ;
: 1+ 1 + ;
: NEGATE -1 XOR 1+ ;
: DUP 0 PICK ;
: 0>= 0 >= ;
: = XOR DUP 0>= SWAP NEGATE 0>= AND ;
: 0= 0 = ;
: < >= 0= ;
: <> = 0= ;
: 0< 0>= 0= ;
: 0> NEGATE 0< ;
: 0<= NEGATE 0>= ;
: 0<> 0 <> ;
: < >= 0= ;
: > SWAP < ;
: <= SWAP >= ;
: 2* 2 * ;
: NIP SWAP DROP ;
: / /MOD NIP ;
: - NEGATE + ;
: 1- 1 - ;
: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: CHAR WORD DROP C@ ;
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE
: ( [CHAR] ) PARSE 2DROP ;
: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;
: CELLS CELL * ;
: CELL+ CELL + ;
: 2! SWAP OVER ! CELL+ ! ;
: 2@ DUP CELL+ @ SWAP @ ;
: 2OVER 3 PICK 3 PICK ;
: ROT 2 ROLL ;
: ?DUP DUP IF DUP THEN ;
: ABS DUP 0< IF NEGATE THEN ;
: ALIGNED CELL 1- + CELL / CELL * ;
: ALLOT (HERE) +! ;
: INVERT NEGATE 1- ;
: CHAR+ 1 + ;
: CHARS ;
: COUNT DUP CHAR+ SWAP C@ ;
: 2SWAP 3 ROLL 3 ROLL ;
: ELSE POSTPONE AHEAD 2SWAP POSTPONE THEN ; IMMEDIATE
: MAX 2DUP > IF DROP ELSE NIP THEN ;
: MIN 2DUP > IF NIP ELSE DROP THEN ;
: MOD /MOD DROP ;
: BOUNDS OVER + SWAP ;
: LOOP 1 POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
: I R@ ; INLINE
: TYPE DUP IF BOUNDS DO I C@ EMIT LOOP ELSE 2DROP THEN ;
: .( [CHAR] ) PARSE TYPE ; IMMEDIATE
0 CONSTANT FALSE
-1 CONSTANT TRUE
ALIGN CREATE PAD 256 ALLOT
: ['] ' POSTPONE LITERAL ; IMMEDIATE
: VALUE CONSTANT ;
: TO ' >BODY STATE @ IF POSTPONE LITERAL POSTPONE ! ELSE ! THEN ; IMMEDIATE
: -ROT ROT ROT ;
: DEFER VARIABLE DOES> @ ?DUP IF EXECUTE THEN ;
: IS ' >BODY ! ;
32 CONSTANT BL
: SPACE BL EMIT ;
: .S [CHAR] < EMIT DEPTH DUP . [CHAR] > EMIT
  DUP IF 1 SWAP 1- NEGATE DO SPACE I NEGATE PICK . LOOP ELSE DROP THEN ;
: */ */MOD NIP ;
: 1+ 1 + ;
: 1- 1 - ;
: UNTIL POSTPONE 0= POSTPONE WHILE POSTPONE REPEAT ; IMMEDIATE
: C, HERE C! 1 ALLOT ;
: S" [CHAR] " PARSE HERE POSTPONE LITERAL DUP POSTPONE LITERAL
  BOUNDS DO I C@ C, LOOP ; IMMEDIATE
: C" [CHAR] " PARSE HERE POSTPONE LITERAL DUP C,
  BOUNDS DO I C@ C, LOOP ; IMMEDIATE
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE
: 2R> R> R> SWAP ; INLINE
: -! TUCK @ SWAP - SWAP ! ;
: CLEAR DEPTH DUP IF 0 DO DROP LOOP THEN ;
: SOURCE TIB TIB# @ ;
