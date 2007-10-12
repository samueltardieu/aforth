: \ TIB# @ >IN ! ;
: CONSTANT ALIGN CREATE , DOES> @ ;
: VARIABLE ALIGN CREATE 0 , ;
: 2VARIABLE ALIGN CREATE 0 , 0 , ;
: 0= 0 = ;
: <> = 0= ;
: 0< 0 < ;
: 0> 0 > ;
: 0<= 0 <= ;
: 0>= 0 >= ;
: 0<> 0 <> ;
: 2* 2 * ;
: NIP SWAP DROP ;
: / /MOD NIP ;
: 2/ 2 / ;
: 1+ 1 + ;
: 1- 1 - ;
: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE
: ( [CHAR] ) PARSE 2DROP ;
: TUCK SWAP OVER ;
: +! TUCK @ + SWAP ! ;
: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;
: CELL 4 ;
: CELLS CELL * ;
: CELL+ CELL + ;
: 2! SWAP OVER ! CELL+ ! ;
: 2@ DUP CELL+ @ SWAP @ ;
: 2OVER 3 PICK 3 PICK ;
: ROT 2 ROLL ;
: ?DUP DUP IF DUP THEN ;
: NEGATE 0 SWAP - ;
: ABS DUP 0< IF NEGATE THEN ;
: ALIGNED CELL 1- + CELL / CELL * ;
: ALLOT HERE +! ;
: INVERT NEGATE 1- ;
: CHAR+ 1 + ;
: CHARS ;
: COUNT DUP CHAR+ SWAP C@ ;
: MAX 2DUP > IF DROP ELSE NIP THEN ;
: MIN 2DUP > IF NIP ELSE DROP THEN ;
: MOD /MOD DROP ;
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
: I R@ ; INLINE
: LOOP 1 POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
: .S [CHAR] < EMIT DEPTH DUP . [CHAR] > EMIT
  DUP 0<> IF 1 SWAP 1- NEGATE DO SPACE I NEGATE PICK . LOOP ELSE DROP THEN ;
: */ */MOD NIP ;
: 1+ 1 + ;
: 1- 1 - ;
: UNTIL POSTPONE 0= POSTPONE WHILE POSTPONE REPEAT ; IMMEDIATE
