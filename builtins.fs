: \ tib# @ >in ! ;
: constant align create , does> @ ;
: variable align create 0 , ;
: 2variable align create 0 , 0 , ;
: 0= 0 = ;
: 0< 0 < ;
: 0> 0 > ;
: 0<= 0 <= ;
: 0>= 0 >= ;
: 0<> 0 <> ;
: 2* 2 * ;
: 2/ 2 / ;
: 1+ 1 + ;
: 1- 1 - ;
: 2drop drop drop ;
: 2dup over over ;
: ( [char] ) parse 2drop ;
: tuck swap over ;
: +! tuck @ + swap ! ;
: decimal 10 base ! ;
: hex 16 base ! ;
: cell 4 ;
: cells cell * ;
: cell+ cell + ;
: 2! swap over ! cell+ ! ;
: 2@ dup cell+ @ swap @ ;
: 2over 3 pick 3 pick ;
: rot 2 roll ;
: ?dup dup if dup then ;
: negate 0 swap - ;
: abs dup 0< if negate then ;
: aligned cell 1- + cell / cell * ;
: allot here +! ;
: invert negate 1- ;
: char+ 1 + ;
: chars ;
: count dup char+ swap c@ ;
: nip swap drop ;
: max 2dup > if drop else nip then ;
: min 2dup > if nip else drop then ;
: mod /mod drop ;
: .( [char] ) parse type ; immediate
0 constant false
-1 constant true
align create pad 256 allot
: ['] ' postpone literal ; immediate
: value constant ;
: to ' >body state @ if postpone literal postpone ! else ! then ; immediate
