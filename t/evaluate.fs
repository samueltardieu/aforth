: make s" : foo 1 2 + ;" ;
make evaluate
foo . \ 3

: make s" : bar 1 2 + ;" evaluate ;
make
bar . \ 3

: str s" 3 4 +" ; immediate
: eva evaluate ; immediate

: make str eva ;
.s \ <0>
make . \ 7
