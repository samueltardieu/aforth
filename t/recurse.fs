: factorial dup 2 > if dup 1- recurse * then ;
1 factorial .  \ 1
10 factorial . \ 3628800
