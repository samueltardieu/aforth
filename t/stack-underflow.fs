dup                 \ *** Stack underflow
1 2 .s cr           \ <2> 1 2
2drop 3 4 .s cr     \ <2> 3 4
drop .s cr          \ <1> 3
2drop               \ *** Stack underflow
.s cr               \ <0>
