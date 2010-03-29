: test 5 0 do i loop ;
test .s  \ <5> 0 1 2 3 4

clear

: test 0 5 do i -1 +loop ;
test .s  \ <6> 5 4 3 2 1 0
