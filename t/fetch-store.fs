variable foobar
50331651 foobar ! .s    \ <0>
foobar @ .              \ 50331651
foobar c@ .             \ 3
foobar 1 + c@ .         \ 0
foobar 2 + c@ .         \ 0
foobar 3 + c@ .         \ 3
2 foobar 1 + c!
2 foobar 2 + c!
foobar @ .s             \ <1> 50463235
