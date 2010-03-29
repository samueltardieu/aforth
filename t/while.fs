: t 5 begin dup . space dup while 1- repeat [CHAR] . emit ; t \ 5 4 3 2 1 0 .
: odd 2 mod 1 = ;
: t 5 begin dup . space dup while dup odd while 1- repeat [CHAR] . emit ; t \ 5 4 .
: t 5 begin dup . space dup odd while dup while 1- repeat [CHAR] . emit ; t \ 5 4 .
