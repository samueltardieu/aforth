#! /usr/bin/env bash
#

. ./lib-tests.sh

cat > commands <<EOF
dup
1 2 .s cr
2drop
1 2 .s cr
drop .s cr
2drop
.s cr
EOF

cat > expected <<EOF
*** Stack underflow
<2> 1 2
<2> 1 2
<1> 1
*** Stack underflow
<0>
EOF

run_test
