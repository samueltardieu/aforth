#! /usr/bin/env bash
#

. ./lib-tests.sh

cat > commands <<EOF
.s cr
10 20 .s cr
depth . cr
clear depth . cr
EOF

cat > expected <<EOF
<0>
<2> 10 20
2
0
EOF

run_test
