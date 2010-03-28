#
# Tests helper functions and setup
#

set -e

testname=$(echo $(basename $0) | sed -e 's/\.sh$//')

rm -rf scratch.$testname.*
testdir=scratch.$testname.$$
mkdir -p $testdir
cd $testdir

echo -n "Testing $testname... "

run_test() {
  echo bye >> commands
  echo >> expected
  ! ../../test_aforth commands | sed -e 's/\xd//' > output 2> errors
  if [ $# != 0 ] ; then
    fail error
  fi
  if [ -s errors ]; then
    fail "standard error not empty"
  fi
  if ! cmp output expected > /dev/null; then
    fail "bad output"
  fi
  echo "ok"
  cd ..
  rm -rf $testdir
  exit 0
}

fail() {
  echo $1
  printf "(test can be examined in directory $testdir)"
  echo
  mycat Errors
  mycat Commands
  mycat Output
  mycat Expected
  exit 1
}

mycat() {
  echo
  echo "$1:"
  sed -e 's/^/  /' < $(echo $1 | tr A-Z a-z)
}