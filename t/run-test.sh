#
# Tests helper functions and setup
#

filter() {
  sed  -e 's/\xd//' -e '/^ok>/d' -e '/^]/d' -e 's/\\ .*//' -e 's/^ *//' -e 's/ *$//' -e '/^$/d'
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

set -e

testfile=$1
testname=$(echo $(basename $testfile) | sed -e 's/\.fs$//')

rm -rf scratch.$testname.*
testdir=scratch.$testname.$$
mkdir -p $testdir
cd $testdir

echo -n "Testing $testname... "

(cat ../$testfile; echo) > commands

sed -ne 's/^.*\\ \(.*\)/\1/p' < commands | filter > expected
../../test_aforth < commands | filter > output 2> errors
if [ $? -ne 0 ] ; then
  fail "bad exit code $?"
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
