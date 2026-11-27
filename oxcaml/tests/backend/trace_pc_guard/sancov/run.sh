#!/bin/sh

set -eu

ocamlopt=$1
clang=$OXCAML_CLANG
sancov=$(dirname "$clang")/sancov

# clang links the executable, so that it adds the (static) UBSan runtime,
# which contains the coverage runtime. It also compiles and instruments the
# C stubs.
"$ocamlopt" -g -gated-trace-pc-guard -cc "$clang -fsanitize=undefined" \
  -ccopt -fsanitize-coverage=trace-pc-guard \
  stubs.c sancov_lib.ml sancov_test.ml -o sancov_test.exe

mkdir -p coverage
UBSAN_OPTIONS=coverage=1:coverage_dir=coverage ./sancov_test.exe

# Keep the functions of interest, with their source location, whatever the name
# mangling scheme. Locations in files other than those of the test, which come
# from inlined library code, are dropped.
#
# A probe of inlined code counts for every function among its inlined frames,
# which [-gated-trace-pc-guard] makes the compiler describe: [hit_inlined] is
# only ever executed inlined in [hit_caller]. [sancov] gives a function the
# least line of its probes, and it also counts the line of inlined code for the
# function it was inlined into, hence the line of [hit_caller].
files='sancov_[a-z]*\.ml\|stubs\.c'
names='\(hit\|missed\)_[a-z]*'
filter () {
  sed -n -e 's|^.*/||' -e "s/^\(\($files\):[0-9]*\) .*\($names\).*/\1 \3/p" \
    | LC_ALL=C sort -u
}

echo "covered:"
"$sancov" -covered-functions sancov_test.exe coverage/*.sancov | filter
echo "not covered:"
"$sancov" -not-covered-functions sancov_test.exe coverage/*.sancov \
  | filter
