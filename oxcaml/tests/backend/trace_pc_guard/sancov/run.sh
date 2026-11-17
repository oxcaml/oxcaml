#!/bin/sh

set -eu

ocamlopt=$1
llvm=$OXCAML_LLVM_BIN

# clang links the executable, so that it adds the (static) UBSan runtime,
# which contains the coverage runtime. It also compiles and instruments the
# C stubs.
"$ocamlopt" -g -gated-trace-pc-guard -cc "$llvm/clang -fsanitize=undefined" \
  -ccopt -fsanitize-coverage=trace-pc-guard \
  stubs.c sancov_lib.ml sancov_test.ml -o sancov_test.exe

mkdir -p coverage
UBSAN_OPTIONS=coverage=1:coverage_dir=coverage ./sancov_test.exe

# Keep the source location and the name of the functions of interest, whatever
# the name mangling scheme.
filter () {
  sed -e 's|^.*/||' \
    -e 's/^\([a-z_.]*:[0-9]*\) .*\(\(hit\|missed\)_[a-z]*\).*/\1 \2/' -e t -e d \
    | LC_ALL=C sort -u
}

echo "covered:"
"$llvm/sancov" -covered-functions sancov_test.exe coverage/*.sancov | filter
echo "not covered:"
"$llvm/sancov" -not-covered-functions sancov_test.exe coverage/*.sancov \
  | filter
