#!/bin/sh

set -eu

ocamlopt=$1
llvm=$OXCAML_LLVM_BIN

# clang links the executable, so that it adds the (static) UBSan runtime,
# which contains the coverage runtime.
"$ocamlopt" -g -gated-trace-pc-guard -cc "$llvm/clang -fsanitize=undefined" \
  gate.c sancov_test.ml -o sancov_test.exe

mkdir -p coverage
UBSAN_OPTIONS=coverage=1:coverage_dir=coverage ./sancov_test.exe

# Keep the source location and the name of the two functions of interest,
# whatever the name mangling scheme.
filter () {
  sed -n 's/.*\(sancov_test\.ml:[0-9]*\) .*\(hit_fn\|missed_fn\).*/\1 \2/p' \
    | sort -u
}

echo "covered:"
"$llvm/sancov" -covered-functions sancov_test.exe coverage/*.sancov | filter
echo "not covered:"
"$llvm/sancov" -not-covered-functions sancov_test.exe coverage/*.sancov \
  | filter
