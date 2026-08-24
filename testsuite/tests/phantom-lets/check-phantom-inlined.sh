#!/bin/sh
# Check the Cmm dump for phantom_inlined.ml.
file="$1"
fail() { echo "check failed: $1"; exit ${TEST_FAIL}; }
# The substituted defining expressions of the inlined locals must carry
# naming wrappers.  The wrappers are annotations only ("this value used to
# be called <foo>"): the named variables need no binding, so no phantom
# lets are required for the inlined locals.
grep -qF "normal_var_optimized_out" "$file" || fail "no normal_var_optimized_out wrappers"
for v in sum1 doubled diff; do
  grep -qF "$v" "$file" || fail "no trace of inlined local $v"
done
# Checks on parameter classification and inlining-stack locations will be
# added once bound variables carry debuginfo and parameter classifications
# (later patches in this series).
exit ${TEST_PASS}
