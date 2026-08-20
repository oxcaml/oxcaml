#!/bin/sh
# Check the Cmm dump for phantom_inlined.ml.
file="$1"
fail() { echo "check failed: $1"; exit ${TEST_FAIL}; }
# The inlined locals must be rebound by (empty) phantom lets...
grep -qF "let?" "$file" || fail "no phantom lets"
for v in sum1 doubled diff; do
  grep -qF "$v" "$file" || fail "no trace of inlined local $v"
done
# ...and the substituted defining expressions must carry naming wrappers.
grep -qF "name_for_debugger" "$file" || fail "no name_for_debugger wrappers"
# Checks on parameter classification and inlining-stack locations will be
# added once bound variables carry debuginfo and parameter classifications
# (later patches in this series).
exit ${TEST_PASS}
