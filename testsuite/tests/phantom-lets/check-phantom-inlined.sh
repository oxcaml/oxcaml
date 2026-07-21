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
# The inlined parameter must be classified with its index...
grep -qF "(parameter (index 0))" "$file" || fail "inlined parameter not classified"
# ...and provenance locations must include a multi-item inlining stack
# (call site followed by the inlined function's frame).
grep -qF ";phantom_inlined.ml:" "$file" || fail "no inlining-stack location"
# Let-bound locals must carry their own source locations (an empty
# location on a local's provenance indicates a regression in closure
# conversion).
if grep -E 'sum1/[0-9]+\) \(location \)' "$file"; then
  fail "local variable with empty source location"
fi
exit ${TEST_PASS}
