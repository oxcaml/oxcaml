#!/bin/sh
# Check the Cmm dump for phantom_positioning.ml.
file="$1"
fail() { echo "check failed: $1"; exit ${TEST_FAIL}; }
# precise: a single fully-precise block.
grep -Eq '\[0: a1/[0-9]+; b1/[0-9]+; \]' "$file" || fail "no precise pair1"
# fork: complementary copies, one per branch.
grep -Eq '\[0: a2/[0-9]+; \?; \]' "$file" || fail "no copy describing a2"
grep -Eq '\[0: \?; b2/[0-9]+; \]' "$file" || fail "no copy describing b2"
# refine: partial block at the fork, fuller copy under b3's later binder.
grep -Eq '\[0: a3/[0-9]+; \?; \]' "$file" || fail "no partial pair3"
grep -Eq '\[0: a3/[0-9]+; b3/[0-9]+; \]' "$file" || fail "no refined pair3"
exit ${TEST_PASS}
