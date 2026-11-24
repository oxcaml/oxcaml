#!/bin/sh

# Ensure the check is not vacuous: both allocations were recorded and escaped.
if test "$(grep -c '^allocated_in(' "$1")" != 2 ||
   test "$(grep -c '^escapes_anywhere(' "$1")" != 2; then
  echo "Expected two candidate allocations, both escaping"
  exit "${TEST_FAIL}"
fi

if grep -E '^(escape_candidate_source|local_allocation)\(' "$1"; then
  echo "Known-escaping allocations must not produce candidate sources or locals"
  exit "${TEST_FAIL}"
fi

exit "${TEST_PASS}"
