#!/bin/sh
lib=camlSpecialise_lifted_stale_hint_lib
has_call () {
  tr '\n' ' ' < "$1" |
    grep -aE \
      "\(app[^[:space:]]*[[:space:]]+[GL]:\"${lib}__${2}_[^\"]*_code\"" \
      > /dev/null
}

if test -s "$1" &&
   ! has_call "$1" outer_hint &&
   ! has_call "$1" outer_control &&
   has_call "$1" large &&
   ! grep -aq 'camlSpecialise_lifted_stale_hint__large_loop_.*_code' "$1"; then
  exit "${TEST_PASS}"
else
  echo "Expected both hint variants to inline, but no copy of the large helper"
  cat "$1"
  exit "${TEST_FAIL}"
fi
