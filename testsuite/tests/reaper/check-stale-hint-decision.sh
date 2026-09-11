#!/bin/sh
lib=camlSpecialise_lifted_stale_hint_lib
has_call () {
  tr '\n' ' ' < "$1" |
    grep -aE \
      "\(app[^[:space:]]*[[:space:]]+[GL]:\"${lib}__${2}_[^\"]*_code\"" \
      > /dev/null
}

if test -s "$1" &&
   has_call "$1" outer_hint &&
   ! has_call "$1" outer_control; then
  exit "${TEST_PASS}"
else
  echo "Expected a call to outer_hint, but outer_control to be inlined"
  cat "$1"
  exit "${TEST_FAIL}"
fi
