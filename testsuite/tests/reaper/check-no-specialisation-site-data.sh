#!/bin/sh
# The lifted [loop] must have code but no closure data. The second argument
# names an exported function whose closure provides a positive control.
exported_function="$2"
if test -s "$1" &&
   ! grep -Fq '*set_of_closures*' "$1" &&
   ! grep -Eq 'addr[[:space:]]+[GL]:"caml[^"]*__loop_[^"]*_code"' "$1" &&
   grep -Eq "addr[[:space:]]+[GL]:\"caml[^\"]*__${exported_function}_[^\"]*_code\"" "$1" &&
   grep -Eq '^[[:space:]]*caml[^[:space:]"]*__loop_[^[:space:]"]*_code$' "$1" &&
   grep -Eq '(^|[[:space:]])[GL]:"caml[^"]*__loop_[^"]*_code"' "$1"; then
  exit "${TEST_PASS}"
else
  echo "Expected direct calls to loop, no closure data for it, and a closure for $exported_function"
  cat "$1"
  exit "${TEST_FAIL}"
fi
