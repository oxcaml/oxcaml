#!/bin/sh
# The lifted [loop] must have code but no closure data (specialisation sites are
# not translated to Cmm); the exported [map_stack] must have both.
if test -s "$1" &&
   ! grep -Fq '*set_of_closures*' "$1" &&
   ! grep -Eq 'addr[[:space:]]+[GL]:"caml[^"]*__loop_[^"]*_code"' "$1" &&
   grep -Eq 'addr[[:space:]]+[GL]:"caml[^"]*__map_stack_[^"]*_code"' "$1" &&
   grep -Eq '^[[:space:]]*caml[^[:space:]"]*__loop_[^[:space:]"]*_code$' "$1" &&
   grep -Eq '(^|[[:space:]])[GL]:"caml[^"]*__loop_[^"]*_code"' "$1"; then
  exit "${TEST_PASS}"
else
  echo "Expected direct calls to loop, no closure data for it, and a closure for map_stack"
  cat "$1"
  exit "${TEST_FAIL}"
fi
