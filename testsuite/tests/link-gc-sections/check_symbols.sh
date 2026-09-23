#!/bin/sh

# Usage: check_symbols.sh absent|present
# Checks the text symbols of ${program}: the live function's code must
# always be present; the dead function's code must be present or absent as
# requested.  (Only code symbols are checked: the functions' closures are
# reachable from the module block, which is a GC root in this prototype.)

exec > "${output}" 2>&1

expect="$1"
symbols=${test_build_directory}/symbols.txt
nm "${program}" > "$symbols"

if grep -q ' T camlDead_code_lib__live_' "$symbols"; then
  echo "live: present"
else
  echo "live: ABSENT"
  exit 1
fi

if grep -q ' T camlDead_code_lib__dead_' "$symbols"; then
  found=present
else
  found=absent
fi
echo "dead: $found"
if [ "$found" != "$expect" ]; then
  echo "expected dead to be $expect"
  exit 1
fi
