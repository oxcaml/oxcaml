#!/bin/sh
# Usage: check-absent.sh <file> <pattern> ...
# Succeeds if no pattern occurs (as a fixed string) in the file.
file="$1"
shift
for pat in "$@"; do
  if grep -qF "$pat" "$file"; then
    echo "unexpectedly present in $file: $pat"
    exit ${TEST_FAIL}
  fi
done
exit ${TEST_PASS}
