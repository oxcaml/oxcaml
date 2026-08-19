#!/bin/sh
# Usage: check-absent-regexp.sh <file> <regexp> ...
# Succeeds if no extended regexp matches anywhere in the file.
file="$1"
shift
for pat in "$@"; do
  if grep -qE "$pat" "$file"; then
    echo "unexpectedly present in $file: $pat"
    exit ${TEST_FAIL}
  fi
done
exit ${TEST_PASS}
