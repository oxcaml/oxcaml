#!/bin/sh
# Usage: check-contains-regexp.sh <file> <regexp> ...
# Succeeds if every extended regexp matches somewhere in the file.
file="$1"
shift
for pat in "$@"; do
  if ! grep -qE "$pat" "$file"; then
    echo "missing from $file: $pat"
    exit ${TEST_FAIL}
  fi
done
exit ${TEST_PASS}
