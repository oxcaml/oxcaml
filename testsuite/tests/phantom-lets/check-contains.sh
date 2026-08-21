#!/bin/sh
# Usage: check-contains.sh <file> <pattern> ...
# Succeeds if every pattern occurs (as a fixed string) in the file.
file="$1"
shift
for pat in "$@"; do
  if ! grep -qF "$pat" "$file"; then
    echo "missing from $file: $pat"
    exit ${TEST_FAIL}
  fi
done
exit ${TEST_PASS}
