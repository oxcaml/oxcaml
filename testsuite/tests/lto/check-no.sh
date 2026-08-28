#!/bin/sh
# Usage: check-no.sh MARKER FILE
# Passes if MARKER does not occur in FILE (which may be a binary file).
if grep -q "$1" "$2"; then
  exit ${TEST_FAIL}
else
  exit ${TEST_PASS}
fi
