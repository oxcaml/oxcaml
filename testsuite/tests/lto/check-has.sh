#!/bin/sh
# Usage: check-has.sh MARKER FILE
# Passes if MARKER occurs in FILE (which may be a binary file).
if grep -q "$1" "$2"; then
  exit ${TEST_PASS}
else
  exit ${TEST_FAIL}
fi
