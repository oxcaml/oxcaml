#!/bin/sh

# For the fp-sc leg of stack_realloc.ml, which traces the reallocations of
# the stack while its recursion runs: fails unless the output has such a
# reallocation, and takes the lines of the trace out of it. How many there
# are, and what sizes they print, depends on the runtime.

growth='^\[[0-9]*\] Growing stack to '

if ! grep -q "$growth" "$1"; then
  echo "the stack did not grow:"
  cat "$1"
  exit 1
fi
grep -v "$growth" "$1" > "$1.tmp" || exit 1
mv "$1.tmp" "$1"
