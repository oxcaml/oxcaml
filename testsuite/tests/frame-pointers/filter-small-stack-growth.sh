#!/bin/sh

# The debug runtime (USE_RUNTIME=d) starts stacks at 512 bytes instead of
# the normal 32k (Stack_init_bsize under DEBUG, runtime/caml/config.h), so
# early execution prints extra "Growing stack to ..." lines below 64k that
# the normal runtime never emits.  Drop the growth lines below 64k before
# the reference comparison, so that the reference positively asserts the
# same four parent-stack growths (64k/128k/256k/512k) under both runtimes.

awk '{
  if (match($0, /^\[[0-9]+\] Growing stack to [0-9]+k? bytes$/)) {
    size = $0
    sub(/^\[[0-9]+\] Growing stack to /, "", size)
    sub(/ bytes$/, "", size)
    mult = 1
    if (sub(/k$/, "", size)) mult = 1024
    if (size * mult < 65536) next
  }
  print
}' "$1" > "$1.tmp"
mv "$1.tmp" "$1"
