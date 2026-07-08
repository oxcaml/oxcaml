#!/usr/bin/env bash

# Extract the body of each OCaml function from an assembly file: print a
# "name:" header (with the mangling stripped), then instruction lines and
# local labels, stopping at the next symbol.

awk '
/^caml[A-Za-z0-9_]*__[A-Za-z0-9_]*_code:$/ {
  name = $0
  sub(/^caml[A-Za-z0-9_]*__/, "", name)
  sub(/_[0-9]+_[0-9]+_code:$/, "", name)
  print name ":"
  infun = 1
  next
}
infun && /^\.Lcaml[A-Za-z0-9_]*:$/ { next }
infun && /^\t\./ { next }
infun && /^\.L[0-9]+:$/ { print; next }
infun && /^[^\t]/ { infun = 0; next }
infun { print }
' "$1"
