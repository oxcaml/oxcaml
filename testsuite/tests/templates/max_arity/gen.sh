#!/bin/sh

# Usage: sh gen.sh <output-directory>
#
# Generates [m001.ml] ... [m130.ml], each parameterised by [P], and [big.ml],
# which depends on all of them.

set -e

dir="$1"

echo "let xs = [" > "$dir/big.ml"
for i in $(seq 1 130); do
  name=$(printf 'm%03d' "$i")
  echo "let x = P.v * $i" > "$dir/$name.ml"
  printf '  M%03d.x;\n' "$i" >> "$dir/big.ml"
done
echo "]" >> "$dir/big.ml"
