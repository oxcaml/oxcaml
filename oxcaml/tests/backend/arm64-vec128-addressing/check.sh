#!/usr/bin/env bash
set -eu

# Ignore register allocation and platform-specific symbol spelling.  These
# leaf functions need no vector spills, so all q accesses are byte accesses.
assembly=$1
ins='^[[:space:]]*'
mem='[[:space:]]+q[0-9]+, \[x[0-9]+, #'
for operation in ldr str; do
  for offset in 0 16 65520; do
    grep -Eq "$ins$operation$mem$offset\\]" "$assembly"
  done
done
# The textual assembler also accepts ldr/str with an unscaled offset.
for operation in 'ldr|ldur' 'str|stur'; do
  grep -Eq "$ins($operation)${mem}1\\]" "$assembly"
done

# The offset beyond the scaled encoding must be added before the access.
# In particular, it must not be truncated to fit in the instruction.
if grep -Eq "$ins(ldr|str)[[:space:]]+q[0-9]+, .*#65536" "$assembly"; then
  exit 1
fi
reg='x[0-9]+'
add="^[[:space:]]*add[[:space:]]+$reg, $reg, "
count=$(grep -Ec "${add}(#65536|#16, lsl #12)$" "$assembly" || true)
test "$count" -eq 2

# The unaligned emitter used to recompute each address in x16, even at 0.
if grep -Eq "^[[:space:]]*add[[:space:]]+x16," "$assembly"; then
  exit 1
fi
