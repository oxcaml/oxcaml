#!/usr/bin/env bash
# ACTUAL COMPILER: Check arm64 vector addressing.
set -eu

assembly=$1

# Match the function, not another access elsewhere in this runtime fixture.
# The symbol prefix differs between Darwin and ELF; register choices can vary.
function_body() {
  awk -v name="$1" '
    $0 ~ "^_?camlAccesses__" name "_[[:alnum:]_]*_code:$" { active = 1 }
    active && /^[[:space:]]*\.globl[[:space:]]/ { exit }
    active { print }
  ' "$assembly"
}

mem='[[:space:]]+q[0-9]+, \[x[0-9]+, #16\]'
function_body load_positive | grep -Eq "^[[:space:]]*ldr$mem"
function_body store_positive | grep -Eq "^[[:space:]]*str$mem"
# Indexed accesses must not rematerialize the address in x16.
for name in load_positive store_positive; do
  if function_body "$name" | grep -Eq '^[[:space:]]*add[[:space:]]+x16,'; then
    echo "Unexpected scratch address in $name" >&2
    exit 1
  fi
done
