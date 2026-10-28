#!/usr/bin/env bash
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
