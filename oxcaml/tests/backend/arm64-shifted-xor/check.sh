#!/usr/bin/env bash
# ACTUAL COMPILER: inspect one function at a time, independent of registers
# and the platform's symbol prefix.
set -eu

assembly=$1
function_body() {
  awk -v name="$1" '
    $0 ~ "^_?caml.*__" name "_[0-9]+_[0-9]+_code:$" { found = 1; next }
    found && /[.]cfi_endproc/ { exit }
    found { print }
  ' "$assembly"
}

expect_shifted_eor() {
  name=$1
  shift=$2
  amount=$3
  body=$(function_body "$name")
  if ! printf '%s\n' "$body" |
    grep -Eq "^[[:space:]]*eor[[:space:]]+x[0-9]+, x[0-9]+, x[0-9]+, $shift #$amount([[:space:]]|$)"
  then
    printf 'Missing %s #%s shifted eor in %s:\n%s\n' "$shift" "$amount" "$name" "$body" >&2
    exit 1
  fi
}

expect_shifted_eor tagged_left13 lsl 13
expect_shifted_eor tagged_left13_commuted lsl 13
expect_shifted_eor tagged_right7 lsr 7
expect_shifted_eor raw_left13 lsl 13
expect_shifted_eor raw_left13_commuted lsl 13
expect_shifted_eor raw_right7 lsr 7
expect_shifted_eor raw_right7_commuted lsr 7
expect_shifted_eor raw_left63 lsl 63
expect_shifted_eor raw_right63 lsr 63

# Arithmetic right shifts are not folded as logical shifts, and variable
# shifts cannot use the immediate-shift encoding.
arithmetic=$(function_body raw_arithmetic)
printf '%s\n' "$arithmetic" | grep -Eq '^[[:space:]]*(asr|sbfm)[[:space:]]'
if printf '%s\n' "$arithmetic" | grep -Eq '^[[:space:]]*eor.*,( asr| lsr) #7'; then
  printf 'Unexpected shifted eor for an arithmetic right shift:\n%s\n' "$arithmetic" >&2
  exit 1
fi
dynamic=$(function_body raw_dynamic)
printf '%s\n' "$dynamic" | grep -Eq '^[[:space:]]*lslv?[[:space:]]'
if printf '%s\n' "$dynamic" | grep -Eq '^[[:space:]]*eor.*, lsl #'; then
  printf 'Unexpected immediate shifted eor for a variable shift:\n%s\n' "$dynamic" >&2
  exit 1
fi
