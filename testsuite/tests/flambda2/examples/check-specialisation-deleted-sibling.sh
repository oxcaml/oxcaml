#!/bin/sh
# Inspect only the final raw Flambda, not flow diagnostics or the input.
simplified=$(sed -n '/^After simplify:/,$p' "$1" | tr '\n' ' ')

has_live_slot() {
  printf '%s\n' "$simplified" |
    grep -Eq "\(\($1/[0-9]+[^)]*\)[[:space:]]+caml[^[:space:]]+_code\)"
}

has_deleted_slot() {
  printf '%s\n' "$simplified" |
    grep -Eq "\(\($1/[0-9]+[^)]*\)[[:space:]]+\[deleted\]\)"
}

check_shape() {
  case "$2" in
    sibling | phantom-sibling)
      has_live_slot live && has_deleted_slot dead || return 1
      printf '%s\n' "$simplified" |
        grep -Eq '\(a/[0-9]+UV[[:space:]]+b/[0-9]+UV\)' || return 1
      if [ "$2" = phantom-sibling ]; then
        printf '%s\n' "$simplified" |
          grep -Eq 'unused/[0-9]+UV[[:space:]]*=[[:space:]]*b/[0-9]+UV' || return 1
      fi
      ;;
    *) return 1 ;;
  esac
}

if check_shape "$@"; then
  exit "${TEST_PASS}"
else
  cat "$1"
  exit "${TEST_FAIL}"
fi
