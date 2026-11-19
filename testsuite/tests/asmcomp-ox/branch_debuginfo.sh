#!/bin/sh

set -eu

src=${test_source_directory}/branch_debuginfo.ml
asm=branch_debuginfo.${asmext}

fail () {
    echo "$1" > ${ocamltest_response}
    exit ${TEST_FAIL}
}

[ -e "$asm" ] || fail "not found $asm"

for line in $(grep -n '(\* LINE \*)' "$src" | cut -d: -f1); do
    grep -q "^[[:space:]]*\.loc[[:space:]][[:space:]]*[0-9][0-9]*[[:space:]][[:space:]]*$line[[:space:]]" "$asm" \
        || fail "no .loc for line $line"
done

missing=$(${AWK:-awk} '
  /^\.?L[0-9]+:/ { label = $1; next }
  label == "" { next }
  /^[ \t]*\.loc[ \t]/ { label = ""; next }
  /^[ \t]*\.(cfi_|align|p2align)/ { next }
  /^[ \t]*[a-zA-Z]/ { printf "%s ", label }
  { label = "" }' "$asm")

[ -z "$missing" ] || fail "no .loc at the start of: $missing"

exit ${TEST_PASS}
