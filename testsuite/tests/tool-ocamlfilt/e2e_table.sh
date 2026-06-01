#!/bin/sh
# Demangle every OCaml-mangled symbol in an object file with each
# ocamlfilt format and print a column-aligned table to stdout.
#
# Usage: sh e2e_table.sh <obj-file> <exclude-pattern>
#
# <exclude-pattern> is an extended regex; nm lines matching it are
# dropped before symbol extraction. Tests typically pass
# [camlStdlib|camlCamlinternal] to drop references to stdlib --
# their names belong to other compilation units and would make
# this reference fragile to unrelated compiler changes. Non-
# excluded undefined references are kept because that's how
# cross-module inlining shows up: the caller's object holds a stub
# for a closure that lives in the inlined callee's unit.
#
# The table has four columns: the raw linker symbol followed by
# its demangling under [--format flat1], [--format structured], and
# [--format auto]. Cells where ocamlfilt refuses the symbol show
# [(error)]. Compiler-generated stamps ([_NN_NN_code], [_NN_code],
# trailing [_NN], [PmakeblockNN], [const_blockNN], [iarrNN]) are
# masked so the reference stays stable across builds.
# No [pipefail]: this script runs under POSIX [sh] (dash on Linux),
# which does not support it. An empty extraction pipeline is caught by
# the [! -s "$SYMS"] check below instead.
set -eu

# Fixed locale for a stable [sort] order across platforms.
export LC_ALL=C

OBJ=${1:?"missing object file argument"}
# The pattern only needs the flat (lowercase [caml]) prefix: stdlib
# is built with the flat scheme, so its symbol names show up
# flat-mangled in the relocation table regardless of the scheme used
# to compile this test.
EXCLUDE=${2:?"missing exclude-pattern argument"}

OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
TAB="$(printf '\t')"

# ocamlfilt mirrors c++filt and passes unrecognised input through unchanged
# on stdout. For this table we want to distinguish "format X demangled the
# symbol" from "format X did not recognise it", so we re-emit [(error)]
# whenever the output equals the input.
ocamlfilt_or_error () {
  out=$("$OCAMLFILT" --format "$1" "$2" 2>/dev/null)
  if [ "$out" = "$2" ]; then
    echo "(error)"
  else
    echo "$out"
  fi
}

SYMS="${test_build_directory}/e2e_table.syms"

# Drop excluded lines, then keep OCaml-mangled symbols: flat
# ([caml] + uppercase) or structured ([_Caml] + uppercase), each
# optionally with the macOS leading underscore.
#
# Strip the extra underscore the macOS assembler prepends, so one
# committed reference works on both Linux and macOS. ocamlfilt accepts
# either form, so demangling is unaffected.
nm "$OBJ" | grep -Ev "$EXCLUDE" | awk '
  $NF ~ /^_?(caml|_Caml)[A-Z]/ { print $NF }
' | sed -E 's/^_(caml|_Caml)/\1/' | sort -u > "$SYMS"

if [ ! -s "$SYMS" ]; then
  echo "no OCaml symbols found in $OBJ" > "$ocamltest_response"
  exit "$TEST_FAIL"
fi

for fmt in flat1 structured auto; do
  COL="${test_build_directory}/e2e_table.${fmt}.col"
  : > "$COL"
  while IFS= read -r sym; do
    ocamlfilt_or_error "$fmt" "$sym" >> "$COL"
  done < "$SYMS"
done

{
  printf 'SYMBOL\tFLAT1\tSTRUCTURED\tAUTO\n'
  paste "$SYMS" \
    "${test_build_directory}/e2e_table.flat1.col" \
    "${test_build_directory}/e2e_table.structured.col" \
    "${test_build_directory}/e2e_table.auto.col"
} | sed -E \
    -e 's/_[0-9]+_[0-9]+_code/_N_N_code/g' \
    -e 's/_[0-9]+_code/_N_code/g' \
    -e 's/Pmakeblock[0-9]+/PmakeblockN/g' \
    -e 's/const_block[0-9]+/const_blockN/g' \
    -e 's/iarr[0-9]+/iarrN/g' \
    -e "s/_[0-9]+(${TAB}|$)/_N\\1/g" \
  | column -t -s "${TAB}"
