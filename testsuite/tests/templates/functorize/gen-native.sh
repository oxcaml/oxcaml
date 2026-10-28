#!/bin/sh

set -eu

gen_native() {
  INPUT="$1"
  OUTPUT="$2"
  INPUTNAME="${INPUT##*/functorize/}"
  CORRECTED="${OUTPUT%.ml}.corrected.ml"
  sed '
s/_byte/_native/g
s/ocamlc/ocamlopt/g
# Oops, we changed ocamlc_byte to ocamlopt_native, so fix that
s/ocamlopt_native/ocamlopt_byte/g
s/\.cmo/.cmx/g
s/\.bc/.exe/g
s/\.cma/.cmxa/g
' "$INPUT" | sed "s|TEST|TEST (* DO NOT EDIT. Instead edit $INPUTNAME and run gen-native.sh. *)|" \
  > "$OUTPUT"
}

gen_native "$1" "$2"
