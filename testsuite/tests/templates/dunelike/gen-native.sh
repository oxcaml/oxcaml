#!/bin/sh

set -eu

case ${1:-} in
  "--check")
    CHECKING="true"
  ;;
  "")
    CHECKING=""
  ;;
  *)
    echo "unrecognized option $1"
    exit 1
  ;;
esac

gen_native() {
  INPUT="$1"
  OUTPUT="$2"
  CORRECTED="${OUTPUT%.ml}.corrected.ml"
  TARGET=$([ -n "$CHECKING" ] && echo "$CORRECTED" || echo "$OUTPUT")
  sed '
s/_byte/_native/g
s/ocamlc/ocamlopt/g
# Oops, we changed ocamlc_byte to ocamlopt_native, so fix that
s/ocamlopt_native/ocamlopt_byte/g
s/\.cmo/.cmx/g
s/\.bc/.exe/g
' "$INPUT" | sed "s/TEST/TEST (* DO NOT EDIT. Instead edit $INPUT and run gen-native.sh. *)/" \
  > "$TARGET"
  if [ -n "$CHECKING" ]; then
    ( diff -q "$OUTPUT" "$TARGET" && rm "$TARGET" ) \
    || exit 1
  fi
}

gen_native test_byte.ml test_native.ml
gen_native test_functorize_byte.ml test_functorize_native.ml
