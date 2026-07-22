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
' "$INPUT" | sed "s|TEST|TEST (* DO NOT EDIT. Instead edit $INPUT and run gen-native.sh. *)|" \
  > "$TARGET"
  if [ -n "$CHECKING" ]; then
    ( diff -q "$OUTPUT" "$TARGET" && rm "$TARGET" ) \
    || exit 1
  fi
}

gen_native bad_input/test_byte.ml bad_input/test_native.ml
gen_native bad_deps/test_byte.ml bad_deps/test_native.ml
gen_native bad_bundle_cmi_overwritten/test_byte.ml bad_bundle_cmi_overwritten/test_native.ml
gen_native uses_plain/test_byte.ml uses_plain/test_native.ml
gen_native input_ordering/test_byte.ml input_ordering/test_native.ml
gen_native module_alias/test_byte.ml module_alias/test_native.ml
gen_native dunelike/test_byte.ml dunelike/test_native.ml
gen_native partial_deps/test_byte.ml partial_deps/test_native.ml
gen_native arg_block_extract/test_byte.ml arg_block_extract/test_native.ml
gen_native missing_impl/test_byte.ml missing_impl/test_native.ml
gen_native cmifile/test_byte.ml cmifile/test_native.ml
gen_native output_modes/test_byte.ml output_modes/test_native.ml
gen_native multi_instances/test_byte.ml multi_instances/test_native.ml
gen_native complete_arg/test_byte.ml complete_arg/test_native.ml
gen_native nested_static/test_byte.ml nested_static/test_native.ml
