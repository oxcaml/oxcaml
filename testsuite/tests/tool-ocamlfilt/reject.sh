#!/bin/sh
# Test that ocamlfilt correctly rejects invalid symbols
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

check_reject () {
  format=$1
  input=$2
  if ${OCAMLFILT} --format "$format" "$input" >/dev/null 2>&1; then
    echo "UNEXPECTED PASS: $input"
    exit 1
  else
    echo "rejected: $input"
  fi
}

check_reject structured "_Caml"
check_reject auto "main"
check_reject auto "_ZN4testE"
check_reject flat0 "caml_foo"
check_reject flat1 "caml_foo"
