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

# Structured: invalid path-item tag ('X' is not one of U/I/M/S/O/F/L/P).
check_reject structured "_CamlU3FooXXX"
# Structured: claimed identifier length overruns the remaining input
# ("U99" says the next 99 bytes are the unit name, but only 3 follow).
check_reject structured "_CamlU99Foo"
# Structured: missing decimal length after a path-item tag.
check_reject structured "_CamlUFoo"
# Structured: bad hex in the [u<len>...] escaped identifier
# ([G] and [g] are not lowercase hex digits).
check_reject structured "_CamlU3FooFu5G2gg_funcsub"

# Flat1: truncated [$xx] escape (only one byte follows the '$').
check_reject flat1 'camlFoo__$3'
# Flat1: truncated [$xx] escape (no byte follows the '$').
check_reject flat1 'camlFoo__$'
# Flat0: same truncation -- exercises the equivalent path of the
# older decoder.
check_reject flat0 'camlFoo__$3'
# Flat scheme accepts [caml...] or [_caml...] (macOS) but never
# [__caml...]; that's a structured-scheme shape ([__Caml...]).
check_reject flat1 '__camlFoo__bar_0'
check_reject flat0 '__camlFoo__bar_0'
