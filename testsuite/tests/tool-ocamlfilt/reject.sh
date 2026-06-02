#!/bin/sh
# Test that ocamlfilt correctly rejects invalid symbols. Like c++filt /
# rustfilt, ocamlfilt passes unrecognised input through unchanged on stdout
# rather than failing, so "rejected" here means "output equals input".
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

check_reject () {
  format=$1
  input=$2
  output=$(${OCAMLFILT} --format "$format" "$input" 2>/dev/null)
  if [ "$output" = "$input" ]; then
    echo "rejected: $input"
  else
    echo "UNEXPECTED DEMANGLE: $input -> $output"
    exit 1
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
# Structured: bad index in the [u<len>...] escaped identifier
# (The last item is [Fu6G1234_] where [G] expect a raw part of at least 6
# characters; [funcsub] is the suffix, the escaped characters shouldn't be
# inserted in it).
check_reject structured "_CamlU3FooFu6G1234_funcsub"
# Structured: bad hex in the [u<len>...] escaped identifier
# ([g] is not a correct hexadecimal digit)
check_reject structured "_CamlU3FooFu11G2g_funcsub"
# Structured: bad hex in the [u<len>...] escaped identifier
# ([A] is not a lowercase hexadecimal digit)
check_reject structured "_CamlU3FooFu11G2A_funcsub"

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
