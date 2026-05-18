#!/bin/sh
# Test ocamlfilt auto-detection across mangling schemes
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

${OCAMLFILT} --format auto \
  "_CamlU3FooM3BarF3baz" \
  "camlFoo__bar_0"
