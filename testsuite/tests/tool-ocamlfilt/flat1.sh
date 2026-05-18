#!/bin/sh
# Test ocamlfilt demangling with the Flat1 scheme (OCaml >= 5.3)
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

${OCAMLFILT} --format flat1 \
  "camlFoo" \
  "camlFoo__bar_0" \
  "camlA__B__C__D__func_0" \
  "camlFoo__bar_baz_42" \
  "camlStdlib__array__map_154" \
  "camlBaz__Foo__Bar__init_0" \
  "camlFoo__"
