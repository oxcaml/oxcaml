#!/bin/sh
# Test ocamlfilt demangling with the Flat0 scheme (OCaml <= 5.2)
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

${OCAMLFILT} --format flat0 \
  "camlFoo" \
  "camlFoo__bar_0" \
  "camlA__B__C__D__func_0" \
  "camlFoo__bar_baz_42" \
  "camlStdlib__array__map_154" \
  "camlBaz__Foo__Bar__init_0" \
  "camlFoo__" \
  'camlStdlib__bytes__$2b$2b_2205' \
  'camlFoo__$2b$2b$2b_1' \
  'camlStdlib__anon_fn$5bstdlib$2eml$3a334$2c0$2d$2d54$5d_1453' \
  '_camlFoo__bar_0' \
  '_camlStdlib__array__map_154'
