#!/bin/sh
# Test ocamlfilt demangling with the Flat1 scheme (OCaml >= 5.3)
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

${OCAMLFILT} --format flat1 \
  "camlFoo" \
  "camlFoo_" \
  "camlFoo__bar_0" \
  "camlA__B__C__D__func_0" \
  "camlFoo__bar_baz_42" \
  "camlStdlib__array__map_154" \
  "camlBaz__Foo__Bar__init_0" \
  "camlFoo__" \
  'camlFoo__$3e$3e$3d_12' \
  'camlFoo__$2b$2b_5' \
  'camlIndexing__.$25$28$29_2_14_code' \
  'camlA$B$C$D$func_0' \
  'camlFoo$$$3e$$3e$$3d_12' \
  'camlFoo$$$2b$$2b_5' \
  'camlIndexing$$$2e$$25$$28$$29_2_14_code' \
  'camlStdlib$anon_fn$$5bstdlib$$2eml$$3a334$$2c0$$2d$$2d54$$5d_1453' \
  'camlList$add_42' \
  'camlBuffer$add_string_5_28_code' \
  '_camlFoo__bar_0' \
  '_camlStdlib__List__map_15_113_code'
