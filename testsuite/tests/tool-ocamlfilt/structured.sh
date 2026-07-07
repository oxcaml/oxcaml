#!/bin/sh
# Test ocamlfilt demangling with the Structured scheme (OxCaml)
OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"

${OCAMLFILT} --format structured \
  "_CamlU3FooM3BarF3baz" \
  "_CamlU6StdlibF3map" \
  "_CamlU3FooO5MyObj" \
  "_CamlU3FooIU3BarF3qux" \
  "_CamlU3FooFu8A3e3e3d_" \
  "_CamlU3FooFu7D2a_let" \
  "_CamlU3FooFu14E27D27_funcsub" \
  "_CamlU6StdlibLu18G2e_stdlibml_334_0" \
  "_CamlU3FooS5_42_7" \
  "_CamlU3FooP5_10_5" \
  "_CamlU3FooM3BarM3BazF6my_fun" \
  "_CamlU3FooFu5_0foo" \
  "_CamlU4MainF11say_hello_0_5_code" \
  "_CamlU4MainM4TestF5foo_1_6_code" \
  "_CamlU12Stdlib__ListF6map_15_113_code" \
  "_CamlU4MainSu15E2e_testml_5_15_300" \
  "_CamlU4MainF4mainLu15E2e_mainml_7_32F4fn_4_9_code" \
  "_CamlU3FooF3barLu14D2e_fooml_3_15Lu14D2e_fooml_4_22F4fn_7" \
  "_CamlU3FooM3BarLu14D2e_fooml_5_10F3bazF4fn_1_2" \
  "_CamlU8Functor2F8combinedLu14D2e_fooml_8_12F4fn_1_3_code" \
  "_CamlU3FooSu14D2e_fooml_2_10F4initF4fn_5_6" \
  "_CamlU3FooF3barPu14D2e_fooml_9_15" \
  "_CamlU3FooM3BarIU3BazF3qux" \
  "_CamlU3FooM3BarO5ShapeF4area" \
  "_CamlU3FooL2__" \
  "_CamlU3FooLu14D2e_fooml__512" \
  "_CamlU3FooS3__7"
