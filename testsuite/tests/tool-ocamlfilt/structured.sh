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
  "_CamlU6StdlibF3mapL0_" \
  "_CamlU3FooS0_" \
  "_CamlU3FooF3barZ0_" \
  "_CamlU3FooF3barP2_M5InnerF3addD1_D2_" \
  "_CamlU3FooF3barP0_D1_D2_" \
  "_CamlU3FooF3barL0_P1_F1fD3_D4_" \
  "_CamlU3FooM3BarM3BazF6my_fun" \
  "_CamlU3FooFu5_0foo" \
  "_CamlU4MainF9say_helloD0_D5_" \
  "_CamlU4MainM4TestF3fooD1_D6_" \
  "_CamlU12Stdlib__ListF3mapD15_D113_" \
  "_CamlU4MainF4mainS1_D300_" \
  "_CamlU4MainF4mainL0_D4_D9_" \
  "_CamlU3FooF3barL0_L1_D7_" \
  "_CamlU3FooM3BarL2_F3bazL0_D1_D2_" \
  "_CamlU8Functor2F8combinedL0_D1_D3_" \
  "_CamlU3FooS0_F4initL0_D5_D6_" \
  "_CamlU3FooM3BarIU3BazF3qux" \
  "_CamlU3FooM3BarO5ShapeF4area"

# Stamps are shown on request, and everything after the last item is rejected:
# the pre-stamp-item [_<n>_code] suffix is no longer valid, and neither is a
# number without its terminator.
${OCAMLFILT} --format structured --stamps \
  "_CamlU4MainF9say_helloD0_D5_" \
  "_CamlU4MainM4TestF3fooD6_" \
  "_CamlU3FooM3BarF3baz"
${OCAMLFILT} --format structured \
  "_CamlU4MainF11say_hello_0_5_code" \
  "_CamlU4MainF9say_helloD0_D5" \
  "_CamlU4MainF9say_helloD0_D5_x" \
  "_CamlU4MainF4mainL0" \
  "_CamlU3FooF3barP3_F1f"
