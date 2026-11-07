#!/bin/sh
# Check that the standalone C implementation of the demangler
# (tools/demangle.c, reached through [ocamlfilt --c-implementation]) agrees
# with the OCaml implementation ([ocamlfilt] with its default options) on
# every input of a corpus; see c_implementation.ml.
#
# Usage: sh c_implementation.sh handwritten|objects [<object file>...]
#
# With [handwritten], a fixed corpus of symbols (the test vectors of
# doc/STRUCTURED_NAME_MANGLING.md and the inputs of the other tests in this
# directory) is compared first and its demangling is printed, so that the
# reference documents it. Then every symbol that nm reports for each object
# file, OCaml-mangled or not, is compared, so that pass-through is exercised
# too. Only a line per object is printed for those, since the symbols of an
# object depend on the inlining decisions of the compiler.
set -eu

# Fixed locale for a stable [sort] order across platforms.
export LC_ALL=C

OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
D="${test_build_directory}"
TAB="$(printf '\t')"

# $1: corpus name; $2: file with one input per line. Fails the test, listing
# the differing inputs, when the two implementations disagree.
compare () {
  name=$1
  inputs=$2
  "$OCAMLFILT" < "$inputs" > "$D/$name.ocaml"
  "$OCAMLFILT" --c-implementation < "$inputs" > "$D/$name.c"
  paste "$inputs" "$D/$name.ocaml" "$D/$name.c" \
    | awk -F "$TAB" \
        '$2 != $3 { printf "%s\n  OCaml: %s\n      C: %s\n", $1, $2, $3 }' \
    > "$D/$name.mismatches"
  if [ -s "$D/$name.mismatches" ]; then
    {
      echo "ocamlfilt and ocamlfilt --c-implementation disagree ($name):"
      cat "$D/$name.mismatches"
    } > "$ocamltest_response"
    exit "$TEST_FAIL"
  fi
}

MODE=${1:?"missing mode argument"}
shift

if [ "$MODE" = handwritten ]; then
  INPUTS="$D/handwritten.syms"
  # The test vectors of doc/STRUCTURED_NAME_MANGLING.md (with stamps made
  # up where the document leaves them unspecified), the inputs of
  # structured.sh, flat0.sh, flat1.sh and reject.sh, and some edge cases of
  # the reference implementation: a symbol made only of stamps or of an
  # inline marker, several inline markers, an escaped identifier that
  # decodes to the empty string, a callee path made only of stamps, the
  # largest and the smallest rejected number, and the flat-scheme corner
  # cases of the style detection.
  cat > "$INPUTS" <<'EOF'
_CamlU3FooM3BarF3baz
_CamlU6StdlibF3map
_CamlU3FooO5MyObj
_CamlU3FooIU3BarF3qux
_CamlU3FooM3BarIU3BazF3qux
_CamlU3FooFu8A3e3e3d_
_CamlU3FooFu7D2a_let
_CamlU3FooFu14E27D27_funcsub
_CamlU3FooFu5_0foo
_CamlU6StdlibF3mapL0_
_CamlU3FooS0_
_CamlU3FooF3barZ0_
_CamlU3FooF3barP2_M5InnerF3addD1_D2_
_CamlU3FooF3barP0_D1_D2_
_CamlU3FooF3barL0_P1_F1fD3_D4_
_CamlU3FooM3BarM3BazF6my_fun
_CamlU4MainF9say_helloD0_D5_
_CamlU4MainM4TestF3fooD1_D6_
_CamlU4MainM4TestF3fooD6_
_CamlU12Stdlib__ListF3mapD15_D113_
_CamlU4MainF4mainS0_D300_
_CamlU4MainF4mainL0_D4_D9_
_CamlU3FooF3barL0_L1_D7_
_CamlU3FooM3BarL2_F3bazL0_D1_D2_
_CamlU8Functor2F8combinedL0_D1_D3_
_CamlU3FooS0_F4initL0_D5_D6_
_CamlU3FooM3BarO5ShapeF4area
_CamlU22Naming_demo_lib__WorldF11double_evenD0_D3_
_CamlU10Lib____ArgF1f
_CamlU4Foo_F3bar
_CamlU5Foo__F3bar
_CamlU5__FooF3bar
_CamlU3FooI
_CamlU9E2e_ocamlM5OuterM6MiddleM5InnerM4LeafF5greetD1_D2_
_CamlU9E2e_ocamlM8Make_setF8Make_setD1_D2_
_CamlU9E2e_ocamlM8Make_setF3addD1_D2_
_CamlU9E2e_ocamlF17with_local_moduleM1MF5scaleD1_D2_
_CamlU9E2e_ocamlF12with_open_inL1_D1_D2_
_CamlU9E2e_ocamlM12Option_monadFu8A3e3e3d_D1_D2_
_CamlU9E2e_ocamlM12Option_monadF13with_let_starL0_D1_D2_
_CamlU9E2e_ocamlM8IndexingFu14A2e2528293c2d_D1_D2_
_CamlU9E2e_ocamlO6circleF2fnD1_D2_
_CamlU9E2e_ocamlF4pickS0_F1vD1_D2_
_CamlU9E2e_ocamlF4pickS1_F1vD1_D2_
_CamlU9E2e_ocamlM8OrdinalsF5thunkZ0_D1_D2_
_CamlU9E2e_ocamlM8OrdinalsF16lazy_then_lambdaL0_D1_D2_
_CamlU9E2e_ocamlM8OrdinalsF16lazy_then_lambdaZ0_D1_D2_
_CamlU9E2e_ocamlM11AlternatingF11outer_namedF12middle_namedL0_F11inner_namedL0_D1_D2_
_CamlU9E2e_ocamlM7PartialF10omit_firstP1_F8labelledD1_D2_
_CamlU9E2e_ocamlF20omit_first_qualifiedP2_M7PartialF8labelledD1_D2_
_CamlU9E2e_ocamlF7curriedP2_M7PartialF8labelledD1_D2_
_CamlU9E2e_ocamlM7PartialF3negD1_D2_
_CamlU10E2e_inlineIU14E2e_inline_libF8mk_adderL0_D1_D2_
_CamlU10E2e_inlineIU14E2e_inline_libF7pack_opS0_F5applyD1_D2_
_CamlU10E2e_inlineIU14E2e_inline_libM6ScalerF9scale_allL0_D1_D2_
_CamlU10E2e_inlineF7add_oneP2_U14E2e_inline_libF3addD1_
_CamlU10E2e_inlineF10omit_firstP2_U14E2e_inline_libF8labelledD1_D2_
_CamlU8ToplevelL0_D0_D3_
_CamlU8ToplevelL1_D1_D4_
_CamlU8ToplevelF2_aL0_D2_D5_
_CamlU29E2e_oxcamlOx_pack__E2e_oxcamlM8PipelineF6nestedL0_L0_D1_D2_
_CamlU29E2e_oxcamlOx_pack__E2e_oxcamlL0_D1_D2_
__CamlU3FooM3BarF3baz
_Caml
__Caml
_CamlU3FooX3bar
_CamlU99Foo
_CamlUFoo
_CamlU3FooFu6G1234_funcsub
_CamlU3FooFu11G2g_funcsub
_CamlU3FooFu11G2A_funcsub
_CamlU4MainF11say_hello_0_5_code
_CamlU4MainF9say_helloD0_D5
_CamlU4MainF9say_helloD0_D5_x
_CamlU4MainF4mainL0
_CamlU3FooF3barP3_F1f
_CamlU4611686018427387903Foo
_CamlD1_
_CamlI
_CamlU3FooID1_
_CamlU3FooIU3BarIU3Baz
_CamlU3FooIU3BarP2_IU3Baz
_CamlU3FooFu2A_
_CamlU3FooFu3AB_
_CamlU3FooFu7AB2a_xy
_CamlU3FooF3barP1_D1_
_CamlU3FooP1_I
_CamlU3FooP1_P0_
_CamlU3FooL007_
_CamlU3FooD4611686018427387903_
_CamlU3FooD4611686018427387904_
_CamlU1_
_CamlU2__
_Camlx
camlFoo
camlFoo_
camlFoo__
camlFoo__bar_0
camlA__B__C__D__func_0
camlFoo__bar_baz_42
camlStdlib__array__map_154
camlBaz__Foo__Bar__init_0
camlStdlib__bytes__$2b$2b_2205
camlFoo__$2b$2b$2b_1
camlStdlib__anon_fn$5bstdlib$2eml$3a334$2c0$2d$2d54$5d_1453
camlFoo__$3e$3e$3d_12
camlFoo__$2b$2b_5
camlIndexing__.$25$28$29_2_14_code
camlA$B$C$D$func_0
camlFoo$$$3e$$3e$$3d_12
camlFoo$$$2b$$2b_5
camlIndexing$$$2e$$25$$28$$29_2_14_code
camlStdlib$anon_fn$$5bstdlib$$2eml$$3a334$$2c0$$2d$$2d54$$5d_1453
camlList$add_42
camlBuffer$add_string_5_28_code
_camlFoo__bar_0
_camlStdlib__array__map_154
_camlStdlib__List__map_15_113_code
camlFoo__bar$
camlFoo$ab
camlFoo__x$2
camlFoo$$
camlFoo$
camlFoo$2
camlFoo$$2e
camlFoo.bar$$2e
camlF$2E
camlF$2E__g
main
_ZN4testE
caml_foo
caml_alloc
camlfoo
caml
_caml
__camlFoo__bar_0
EOF
  compare handwritten "$INPUTS"
  echo "# Hand-written corpus: $(wc -l < "$INPUTS" | tr -d ' ') inputs"
  paste "$INPUTS" "$D/handwritten.ocaml" \
    | awk -F "$TAB" '{
        print $1 "\t" ($1 == $2 ? "(unchanged)" : $2 == "" ? "(empty)" : $2)
      }' \
    | column -t -s "$TAB"
fi

for obj in "$@"; do
  name=$(basename "$obj")
  # Every symbol in the object, defined or referenced, OCaml-mangled or not.
  nm "$obj" | awk 'NF { print $NF }' | sort -u > "$D/$name.syms"
  if ! grep -Eq '^_?(caml|_Caml)[A-Z]' "$D/$name.syms"; then
    echo "no OCaml symbols found in $obj" > "$ocamltest_response"
    exit "$TEST_FAIL"
  fi
  compare "$name" "$D/$name.syms"
  echo "$name: the two implementations agree on every symbol"
done
