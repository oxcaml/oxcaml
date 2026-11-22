#!/bin/sh

# Helper for nested_ocamltest.ml, which selects a codegen variant: runs
# ocamltest on tests that select another variant, or none. Each of them
# compiles a unit from a script, whose compiler takes its variant from
# OCAMLPARAM, and links it with the compiler that ocamltest runs for the
# variant of that test. The link refuses a unit compiled for another one.

ocamltest="$1"

echo 'let f () = 42' > helper.ml

# nested <name> <selection> <text>: the test passes, or ocamltest says <text>
nested () {
  cat > "$1.ml" <<TEST
(* TEST
 readonly_files = "helper.ml";
 $2
 setup-ocamlopt.byte-build-env;
 script = "\${ocamlrun} \${ocamlopt_byte} -nostdlib -I \${stdlib} -c helper.ml";
 script;
 binary_modules = "helper";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)
let () = assert (Helper.f () = 42)
TEST
  env -u OCAMLTESTDIR "$ocamltest" "$1.ml" > "$1.output" 2>&1
  if ! grep -q -F -e "=> passed" -e "$3" "$1.output"; then
    echo "$1: ocamltest did not run the test for its own variant:"
    cat "$1.output"
    exit 1
  fi
}

# No selection: the default variant of the compiler under test.
nested unselected "" "=> passed"

# Frame pointers off and stack checks on, where the compiler under test
# has such a variant.
nested selected 'codegen_variant = "-fp+sc"; variant_available;' \
  'codegen variant meeting -fp+sc available" is not satisfied'

# An axis that OCAMLPARAM sets after the '_' is not a default: the
# compilers obey it whatever a test selects. ocamltest does not run, and
# names the setting and the value that it found it in, as it was.
refused="stack-checks=1,verbose=0,_,frame-pointers=1"
if OCAMLPARAM="$refused" \
     env -u OCAMLTESTDIR "$ocamltest" unselected.ml > refused.output 2>&1
then
  echo "ocamltest ran with an axis set after the '_' of OCAMLPARAM:"
  cat refused.output
  exit 1
fi
if ! grep -q -F "OCAMLPARAM=$refused in the environment sets \
frame-pointers=1 after" refused.output; then
  echo "ocamltest does not name the value and the setting that it refuses:"
  cat refused.output
  exit 1
fi

# Another setting there is none of ocamltest's business.
OCAMLPARAM="verbose=0,_,g=1" \
  env -u OCAMLTESTDIR "$ocamltest" unselected.ml > unrelated.output 2>&1
if ! grep -q -F "=> passed" unrelated.output; then
  echo "ocamltest did not run the test with OCAMLPARAM=verbose=0,_,g=1:"
  cat unrelated.output
  exit 1
fi
