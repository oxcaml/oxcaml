#!/bin/sh

# Helper for variant_only.ml: runs ocamltest on test files, as
# child-ocamltest.sh does, for a codegen variant other than the default one
# and with -codegen-variant-only, and checks which of them it runs.

ocamltest="$1"
tree="$2"
tests="$3"

# A variant that the compiler under test and the test tree both have. The
# default variant has no directory of its own.
variant=
for hosts in "$tree"/testsuite/tools/variants/*; do
  name=$(basename "$hosts")
  if [ -d "$tree/stdlib/variants/$name" ]; then
    variant="$name"
    break
  fi
done
if [ -z "$variant" ]; then
  echo "the compiler under test has one codegen variant: nothing to check"
  exit 0
fi

# report <name>: what ocamltest says of variant_only_<name>.tsl
report () {
  sed -e 's/CHILDTEST/TEST/' "$tests/variant_only_$1.tsl" > "$1.ml"
  "$ocamltest" -codegen-variant "$variant" -codegen-variant-only "$1.ml" 2>&1
}

# These start commands that are given the variant: they run.
for name in commandline ocamlmklib run; do
  if ! report "$name" | grep -q "=> passed"; then
    echo "$name: not run for the codegen variant $variant:"
    report "$name"
    exit 1
  fi
done

# Nothing here depends on the variant: it is skipped.
if ! report bytecode | grep -q -F "=> skipped (nothing in this test depends"
then
  echo "bytecode: not skipped for the codegen variant $variant:"
  report bytecode
  exit 1
fi
