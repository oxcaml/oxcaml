#!/bin/sh

# Run a nested ocamltest on a child test that uses the ocamlc.byte action, with
# OCAMLTEST_SKIP_BYTECODE_COMPILERS set, and report how that action was
# classified. Driven by skip-bytecode-compilers.ml, which passes the child's
# source path in $child.

set -u

copy=child_skip_bytecode.ml

sed -e 's/CHILDTEST/TEST/' "${child:?child must be set}" > "$copy"

OCAMLTEST_SKIP_BYTECODE_COMPILERS=1 \
  "${ocamlsrcdir}/ocamltest/ocamltest" "$copy" > child.log 2>&1
echo "child ocamltest exit: $?"

# Report the classification only, and the reason just for a skip: a failure's
# reason quotes absolute build paths, which would make the expectation
# machine-specific.
classification=$(
  sed -n 's/.*(ocamlc\.byte) => \([a-z]*\).*/\1/p' child.log | head -n 1
)
echo "ocamlc.byte => $classification"
if [ "$classification" = skipped ]; then
  sed -n 's/.*(ocamlc\.byte) => skipped (\(.*\))$/reason: \1/p' child.log \
    | head -n 1
fi
