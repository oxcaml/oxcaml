#!/bin/sh

# Run a nested ocamltest with -promote on a test whose reference file does not
# exist, and report whether promotion created it. Driven by
# promote-missing-reference.ml, which passes the child's source path in $child.

set -u

copy=child_promote_missing.ml
reference=child_promote_missing.reference

# The child's output is this file; anything non-empty will do.
printf 'promoted line one\npromoted line two\n' > promote-missing-input

# CHILDTEST keeps the Make-based drivers from treating the child as a test.
sed -e 's/CHILDTEST/TEST/' "${child:?child must be set}" > "$copy"

rm -f "$reference"

"${ocamlsrcdir}/ocamltest/ocamltest" -promote "$copy" > child.log 2>&1
echo "child ocamltest exit: $?"

if [ -f "$reference" ]; then
  echo "reference created, contents:"
  cat "$reference"
else
  echo "reference not created"
fi
