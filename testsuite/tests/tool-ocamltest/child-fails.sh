#!/bin/sh

# Runs ocamltest on a file as child-ocamltest.sh does, and checks that the
# test fails and that ocamltest gives the reason in the file named by the
# third argument.

tsl="$1"
ocamltest="$2"
reason="$3"

if sh "$(dirname "$0")/child-ocamltest.sh" "$tsl" "$ocamltest" > output
then
  echo "the test did not fail:"
  cat output
  exit 1
fi
if ! grep -q -F -f "$reason" output; then
  echo "the test failed for another reason:"
  cat output
  exit 1
fi
