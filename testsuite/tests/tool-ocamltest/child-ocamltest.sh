#!/bin/sh

# This script runs ocamltest on a file after having replaced CHILDTEST with
# TEST. This allows testing that ocamltest catches syntax errors without the
# Make-based ocamltest drivers confusing the test test for a real test.

tsl="$1"
ocamltest="$2"

temp_tsl=$(mktemp child_test_temp_XXX.ml)
trap 'rm -f -- "$temp_tsl"' EXIT

sed -e 's/CHILDTEST/TEST/' "${tsl:?tsl must be set}" > "$temp_tsl"

"$ocamltest" "$temp_tsl" 2>&1
