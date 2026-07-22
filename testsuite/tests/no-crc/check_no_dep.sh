#!/bin/sh
# Check that [-no-crc Dep] omitted Dep's row from both import tables of a .cmx.
# Argument: the ocamlobjinfo output file. We only inspect the two import-table
# sections (between "Interfaces imported:" and "Interfaces used in quotations:")
# so we are not confused by the Flambda 2 export dump that follows.
set -e
out="$1"
imports=$(sed -n '/^Interfaces imported:/,/^Interfaces used in quotations:/p' "$out")
printf '%s\n' "$imports"
if printf '%s\n' "$imports" | grep -qw Dep; then
  echo "FAIL: Dep should have been omitted from the .cmx import tables"
  exit 1
fi
if ! printf '%s\n' "$imports" | grep -qw Stdlib; then
  echo "FAIL: Stdlib unexpectedly missing from the .cmx interface imports"
  exit 1
fi
echo "OK: Dep omitted from both .cmx import tables"
