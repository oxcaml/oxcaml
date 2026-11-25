#!/bin/sh

set -eu

# usage: compare-with-annotated.sh TEST
#
# TEST_annotated.ml is TEST.ml with the [local_] and [exclave_] annotations
# that the escape analysis is expected to infer, compiled without the escape
# analysis. The annotated tests clear OCAMLPARAM so CI's testing preset
# cannot override their -no-experimental-reaper-escape flag.
# Its reaper dump must be the reference dump of TEST, which is
# compiled with the analysis, up to the name of the compilation unit; or, when
# the two cannot be reconciled, be the result of applying the patch
# TEST_annotated.diff (made by diff(1) from the dump of TEST to the one of
# TEST_annotated, both laid out as below) to the dump of TEST.

inferred=$1
annotated=${inferred}_annotated

fail () {
  echo "$1"
  exit "${TEST_FAIL}"
}

capitalize () {
  first=$(printf '%s' "$1" | cut -c1 | tr '[:lower:]' '[:upper:]')
  rest=$(printf '%s' "$1" | cut -c2-)
  printf '%s%s' "${first}" "${rest}"
}

# normalize DUMP NAME OUTPUT
#
# The layout of a dump depends on the length of the names, so the dumps are
# laid out again, with one binding or handler per line. The names of anonymous
# functions contain their location in the source, which is removed. Each step
# is run on its own so that a failure of any of them is one of the script.
normalize () {
  test -r "$1" || fail "Cannot read $1"
  sed -e "s/$(capitalize "$2")/Test/g" -e "s/$2/test/g" \
      -e 's/\[test\.ml:[0-9,-]*\]/[test.ml]/g' "$1" > "$3.1"
  tr -s '[:space:]' ' ' < "$3.1" > "$3.2"
  sed -e 's/ )/)/g' -e 's/( /(/g' "$3.2" > "$3.3"
  sed -e 's/ in /&\
/g' -e 's/ where /\
where /g' -e 's/ | /\
| /g' -e 's/ and code /\
and code /g' "$3.3" > "$3"
  echo >> "$3"
  test -s "$3" || fail "Empty dump: $1"
}

actual="${test_build_directory}/${annotated}.normalized"
base="${test_build_directory}/${inferred}.normalized"
expected="${test_build_directory}/${annotated}.expected"
patch="${test_source_directory}/${annotated}.diff"

normalize "${test_build_directory}/${annotated}.reaper.fl" "${annotated}" \
  "${actual}"
normalize "${test_source_directory}/${inferred}.reaper.reference" \
  "${inferred}" "${base}"

if test -f "${patch}"; then
  cp "${base}" "${expected}"
  patch -s "${expected}" "${patch}" ||
    fail "Cannot apply ${annotated}.diff to the dump of ${inferred}"
else
  cp "${base}" "${expected}"
fi

if cmp -s "${expected}" "${actual}"; then
  exit "${TEST_PASS}"
fi

status=0
diff "${expected}" "${actual}" || status=$?
if test "${status}" -ne 1; then
  fail "diff failed with status ${status}"
fi
fail "The annotated program is not compiled as expected (see above)"
