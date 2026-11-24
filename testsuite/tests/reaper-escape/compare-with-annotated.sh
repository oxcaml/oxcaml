#!/bin/sh

set -eu

# usage: compare-with-annotated.sh TEST
#
# TEST_annotated.ml is TEST.ml with the [local_] and [exclave_] annotations
# that the escape analysis is expected to infer, compiled without the escape
# analysis. Its reaper dump must be the reference dump of TEST, which is
# compiled with the analysis, up to the name of the compilation unit; or, when
# the two cannot be reconciled, differ from it as recorded in
# TEST_annotated.diff (lines starting with "<" are from TEST, with ">" from
# TEST_annotated).

inferred=$1
annotated=${inferred}_annotated

capitalize () {
  first=$(printf '%s' "$1" | cut -c1 | tr '[:lower:]' '[:upper:]')
  rest=$(printf '%s' "$1" | cut -c2-)
  printf '%s%s' "${first}" "${rest}"
}

# The layout of a dump depends on the length of the names, so the dumps are
# laid out again, with one binding or handler per line.
normalize () {
  sed -e "s/$(capitalize "$2")/Test/g" -e "s/$2/test/g" "$1" |
    tr -s '[:space:]' ' ' |
    sed -e 's/ )/)/g' -e 's/( /(/g' |
    sed -e 's/ in /&\
/g' -e 's/ where /\
where /g' -e 's/ | /\
| /g' -e 's/ and code /\
and code /g'
  echo
}

normalize "${test_build_directory}/${annotated}.reaper.fl" "${annotated}" \
  > "${test_build_directory}/${annotated}.normalized"
normalize "${test_source_directory}/${inferred}.reaper.reference" \
  "${inferred}" > "${test_build_directory}/${inferred}.normalized"

diff "${test_build_directory}/${inferred}.normalized" \
  "${test_build_directory}/${annotated}.normalized" \
  > "${test_build_directory}/${annotated}.diff" || true

expected="${test_source_directory}/${annotated}.diff"
if test -f "${expected}"; then
  if cmp -s "${expected}" "${test_build_directory}/${annotated}.diff"; then
    exit "${TEST_PASS}"
  else
    echo "The differences with the inferred program are not the expected ones:"
    diff "${expected}" "${test_build_directory}/${annotated}.diff" || true
    exit "${TEST_FAIL}"
  fi
elif test -s "${test_build_directory}/${annotated}.diff"; then
  echo "The annotated program is not compiled like the inferred one:"
  cat "${test_build_directory}/${annotated}.diff"
  exit "${TEST_FAIL}"
else
  exit "${TEST_PASS}"
fi
