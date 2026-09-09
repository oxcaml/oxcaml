#!/bin/sh
# Check that demangled structured names survive unrelated edits: see the
# description in stability.ml. For each of the library and the consumer,
# the structured symbols of the object compiled from the source as written
# and from its [_edited] variant are demangled, the [_edited] unit names are
# rewritten to the original ones and the [unrelated_*] functions dropped,
# and the two lists must then be identical. The list for the unedited
# source is printed, so that the reference documents the stable names.
set -eu

# Fixed locale for a stable [sort] order across platforms.
export LC_ALL=C

OCAMLFILT="${ocamlsrcdir}/tools/ocamlfilt"
D="${test_build_directory}"

# $1: object file; $2: sed expression rewriting the [_edited] unit names.
# Structured symbols are those starting with [_Caml] (or [__Caml] on macOS,
# which ocamlfilt accepts as well); this includes references to other units.
demangled_names () {
  nm "$1" | awk '$NF ~ /^_?_Caml[A-Z]/ { print $NF }' \
    | "$OCAMLFILT" --format structured \
    | sed -E "$2" | grep -v 'unrelated_' | sort -u
}

# $1: unit name; $2: sed expression rewriting the [_edited] unit names, which
# matches nothing in the names of the unit as written.
check () {
  name=$1
  demangled_names "$D/$name.o" "$2" > "$D/$name.as_written"
  demangled_names "$D/${name}_edited.o" "$2" > "$D/$name.edited"
  if ! diff "$D/$name.as_written" "$D/$name.edited" > "$D/$name.diff"; then
    {
      echo "Demangled names of $name changed after an unrelated edit:"
      cat "$D/$name.diff"
    } > "$ocamltest_response"
    exit "$TEST_FAIL"
  fi
  echo "# $name"
  cat "$D/$name.as_written"
}

check stability_lib 's/^Stability_lib_edited\./Stability_lib./'
check stability \
  's/Stability_lib_edited\./Stability_lib./g; s/^Stability_edited\./Stability./'
