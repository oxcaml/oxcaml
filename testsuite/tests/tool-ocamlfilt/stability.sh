#!/bin/sh
# Check that demangled structured names survive unrelated edits: see the
# description in stability.ml. For each of the library and the consumer,
# the structured symbols of the object compiled from the source as written
# and from its [_edited] variant are demangled, the [_edited] unit names are
# rewritten to the original ones and the [unrelated_*] functions dropped,
# and the two lists must then be identical. The names the unedited unit
# defines are printed, so that the reference documents the stable names;
# cross-unit entries (references to the other unit, and copies of its
# closures that -O3 inlining may or may not create) are compared but not
# printed, so that the reference does not depend on the optimization level.
# The names the consumer defines in the library's namespace are the copies
# of library closures made by inlining; each must be a name the library
# defines itself, or the copies would not aggregate with the original.
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

# As [demangled_names], for the symbols the object defines (not those it
# merely refers to).
defined_names () {
  nm "$1" | awk 'NF == 3 && $2 != "U" && $NF ~ /^_?_Caml[A-Z]/ { print $NF }' \
    | "$OCAMLFILT" --format structured \
    | sed -E "$2" | grep -v 'unrelated_' | sort -u
}

# $1: unit name; $2: sed expression rewriting the [_edited] unit names, which
# matches nothing in the names of the unit as written; $3: the demangled
# prefix of the unit's own names.
check () {
  name=$1
  own=$3
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
  grep "^$own" "$D/$name.as_written"
}

check stability_lib 's/^Stability_lib_edited\./Stability_lib./' \
  'Stability_lib\.'
check stability \
  's/Stability_lib_edited\./Stability_lib./g; s/^Stability_edited\./Stability./' \
  'Stability\.'

# Without cross-module inlining (e.g. under [Oclassic]) there are no copies and
# the check is vacuous.
defined_names "$D/stability.o" 's/^//' | grep '^Stability_lib\.' > "$D/copies" \
  || true
comm -23 "$D/copies" "$D/stability_lib.as_written" > "$D/foreign"
if [ -s "$D/foreign" ]; then
  {
    echo "Copies of stability_lib closures in stability have names that"
    echo "stability_lib does not define:"
    cat "$D/foreign"
  } > "$ocamltest_response"
  exit "$TEST_FAIL"
fi
