#!/bin/sh
# Link a program with -link-jsir from inputs that are found through a manifest
# (see -I-manifest), as in a content-addressed store: the files of the store
# have no extension.
#
# Arguments: $1=build_dir $2=ocamlsrcdir $3=target (js or wasm)

set -eu

BUILD_DIR="$1"
OCAMLSRCDIR="$2"
TARGET="$3"

cd "$BUILD_DIR"

OXCAML_JS_OF_OCAML="$OCAMLSRCDIR/js_of_ocaml"
OXCAML_WASM_OF_OCAML="$OCAMLSRCDIR/wasm_of_ocaml"
MANIFEST_FILES_ROOT="$BUILD_DIR"
export OXCAML_JS_OF_OCAML OXCAML_WASM_OF_OCAML MANIFEST_FILES_ROOT

compile () {
  "$OCAMLSRCDIR/ocamlopt.opt" -target "${TARGET}_of_ocaml" -link-jsir \
    -nostdlib -I "$OCAMLSRCDIR/stdlib" "$@"
}

OBJ=cmx-$TARGET
LIB=cmxa-$TARGET
JSIR=cm-jsir-$TARGET
JSIR_LIB=cma-jsir-$TARGET

compile -a -o mylib.$LIB link_manifest_b.$OBJ
compile -a -o empty.$LIB

mkdir -p store
mv link_manifest_a.$OBJ store/a
mv link_manifest_a.$JSIR store/a_jsir
mv mylib.$LIB store/mylib
mv mylib.$JSIR_LIB store/mylib_jsir
mv empty.$JSIR_LIB store/empty_jsir
mv link_manifest.$OBJ store/main
mv link_manifest.$JSIR store/main_jsir

cat > manifest.txt <<EOF
file link_manifest_a.$OBJ store/a
file link_manifest_a.$JSIR store/a_jsir
file mylib.$LIB store/mylib
file mylib.$JSIR_LIB store/mylib_jsir
file link_manifest.$OBJ store/main
file link_manifest.$JSIR store/main_jsir
EOF

compile -I-manifest manifest.txt \
  link_manifest_a.$OBJ mylib.$LIB link_manifest.$OBJ -o link_manifest.js

# An archive that does not have a unit it should have is an error, which names
# the file of the store.
sed "s|^file mylib\.$JSIR_LIB .*|file mylib.$JSIR_LIB store/empty_jsir|" \
  manifest.txt > manifest_empty.txt
if compile -I-manifest manifest_empty.txt \
     link_manifest_a.$OBJ mylib.$LIB link_manifest.$OBJ -o should_fail.js \
     2> should_fail.err; then
  echo "ERROR: linked without the JSIR of Link_manifest_b" >&2
  exit 1
fi
if ! grep -q "store/empty_jsir" should_fail.err; then
  echo "ERROR: expected the error to name store/empty_jsir, got:" >&2
  cat should_fail.err >&2
  exit 1
fi
