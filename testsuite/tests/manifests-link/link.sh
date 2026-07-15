#!/bin/sh
# Link a program whose inputs are resolved through a manifest file
# (see -I-manifest), simulating a content-addressed store: artifacts are
# stored under extensionless blob names and referenced by manifest entries.
#
# Arguments: $1=build_dir $2=ocamlrun $3=compiler $4=stdlib $5=runtime_dir
#            $6=ext (cmx or cmo) $7...=extra flags (optional)

set -eu

BUILD_DIR="$1"
OCAMLRUN="$2"
COMPILER="$3"
STDLIB="$4"
RUNTIME_DIR="$5"
EXT="$6"
shift 6

cd "$BUILD_DIR"

if [ "$EXT" = "cmx" ]; then LIBEXT=cmxa; else LIBEXT=cma; fi

compile () {
  "$OCAMLRUN" "$COMPILER" -nostdlib -I "$STDLIB" -I "$RUNTIME_DIR" "$@"
}

# A regular archive with two units, and an empty archive (no units).
compile -a -o mylib.$LIBEXT lib_a.$EXT lib_b.$EXT
compile -a -o empty.$LIBEXT

# Move the artifacts into a simulated content-addressed store, under
# extensionless blob names, so that:
#  - lookups cannot accidentally be satisfied by the current directory;
#  - file-type dispatch and companion-object ('.o'/'.a') resolution must use
#    the requested (logical) name, not the resolved path.
mkdir -p cas
mv helper.$EXT cas/blob_helper
mv link_manifest.$EXT cas/blob_main
mv mylib.$LIBEXT cas/blob_mylib
mv empty.$LIBEXT cas/blob_empty

cat > manifest.txt <<EOF
file helper.$EXT cas/blob_helper
file link_manifest.$EXT cas/blob_main
file mylib.$LIBEXT cas/blob_mylib
file empty.$LIBEXT cas/blob_empty
EOF

if [ "$EXT" = "cmx" ]; then
  # Native: '.o'/'.a' companions become separate store entries. The empty
  # archive deliberately has no '.a' entry (there may not even be a '.a' file
  # for an archive with no units).
  mv helper.o cas/blob_helper_o
  mv link_manifest.o cas/blob_main_o
  mv mylib.a cas/blob_mylib_a
  rm -f empty.a
  cat >> manifest.txt <<EOF
file helper.o cas/blob_helper_o
file link_manifest.o cas/blob_main_o
file mylib.a cas/blob_mylib_a
EOF
fi

# Link by bare (logical) names: every input below is resolved through the
# manifest. The empty archive exercises the missing-'.a'-is-fine case.
MANIFEST_FILES_ROOT="$BUILD_DIR"
export MANIFEST_FILES_ROOT

compile "$@" -I-manifest manifest.txt \
  helper.$EXT mylib.$LIBEXT empty.$LIBEXT link_manifest.$EXT \
  -o link_manifest.exe

if [ "$EXT" = "cmx" ]; then
  # Negative test: dropping the '.a' entry of a non-empty archive must fail,
  # mentioning the missing file.
  grep -v "^file mylib\.a " manifest.txt > manifest_no_a.txt
  if compile "$@" -I-manifest manifest_no_a.txt \
       helper.$EXT mylib.$LIBEXT empty.$LIBEXT link_manifest.$EXT \
       -o should_fail.exe 2> negative.err; then
    echo "ERROR: link unexpectedly succeeded without a mylib.a entry" >&2
    exit 1
  fi
  if ! grep -q "mylib\.a" negative.err; then
    echo "ERROR: expected the error to mention mylib.a, got:" >&2
    cat negative.err >&2
    exit 1
  fi
fi
