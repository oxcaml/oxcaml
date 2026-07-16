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
  if ! grep -q "manifest" negative.err; then
    echo "ERROR: expected the error to mention manifests, got:" >&2
    cat negative.err >&2
    exit 1
  fi
fi

if [ "$EXT" = "cmx" ] && [ "$(uname -s)" = "Linux" ] && [ "$(uname -m)" = "x86_64" ]; then
  # The dissector (-dissector) inserts an extra link-time pass that reads
  # every link input; it must classify inputs by their content (magic bytes),
  # not their file names, to work with manifest-resolved blob paths.
  #
  # Also give a second empty archive a '.a' companion sharing one blob with
  # the first, as happens in a content-addressed store (identical files, one
  # path). The dissector must tolerate the repeated path instead of rejecting
  # it as a duplicate input.
  compile -a -o empty2.$LIBEXT
  mv empty2.$LIBEXT cas/blob_empty2
  printf '!<arch>\n' > cas/blob_shared_empty_a
  cat >> manifest.txt <<MANIFEST
file empty2.$LIBEXT cas/blob_empty2
file empty.a cas/blob_shared_empty_a
file empty2.a cas/blob_shared_empty_a
MANIFEST

  compile "$@" -dissector -I-manifest manifest.txt \
    helper.$EXT mylib.$LIBEXT empty.$LIBEXT empty2.$LIBEXT link_manifest.$EXT \
    -o dissected.exe
  ./link_manifest.exe > expected.out
  ./dissected.exe > dissected.out
  diff expected.out dissected.out

  # Negative test: a link input that is neither an ELF object nor an ar
  # archive must be a hard error; silently skipping it would drop its code
  # from the final link.
  echo "this is not an object file" > cas/blob_text
  sed 's|^file mylib\.a .*|file mylib.a cas/blob_text|' manifest.txt \
    > manifest_bad_kind.txt
  if compile "$@" -dissector -I-manifest manifest_bad_kind.txt \
       helper.$EXT mylib.$LIBEXT empty.$LIBEXT empty2.$LIBEXT link_manifest.$EXT \
       -o should_fail_kind.exe 2> negative_kind.err; then
    echo "ERROR: dissected link unexpectedly succeeded with a bogus mylib.a" >&2
    exit 1
  fi
  if ! grep -q "neither an ELF object file nor an ar archive" negative_kind.err
  then
    echo "ERROR: expected an unrecognized-input error, got:" >&2
    cat negative_kind.err >&2
    exit 1
  fi

  # Same for a zero-length input, which additionally exercises mapping an
  # empty file.
  : > cas/blob_empty_file
  sed 's|^file mylib\.a .*|file mylib.a cas/blob_empty_file|' manifest.txt \
    > manifest_empty_kind.txt
  if compile "$@" -dissector -I-manifest manifest_empty_kind.txt \
       helper.$EXT mylib.$LIBEXT empty.$LIBEXT empty2.$LIBEXT link_manifest.$EXT \
       -o should_fail_empty.exe 2> negative_empty.err; then
    echo "ERROR: dissected link unexpectedly succeeded with an empty mylib.a" >&2
    exit 1
  fi
  if ! grep -q "neither an ELF object file nor an ar archive" negative_empty.err
  then
    echo "ERROR: expected an unrecognized-input error for an empty file, got:" >&2
    cat negative_empty.err >&2
    exit 1
  fi
fi
