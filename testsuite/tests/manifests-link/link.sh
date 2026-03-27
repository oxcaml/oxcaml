#!/bin/sh
# Link using -manifest flag
# Arguments: $1=build_dir $2=ocamlrun $3=compiler $4=stdlib $5=runtime_dir
#            $6=ext (cmx or cmo) $7=extra_flags (optional)

BUILD_DIR="$1"
OCAMLRUN="$2"
COMPILER="$3"
STDLIB="$4"
RUNTIME_DIR="$5"
EXT="$6"
shift 6

cd "$BUILD_DIR"
# Copy object files to extensionless digest-like names to simulate blobstore
# layout. This tests that -manifest uses the logical filename (first column)
# for file type dispatch, not the filesystem path (second column).
cp helper.$EXT helper_abc123
cp link_manifest.$EXT link_manifest_def456

# Build the link manifest (lists files to link)
echo "file helper.$EXT helper_abc123" > link_manifest.txt
echo "file link_manifest.$EXT link_manifest_def456" >> link_manifest.txt

# Build the I-manifest (populates load path so Load_path.find resolves
# logical filenames to blobstore paths)
echo "file helper.$EXT helper_abc123" > i_manifest.txt
echo "file link_manifest.$EXT link_manifest_def456" >> i_manifest.txt

# For native code, also copy and register .o files
if [ "$EXT" = "cmx" ]; then
  cp helper.o helper_abc123_o
  cp link_manifest.o link_manifest_def456_o
  echo "file helper.o helper_abc123_o" >> i_manifest.txt
  echo "file link_manifest.o link_manifest_def456_o" >> i_manifest.txt
fi

MANIFEST_FILES_ROOT="$BUILD_DIR" \
  "$OCAMLRUN" "$COMPILER" \
  -nostdlib -I "$STDLIB" -I "$RUNTIME_DIR" \
  -I-manifest i_manifest.txt \
  "$@" \
  -manifest link_manifest.txt \
  -o link_manifest.exe
