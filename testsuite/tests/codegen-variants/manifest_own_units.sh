#!/bin/sh

# usage: sh manifest_own_units.sh <library root> <variant> <directory>
#
# In <directory>, the current one: makes a library root (see
# library_root.sh) with the source of the helper unit in it, and
# own.manifest, which names the interface and the .cmx file that compiling
# the helper writes next to its source, for a MANIFEST_FILES_ROOT that is
# the root directory.  When <variant> has a tree of its own, that tree gets
# files of the same names, and an object, which are not what a compiler
# writes.

set -eu

. "$(dirname "$0")/library_root.sh"
make_library_root "$1" "$2"
cp manifest_own_units_helper.ml root/
for extension in cmi cmx; do
  file=manifest_own_units_helper.$extension
  echo "file $file ${3#/}/root/$file"
done > own.manifest
if [ "$selected" != root ]; then
  for extension in cmi cmx o; do
    echo "not written by this compilation" \
      > "$selected/manifest_own_units_helper.$extension"
  done
fi
