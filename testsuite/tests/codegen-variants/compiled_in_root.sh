#!/bin/sh

# usage: sh compiled_in_root.sh <library root> <variant>
#
# Makes a library root (see library_root.sh) with the source of the test in
# it.  When <variant> has a tree of its own, that tree gets files named like
# the ones that compiling the source writes next to it, which are not
# objects at all.
#
# Checks first what library_root.sh promises: no directory of the root is a
# symbolic link, through which a file that a test adds would land in
# <library root>.

set -eu

. "$(dirname "$0")/library_root.sh"
make_library_root "$1" "$2"
find -L root -type d | while IFS= read -r directory; do
  if [ -L "$directory" ]; then
    echo "$directory is a symbolic link"
    exit 1
  fi
done
cp compiled_in_root.ml root/
if [ "$selected" != root ]; then
  for extension in cmx o; do
    echo "not written by this compilation" \
      > "$selected/compiled_in_root.$extension"
  done
fi
