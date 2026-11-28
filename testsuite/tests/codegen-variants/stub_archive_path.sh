#!/bin/sh

# usage: sh stub_archive_path.sh <library root> <variant>
#
# Makes a library root (see library_root.sh) and adds C objects and archives
# to its directory probe.  A function defined in both trees returns 1 in the
# tree that a compilation for <variant> uses and 0 in the other one.

set -eu

. "$(dirname "$0")/library_root.sh"
make_library_root "$1" "$2"
mkdir -p root/probe "$selected/probe"

# object <file> <function> <value>
object () {
  printf 'long %s(long unit) { (void)unit; return %s; }\n' "$2" "$3" \
    > probe.c
  ${cc} -c probe.c -o "$1"
}

# archive <file> <function> <value>
archive () {
  object probe_member.o "$2" "$3"
  rm -f "$1"
  ar rc "$1" probe_member.o
}

# in_both_trees <object or archive> <file> <function>
in_both_trees () {
  if [ "$selected" != root ]; then
    "$1" "root/probe/$2" "$3" 0
  fi
  "$1" "$selected/probe/$2" "$3" 1
}

in_both_trees archive libprobe.a probe_archive
in_both_trees object probe.o probe_object
in_both_trees archive libcclib.a probe_cclib
in_both_trees archive librecorded.a probe_recorded
archive root/probe/libonly.a probe_default_tree_only 1
