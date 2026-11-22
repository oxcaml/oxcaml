#!/bin/sh

# usage: sh poll_object_manifest.sh
#
# Makes a library root out of the default library tree and the tree of the
# variant with the other poll-insertion setting. Compiles a unit with and
# without polls, and puts the .cmx file and the object file with polls in
# the tree with polls, and the object file without polls in the tree
# without, all under names without extension. Links a program whose other
# units and libraries have polls, twice, with a manifest that names the
# unit info in the tree with polls and the object file in one tree or the
# other. With the object file without polls the link must be refused; with
# the other one the program is fully polled, and its loop polls, which it
# finds out by running it.

set -e

. "$(dirname "$0")/poll_pair.sh"

root=$(pwd)/root
mkdir -p "$root/variants/$other"
for entry in "$stdlib"/*; do
  case $entry in
    */variants) ;;
    *) ln -s "$entry" "$root/";;
  esac
done
for entry in "$stdlib/variants/$other"/*; do
  ln -s "$entry" "$root/variants/$other/"
done
if [ "$with_polls" = "$stdlib" ]; then
  with_tree=$root
  without_tree=$root/variants/$other
else
  with_tree=$root/variants/$other
  without_tree=$root
fi
OCAMLLIB=$root
MANIFEST_FILES_ROOT=/
export OCAMLLIB MANIFEST_FILES_ROOT

with=-enable-poll-insertion
without=-disable-poll-insertion
unit=poll_object_unit

# usage: compile <arguments>
# Runs the compiler. What it prints is shown when it fails, and only then:
# a warning that the settings of a run add (for a library without .cmx
# files, under -O3) is not what this test compares.
compile () {
  if ! $ocamlopt "$@" > compile.log 2>&1; then
    cat compile.log
    exit 2
  fi
}

mkdir with without "$with_tree/pkg" "$without_tree/pkg"
compile $with -c -o with/$unit.cmx $unit.ml
compile $without -c -o without/$unit.cmx $unit.ml
compile $with -alert -all -I +unix -I with -c \
  -o with/poll_object_loop.cmx poll_object_loop.ml
cp with/$unit.cmx "$with_tree/pkg/unit-info"
cp with/$unit.o "$with_tree/pkg/object-with-polls"
cp without/$unit.o "$without_tree/pkg/object-without-polls"

# usage: link <description> <object file>
link () {
  {
    echo "file $unit.cmx ${with_tree#/}/pkg/unit-info"
    echo "file $unit.o ${2#/}"
  } > manifest
  manifest=$(pwd)/manifest
  if $ocamlopt $with -I +unix unix.cmxa -I-manifest "${manifest#/}" \
       -o program.exe $unit.cmx with/poll_object_loop.cmx > link.log 2>&1
  then
    echo "$1: $(./program.exe 2>&1)"
  elif grep -q caml_code_not_fully_polled link.log; then
    echo "$1: refused, as an object file without polls"
  else
    echo "$1: refused for another reason:"
    cat link.log
  fi
  rm -f program.exe
}

link "unit info of the tree with polls, object file of the tree without" \
  "$without_tree/pkg/object-without-polls"
link "unit info and object file of the tree with polls" \
  "$with_tree/pkg/object-with-polls"

# An object file that both trees have under one name, named in the default
# tree: each file of a manifest is mapped on its own, whatever the manifest
# says of the unit info, so the link takes the one of the tree with polls,
# which is the selected tree. (Where the default tree is the one with
# polls, it is the selected tree and nothing is mapped: the case then
# repeats the one before it.)
cp with/$unit.o "$with_tree/pkg/body"
cp without/$unit.o "$without_tree/pkg/body"
link "unit info of the tree with polls, object file that both trees have" \
  "$root/pkg/body"
