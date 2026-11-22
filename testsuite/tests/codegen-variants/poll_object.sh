#!/bin/sh

# usage: sh poll_object.sh
#
# Compiles the unit of poll_object.ml with and without poll insertion, on
# its own and for a pack, and links programs from each pair of a .cmx file
# and an object file of the two compilations, with the libraries that have
# polls; then programs that get the object file without polls in other
# ways. Says what each program that links says of itself, and whether each
# failure is the one expected: an undefined symbol, the one that an object
# file without polls refers to.

ocamlopt="$ocamlrun $ocamlopt_byte"
with=-enable-poll-insertion
without=-disable-poll-insertion
unit=poll_object_unit
pack=poll_object_pack
for_pack="-for-pack Poll_object_pack"

# The two compilations of the unit: on its own in <setting>, and for the
# pack in <setting>-pack.
for setting in with without; do
  eval "flag=\$$setting"
  mkdir $setting $setting-pack
  $ocamlopt $flag -c -o $setting/$unit.cmx $unit.ml || exit 2
  $ocamlopt $flag $for_pack -c -o $setting-pack/$unit.cmx $unit.ml || exit 2
done

# usage: pair <directory> <directory of the .cmx file> <directory of the
#   object file>
# Makes <directory> the result of the compilation in the first directory
# given, but for the object file, which is that of the second.
pair () {
  mkdir "$1"
  cp "$2/$unit.cmx" "$2/$unit.cmi" "$1/"
  cp "$3/$unit.o" "$1/"
}

# usage: link <description> <arguments of the link>
# Links a program with poll insertion and runs it.
link () {
  description=$1
  shift
  if $ocamlopt $with -o program.exe "$@" > link.log 2>&1; then
    echo "$description: $(./program.exe 2>&1)"
  elif grep -q caml_code_not_fully_polled link.log; then
    echo "$description: refused, as an object file without polls"
  else
    echo "$description: refused for another reason:"
    cat link.log
  fi
  rm -f program.exe
}

# usage: program <directory>
# Compiles the main unit against the unit of the directory and links the
# two.
program () {
  $ocamlopt $with -I "$1" -c -o "$1/poll_object.cmx" poll_object.ml \
    || exit 2
  link "$2" "$1/$unit.cmx" "$1/poll_object.cmx"
}

pair with-with with with
pair with-without with without
pair without-with without with
pair without-without without without
program with-with "unit info and object file with polls"
program with-without "unit info with polls, object file without"
program without-with "unit info without polls, object file with"
program without-without "unit info and object file without polls"

# The same for a member of a pack: the pack is made, and it is the link of
# a program with it that fails.
packed () {
  $ocamlopt $with -pack -o "$1/$pack.cmx" "$1/$unit.cmx" || exit 2
  $ocamlopt $with -I "$1" -c -o "$1/poll_object_packed.cmx" \
    poll_object_packed.ml || exit 2
  link "$2" "$1/$pack.cmx" "$1/poll_object_packed.cmx"
}

# A compilation with polls that stops before it writes the object file
# writes the .cmx file: the object file of the compilation before it stays.
pair stopped without without
cp $unit.ml stopped/
(cd stopped && $ocamlopt $with -stop-after linearization -c $unit.ml) \
  || exit 2
program stopped "unit info of a compilation with polls that wrote no object"

# An archive of each compilation, and a unit compiled against the one with
# polls right after it: the unit info is read in that archive, the linker
# is given both, and which of the two it takes the object file from is its
# own business. Either the link fails on the object file without polls, or
# the program is fully polled and the loop that it runs is the one with
# polls: the program of poll_object_loop.ml finds that out by running it.
$ocamlopt $with -a -o with/with.cmxa with/$unit.cmx || exit 2
$ocamlopt $without -a -o without/without.cmxa without/$unit.cmx || exit 2
$ocamlopt $with -alert -all -I +unix -I with-with -c \
  -o with-with/poll_object_loop.cmx poll_object_loop.ml || exit 2

# usage: link_archives <description> <archives and units of the link>
link_archives () {
  description=$1
  shift
  if $ocamlopt $with -I +unix unix.cmxa -o program.exe "$@" \
       > link.log 2>&1
  then
    result=$(./program.exe 2>&1)
  elif grep -q caml_code_not_fully_polled link.log; then
    result="refused, as an object file without polls"
  else
    result="refused for another reason: $(cat link.log)"
  fi
  case $result in
    "refused, as an object file without polls" \
    | "fully polled, the loop polls")
      result="no loop without polls in a fully polled program";;
  esac
  echo "$description: $result"
  rm -f program.exe
}

link_archives "archive with polls, unit, archive without polls" \
  with/with.cmxa with-with/poll_object_loop.cmx without/without.cmxa
link_archives "archive without polls, archive with polls, unit" \
  without/without.cmxa with/with.cmxa with-with/poll_object_loop.cmx

# An object file without polls that comes with no unit info: one given to
# the linker as it is, beside a program whose units all have polls.
cp $unit.ml poll_object_other.ml
$ocamlopt $without -c poll_object_other.ml || exit 2
link "object file without polls and without unit info" \
  poll_object_other.o with-with/$unit.cmx with-with/poll_object.cmx

pair pack-with-with with-pack with-pack
pair pack-with-without with-pack without-pack
packed pack-with-with "pack, unit info and object file of the member with polls"
packed pack-with-without \
  "pack, unit info of the member with polls, object file without"
