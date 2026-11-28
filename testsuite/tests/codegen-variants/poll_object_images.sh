#!/bin/sh

# usage: sh poll_object_images.sh
#
# Makes a shared object that holds an OCaml image without polls, and links
# with it two programs made of the unit of poll_object.ml and of
# poll_object_loop.ml: one from the .cmx file with polls and the object
# file without, one from the .cmx file and the object file with polls.
# Then makes a plugin of each of the two pairs.

ocamlopt="$ocamlrun $ocamlopt_byte"
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

# The shared object exports that symbol and nothing else. One that exports
# all the symbols of its image also gives the program its runtime: the
# linker takes no member of an archive for a symbol that a shared object
# already defines. The program would then run on the runtime of the other
# image, which is not what this test is about. (With AddressSanitizer it
# would also lose the options that the runtime sets for the sanitizer,
# and fail in the check for leaks when it exits.)
# The runtime that goes into a shared object is the position-independent
# one: the other is not always compiled so that it can.
echo '{ global: caml_code_not_fully_polled; local: *; };' > exports
compile $without -output-complete-obj -runtime-variant _pic \
  -o libpoll_object_images.so \
  -ccopt "-Wl,--version-script=$(pwd)/exports" poll_object_images.ml
# The symbols are written to a file, and the file is searched: a search
# that ends at the first match would close the pipe on a tool that is
# still writing, and some say so.
if ! nm -D --defined-only libpoll_object_images.so > exported 2> nm.log
then
  echo "the symbols of the shared object cannot be read:"
  cat nm.log
elif ! grep -q ' [A-Za-z] caml_code_not_fully_polled$' exported; then
  echo "the shared object does not define the symbol"
fi

mkdir with without mismatched
compile $with -c -o with/$unit.cmx $unit.ml
compile $without -c -o without/$unit.cmx $unit.ml
cp with/$unit.cmx with/$unit.cmi mismatched/
cp without/$unit.o mismatched/
compile $with -alert -all -I +unix -I with -c \
  -o with/poll_object_loop.cmx poll_object_loop.ml

# usage: link <description> <directory of the unit>
# The shared object, and no other library, is made a dependency of the
# program whether or not the program takes a symbol from it, which some
# linkers go by unless told: a program that links is one of two images,
# and must not start without the shared object.
link () {
  if $ocamlopt $with -I +unix unix.cmxa -o program.exe "$2/$unit.cmx" \
       with/poll_object_loop.cmx -ccopt -L. -ccopt "-Wl,-rpath,$(pwd)" \
       -cclib -Wl,--push-state,--no-as-needed -cclib -lpoll_object_images \
       -cclib -Wl,--pop-state > link.log 2>&1
  then
    echo "$1: $(./program.exe 2>&1)"
    mv libpoll_object_images.so absent.so
    if ./program.exe > /dev/null 2>&1; then
      echo "$1: the program starts without the shared object"
    fi
    mv absent.so libpoll_object_images.so
  elif grep -q caml_code_not_fully_polled link.log; then
    echo "$1: refused, as an object file without polls"
  else
    echo "$1: refused for another reason:"
    cat link.log
  fi
  rm -f program.exe
}

link "unit info with polls, object file without, another image" mismatched
link "unit info and object file with polls, another image" with

# usage: plugin <description> <directory of the unit>
plugin () {
  if $ocamlopt $with -shared -o plugin.cmxs "$2/$unit.cmx" > link.log 2>&1
  then
    echo "$1: links"
  elif grep -q caml_code_not_fully_polled link.log; then
    echo "$1: refused, as an object file without polls"
  else
    echo "$1: refused for another reason:"
    cat link.log
  fi
  rm -f plugin.cmxs
}

plugin "unit info with polls, object file without, in a plugin" mismatched
plugin "unit info and object file with polls, in a plugin" with
