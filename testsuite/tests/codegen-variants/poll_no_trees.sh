#!/bin/sh

# usage: sh poll_no_trees.sh
#
# Compiles the units of poll_no_trees.ml with and without poll insertion
# against a copy of the default library tree ($ocamlsrcdir/stdlib) that has
# no tree of another codegen variant, and links and runs them with each
# setting. Checks that a link with poll insertion is told when these
# libraries have none.

set -e

mkdir root
for entry in "$ocamlsrcdir"/stdlib/*; do
  case $entry in
    */variants) ;;
    *) ln -s "$entry" root/;;
  esac
done
OCAMLLIB=$(pwd)/root
export OCAMLLIB
# Not the defaults for the codegen variant that the testsuite may be
# running under: this root has the default tree only.
unset OCAMLPARAM

ocamlopt="$ocamlrun $ocamlopt_byte"
echo "variants: [$($ocamlopt -config-var codegen_variants)]"

mkdir with without
$ocamlopt -enable-poll-insertion -c -o with/poll_link_a.cmx poll_link_a.ml
$ocamlopt -disable-poll-insertion -I with -c -o without/poll_link_b.cmx \
  poll_link_b.ml

# usage: link <expected number of alerts> <flags>
# Links and runs the program. Prints what the compiler printed if that is
# not the alert about the libraries, the expected number of times, naming
# this library root and a unit of the standard library in it.
link () {
  expected=$1
  shift
  $ocamlopt "$@" -I without -o program.exe with/poll_link_a.cmx \
    without/poll_link_b.cmx poll_no_trees.ml 2> link.log
  echo "$*: $(./program.exe)"
  alerts=$(grep -c 'Alert no_polled_libraries' link.log || true)
  named=$(grep 'Alert no_polled_libraries' link.log \
            | grep -F "the libraries in $OCAMLLIB," \
            | grep -c -F '(CamlinternalFormatBasics, for one)' || true)
  others=$(grep -c -v -e 'Alert no_polled_libraries' -e '^File "_none_"' \
             link.log || true)
  if [ "$alerts" != "$expected" ] || [ "$named" != "$expected" ] \
     || [ "$others" != 0 ]
  then
    echo "expected the alert $expected times, about $OCAMLLIB and" \
      "CamlinternalFormatBasics, and nothing else:"
    cat link.log
  fi
}

# The one library tree is that of the default variant.
if [ "$($ocamlopt -config-var poll_insertion)" = true ]; then
  without_polled_libraries=0
else
  without_polled_libraries=1
fi
link $without_polled_libraries -enable-poll-insertion
link 0 -disable-poll-insertion
link 0 -enable-poll-insertion -alert -no_polled_libraries

# A program whose units all have polls, linked with poll insertion, is
# fully polled when these libraries are, and then spawns a domain. When
# they are not, it cannot, and the failure has to name the libraries: the
# units of the program are not the cause, and the flag that the message
# advises was given.
$ocamlopt -enable-poll-insertion \
  -alert -no_polled_libraries-unsafe_multidomain-do_not_spawn_domains \
  -o spawn.exe poll_spawn.ml 2> spawn.log
cat spawn.log
spawn=$(./spawn.exe)
case $without_polled_libraries:$spawn in
  0:"spawned: 42") spawn="what these libraries allow";;
  1:"spawn failed: "*"or of the libraries it is linked with"*)
    spawn="what these libraries allow";;
esac
echo "units with polls, linked with: $spawn"

# The alert is about the units that are linked, not about the tree that is
# searched: an object file made from a unit that takes nothing from these
# libraries is fully polled, and the link that makes it has nothing to say.
echo 'let identity x = x' > nothing.ml
$ocamlopt -enable-poll-insertion -output-obj -o nothing_object.o nothing.ml \
  2> nothing.log
echo "no unit of the libraries linked:" \
  "$(grep -c 'Alert no_polled_libraries' nothing.log) alerts"
