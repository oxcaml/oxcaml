#!/bin/sh

# usage: sh poll_alert.sh
#
# Links programs with poll insertion against the two library trees of the
# installation that differ only in poll insertion, the default one and
# the one of the variant with the other setting, and says for each link
# whether the alert no_polled_libraries was printed and what it names.

set -e

. "$(dirname "$0")/poll_pair.sh"
# Without symbolic links, so that the ".." components below lead back to
# the root of the file system.
with_polls=$(cd "$with_polls" && pwd -P)
without_polls=$(cd "$without_polls" && pwd -P)

# usage: named <expected directory, or none>
# What the alert of the link that wrote link.log names: its directory must
# be the expected one, and the unit is shown.
named () {
  alert=$(grep 'Alert no_polled_libraries' link.log || true)
  unit=$(echo "$alert" | sed -n -e 's/.*(\([A-Za-z_]*\), for one).*/\1/p')
  case $1:$alert in
    none:) echo "no alert";;
    none:*) echo "unexpected: $alert";;
    *:*"the libraries in $1, "*)
      echo "an alert that names its directory and the unit $unit";;
    *) echo "unexpected: ${alert:-no alert}";;
  esac
}

# usage: link <description> <expected directory, or none> <arguments>
# Links a program with poll insertion, runs it, and says what the alert
# names.
link () {
  description=$1
  expected=$2
  shift 2
  $ocamlopt -enable-poll-insertion -o program.exe "$@" 2> link.log
  echo "$description: $(./program.exe), $(named "$expected")"
}

# usage: object <description> <expected directory, or none> <arguments>
# The same for an object file made with -output-obj, which is not run.
object () {
  description=$1
  expected=$2
  shift 2
  $ocamlopt -enable-poll-insertion -output-obj -o code.o "$@" 2> link.log
  echo "$description: $(named "$expected")"
}

# The library root has polls, and a directory given with -I, searched
# before it, holds the standard library without them.
OCAMLLIB=$with_polls
export OCAMLLIB
link "standard library without polls found before the root with polls" \
  "$without_polls" -I "$without_polls" poll_alert.ml

# The same library, given to the link as a file: the alert does not depend
# on how the path of the file is spelled. (The directory is reached under
# another name, which is not a path inside the library root. Without the
# library that the compiler adds itself, a link is left to whoever writes
# it: this one stops at the object file.)
ln -s "$without_polls" libraries
$ocamlopt -enable-poll-insertion -c poll_alert.ml
object "standard library without polls given by absolute path" \
  "$(pwd)/libraries" -nopervasives "$(pwd)/libraries/stdlib.cmxa" \
  poll_alert.cmx
object "standard library without polls given by relative path" \
  libraries -nopervasives libraries/stdlib.cmxa poll_alert.cmx

# A link need not take the unit Stdlib from the library that holds it. An
# object file made with -output-obj does not end with Std_exit, which needs
# that unit, and this one takes a single module of the library.
echo 'let compare = Bool.compare' > poll_alert_module.ml
object "one module of the standard library without polls" \
  "$without_polls" -I "$without_polls" poll_alert_module.ml

# The unit that ends a program, Std_exit, is a file of its own, which the
# link finds like the library: here without polls, before the root.
mkdir exit
cp "$without_polls/std_exit.cmx" "$without_polls/std_exit.o" exit/
link "Std_exit without polls found before the root with polls" \
  exit -I exit poll_alert.ml

# The libraries have polls, and a unit of the program without polls is in
# their directory: in a copy of the tree, so as to leave the installation
# alone.
mkdir root
for entry in "$with_polls"/*; do
  ln -s "$entry" root/
done
mkdir root/own
echo 'let value = 1' > poll_alert_unit.ml
OCAMLLIB=$(pwd)/root
$ocamlopt -disable-poll-insertion -c -o root/own/poll_alert_unit.cmx \
  poll_alert_unit.ml
link "unit without polls in the directory of the libraries" \
  none "$OCAMLLIB/own/poll_alert_unit.cmx" poll_alert.ml

# The same for units of the program that are named like units of the
# standard library.
OCAMLLIB=$with_polls
echo 'let price = 1' > camlinternalPrice.ml
echo 'let foo = 2' > stdlib__foo.ml
$ocamlopt -disable-poll-insertion -c camlinternalPrice.ml stdlib__foo.ml
link "units without polls named like those of the standard library" \
  none camlinternalPrice.cmx stdlib__foo.cmx poll_alert.ml

# The same for a unit that is elsewhere, and is named by a path that goes
# into the directory of the libraries and out again.
OCAMLLIB=$with_polls
mkdir outside
$ocamlopt -disable-poll-insertion -c -o outside/poll_alert_unit.cmx \
  poll_alert_unit.ml
up=$(echo "$with_polls" | sed -e 's|[^/][^/]*|..|g')
link "unit without polls named through the directory of the libraries" \
  none "$with_polls$up$(pwd -P)/outside/poll_alert_unit.cmx" poll_alert.ml
