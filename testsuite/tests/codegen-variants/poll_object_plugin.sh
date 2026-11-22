#!/bin/sh

# usage: sh poll_object_plugin.sh
#
# Makes four plugins from the unit of poll_object.ml, compiled with and
# without poll insertion, and has a fully polled program load them:
# - without.cmxs, from the .cmx file and the object file without polls;
# - mismatched.cmxs, from the .cmx file with polls and the object file
#   without;
# - object.cmxs, from the object file without polls alone;
# - with.cmxs, from the .cmx file and the object file with polls.
# The second and the third must be refused. The second by the linker that
# makes it, or by the dynamic linker when it is loaded: the line that this
# prints for it is the same in both cases. The third by the dynamic linker:
# it has no unit, so the linker has nothing to hold against it, and a
# program that is not fully polled loads it.

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

mkdir with without mismatched
compile $with -c -o with/$unit.cmx $unit.ml
compile $without -c -o without/$unit.cmx $unit.ml
cp with/$unit.cmx with/$unit.cmi mismatched/
cp without/$unit.o mismatched/

# usage: plugin <name> <files>
# Makes <name>.cmxs. For mismatched.cmxs, a link that fails on the symbol
# of the object files without polls is one way to refuse the plugin. Any
# other failure is shown, and so is any failure for the other plugins,
# which must link.
refused_by_the_linker=
plugin () {
  name=$1
  shift
  if $ocamlopt $with -shared -o $name.cmxs "$@" > $name.log 2>&1; then
    plugins="$plugins $name.cmxs"
  elif [ $name = mismatched ] \
       && grep -q caml_code_not_fully_polled $name.log; then
    refused_by_the_linker="$refused_by_the_linker $name.cmxs"
  else
    echo "$name.cmxs: its link fails:"
    cat $name.log
  fi
}

plugins=
plugin without without/$unit.cmx
plugin mismatched mismatched/$unit.cmx
plugin object without/$unit.o
plugin with with/$unit.cmx

compile $with -I +dynlink dynlink.cmxa -o host.exe poll_object_plugin.ml

# usage: host <domains> <plugins>
# Runs the program with that limit on the number of its domains, whatever
# limit the environment of the test sets.
host () {
  domains=$1
  shift
  OCAMLRUNPARAM="${OCAMLRUNPARAM:+$OCAMLRUNPARAM,}d=$domains" ./host.exe "$@"
}

host 2 $plugins > host.log 2>&1

# The plugins in the order above, wherever each was refused.
for name in without mismatched object with; do
  case " $refused_by_the_linker " in
    *" $name.cmxs "*)
      echo "$name.cmxs: refused, as an object file without polls";;
    *) grep "^$name.cmxs: " host.log;;
  esac
done
grep -v -e '^without.cmxs: ' -e '^mismatched.cmxs: ' -e '^object.cmxs: ' \
  -e '^with.cmxs: ' host.log

# With one domain the program loads the plugin that says what it is.
echo "one domain: $(host 1 without.cmxs 2>&1)"

# A program that is not fully polled defines the symbol of the object files
# without polls, and loads the plugin that is made of one alone.
compile $without -I +dynlink dynlink.cmxa -o unpolled.exe \
  poll_object_plugin.ml
echo "not fully polled: $(./unpolled.exe object.cmxs 2>&1)"
