#!/bin/sh

# usage: sh per_file_axes.sh
#
# Makes ./root, the library root of the installation under test (as
# symbolic links) with an ocaml_compiler_internal_params file of its own:
# the one of the installation, if it has one, is not linked, so that
# nothing is written to it.  For the codegen variant of every library tree
# of the installation, and for base with poll insertion, which an
# installation need not have a tree for, the file sets the axes of that
# variant for first.ml alone, and an invocation with the axes of base
# compiles first.ml and then second.ml, and links first.ml.  Prints the
# failures, with what the compiler said, and "ok" if there is none.

ocamlopt="$ocamlrun $ocamlopt_byte"
objinfo="$ocamlrun $ocamlsrcdir/tools/ocamlobjinfo"
params=root/ocaml_compiler_internal_params
mkdir root
for entry in "$ocamlsrcdir"/stdlib/*; do
  case $entry in
    */ocaml_compiler_internal_params) ;;
    *) ln -s "$entry" root/;;
  esac
done
OCAMLLIB=$(pwd)/root
OCAMLPARAM=_
LC_ALL=C
export OCAMLLIB OCAMLPARAM LC_ALL

for unit in alone first second; do
  echo 'let () = print_endline "linked"' > $unit.ml
done

# usage: has <variant> <token>
has () {
  case -$1- in
    *-$2-*) true;;
    *) false;;
  esac
}

# usage: key <variant> <token> <OCAMLPARAM key>
key () {
  if has "$1" "$2"; then echo "$3=1"; else echo "$3=0"; fi
}

# usage: axes <.cmx file>
# The axes that the unit was compiled with: those of the whole program,
# and poll insertion.
axes () {
  $objinfo -no-code -no-approx "$1" | grep '^Codegen fingerprint:'
  $objinfo -no-code -no-approx "$1" | grep '^Polled:'
}

status=ok
failure () {
  status=failed
  echo "With the settings of $variant for first.ml: $1"
  cat compiler.log
}

for variant in $($ocamlopt -config-var codegen_variants) other-poll; do
  case $variant in
    other-poll) settings=poll-insertion=1;;
    *)
      settings="$(key "$variant" asan asan) \
        $(key "$variant" fp frame-pointers) \
        $(key "$variant" sc stack-checks) \
        $(key "$variant" poll poll-insertion)";;
  esac
  : > compiler.log

  # What a unit records when the whole invocation has the settings, and
  # when it has the axes of base.
  : > $params
  OCAMLPARAM=_,$(echo $settings | tr ' ' ,) $ocamlopt -c alone.ml \
    2> compiler.log
  with_settings=$(axes alone.cmx)
  $ocamlopt -c alone.ml 2>> compiler.log
  with_base_axes=$(axes alone.cmx)

  for setting in $settings; do
    echo "first.ml : ${setting%=*} = ${setting#*=}"
  done > $params
  rm -f first.cmx second.cmx program.exe
  $ocamlopt -c first.ml second.ml 2>> compiler.log
  if [ "$(axes first.cmx)" != "$with_settings" ]; then
    failure "first.ml was not compiled with them"
  fi
  if [ "$(axes second.cmx)" != "$with_base_axes" ]; then
    failure "second.ml was not compiled with the axes of the invocation"
  fi

  # The link has the axes of the invocation.  So it takes a unit that
  # differs from them in poll insertion alone, which is a property of each
  # unit, and refuses any other.
  $ocamlopt first.ml -o program.exe 2> compiler.log
  linked=$?
  if [ "${with_settings%Polled:*}" = "${with_base_axes%Polled:*}" ]; then
    if [ $linked != 0 ] || ! ./program.exe > /dev/null; then
      failure "the program was not linked"
    fi
  elif [ $linked = 0 ] || ! grep -q 'first\.cmx' compiler.log; then
    failure "the link did not refuse first.cmx"
  fi
done
echo $status
