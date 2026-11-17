#!/bin/sh

# usage: sh poll_other_setting.sh
#
# For every library tree of the installation under test (those under
# $ocamlsrcdir/stdlib, the default library root), compiles, links and runs
# poll_other_setting.ml for the codegen variant that differs from the
# tree's in poll insertion alone. Prints the failures, with what the
# compiler said, and "ok" if there is none.

root=$ocamlsrcdir/stdlib
ocamlopt="$ocamlrun $ocamlopt_byte"
OCAMLLIB=$root
export OCAMLLIB

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

status=ok
for tree in default $($ocamlopt -config-var codegen_variants); do
  # The variant of the default tree is the one that the compiler selects
  # by itself, and the only one of an installation without variants. The
  # other axes of the variant of another tree are given with OCAMLPARAM,
  # in place of the defaults for the variant that the testsuite may be
  # running under.
  case $tree in
    default)
      OCAMLPARAM=_
      polled=$($ocamlopt -config-var poll_insertion);;
    *)
      OCAMLPARAM=$(key "$tree" asan asan),$(key "$tree" fp frame-pointers)
      OCAMLPARAM=$OCAMLPARAM,$(key "$tree" sc stack-checks),_
      if has "$tree" poll; then polled=true; else polled=false; fi;;
  esac
  export OCAMLPARAM
  if $polled; then
    flag=-disable-poll-insertion
  else
    flag=-enable-poll-insertion
  fi
  if ! { $ocamlopt $flag -o program.exe poll_other_setting.ml \
           2> compiler.log \
         && ./program.exe; }; then
    status=failed
    echo "For the library tree of $tree, with $flag:"
    cat compiler.log
  fi
done
echo $status
