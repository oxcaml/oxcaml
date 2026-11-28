#!/bin/sh

# usage: sh toplevel_load_cmx.sh
#
# Compiles a unit in a directory, and loads its .cmx file in the native
# toplevel with the current directory left out of the load path: by its
# name, the directory being given with -I under a relative name, and by
# its path.

set -e

# The base variant, whatever the testsuite is running under.
unset OCAMLPARAM
ocamlopt="$ocamlrun $ocamlopt_byte"

mkdir sub
echo 'let () = print_endline "loaded"' > sub/unit.ml
(cd sub && $ocamlopt -c unit.ml)

toplevel="$ocamlsrcdir/ocamlnat -noinit -no-version -noprompt -nocwd"
echo "by name: $(echo '#load "unit.cmx";;' | $toplevel -I sub 2>&1)"
echo "by path: $(echo '#load "./sub/unit.cmx";;' | $toplevel 2>&1)"
