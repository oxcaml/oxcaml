#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Check installed META files and run native toplevel, JIT and eval consumers.
# Usage: check-installed-metadata.sh PREFIX

set -euo pipefail

[ -d "$1/lib/ocaml" ] || { echo "No install at $1" >&2; exit 1; }
prefix=$(cd "$1" && pwd)
script_dir=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Findlib uses the bootstrap compiler's ABI, not the installed compiler's.
cp "$script_dir/check_installed_metadata.ml" "$work/"
cd "$work"
echo 'Checking installed META paths and dependencies'
ocamlfind ocamlc -package findlib,unix -linkpkg check_installed_metadata.ml \
  -o check_metadata.exe

cat > findlib.conf <<EOF
path="$prefix/lib:$prefix/lib/ocaml"
stdlib="$prefix/lib/ocaml"
ocamlopt="$prefix/bin/ocamlopt"
ldconf="ignore"
EOF
export OCAMLFIND_CONF="$work/findlib.conf"
unset CAMLLIB OCAMLPATH OCAMLFIND_COMMANDS OCAMLFIND_TOOLCHAIN
# The bootstrap helper must not load stubs from the target's OCAMLLIB.
./check_metadata.exe "$prefix" "$script_dir/installed-unreferenced-archives.txt"
export OCAMLLIB="$prefix/lib/ocaml"

smoke() {
  local package=$1 source=$2
  shift 2
  echo "Checking native consumer of $package"
  printf '%s\n' "$source" > main.ml
  ocamlfind ocamlopt -package "$package" -linkpkg "$@" main.ml -o smoke.exe
  ./smoke.exe
}

smoke compiler-libs.native-toplevel \
  'let () = Opttoploop.initialize_toplevel_env ()'
smoke ocaml-jit 'let () = Jit.init_top ()'
# Check eval requires ocaml-jit: -uses-metaprogramming would mask its absence.
ocamlfind query -recursive -predicates native -format '%p' eval \
  | grep -Fxq ocaml-jit || {
    echo 'eval: missing ocaml-jit dependency' >&2
    exit 1
  }
smoke eval 'let () = ()' \
  -passopt -extension -passopt runtime_metaprogramming \
  -passopt -uses-metaprogramming
