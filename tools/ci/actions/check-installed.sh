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

# Build the bootstrap checker and run it in a disposable working directory.

set -euo pipefail
usage() { echo "Usage: $0 PREFIX [--bundled]" >&2; exit 1; }
case $# in
  1) ;;
  2) [ "$2" = --bundled ] || usage ;;
  *) usage ;;
esac
script_dir=$(cd "$(dirname "$0")" && pwd)
prefix=$(cd -- "$1" && pwd)
shift
# Resolve bootstrap tools before the checker prepends the installed bin to PATH.
ocamlfind=$(command -v ocamlfind)
dune=$(command -v dune)
case "$ocamlfind" in /*) ;; *) ocamlfind="$PWD/$ocamlfind" ;; esac
case "$dune" in /*) ;; *) dune="$PWD/$dune" ;; esac
findlib_version=$("$ocamlfind" query -format '%v' findlib)
dune_version=$("$dune" --version)
ocamlopt_version=$("$ocamlfind" ocamlopt -version)
printf 'Bootstrap ocamlfind: %s (%s)\n' "$ocamlfind" "$findlib_version"
printf 'Bootstrap dune: %s (%s)\n' "$dune" "$dune_version"
printf 'Bootstrap ocamlopt: %s\n' "$ocamlopt_version"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$script_dir/check_installed.ml" "$work/"
(cd "$work" && "$ocamlfind" ocamlopt -package findlib,unix -linkpkg \
  check_installed.ml -o check_installed.exe)
mkdir "$work/run"
cd "$work/run"
# OCaml 5.4 lacks Unix.unsetenv; empty OCAMLLIB is not unset.
unset OCAMLLIB CAMLLIB OCAMLPATH CAML_LD_LIBRARY_PATH OCAMLFIND_CONF \
  OCAMLFIND_COMMANDS OCAMLFIND_TOOLCHAIN OPAM_SWITCH_PREFIX OPAMROOT
"$work/check_installed.exe" --lists-dir "$script_dir" \
  --ocamlfind "$ocamlfind" --dune "$dune" "$prefix" "$@"
