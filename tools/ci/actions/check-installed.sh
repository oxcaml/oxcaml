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

# Check metadata in PREFIX; --bundled also checks the complete Nix library set.
# Usage: check-installed.sh PREFIX [--bundled]

set -euo pipefail
script_dir=$(cd "$(dirname "$0")" && pwd)
prefix=$(cd -- "${1:?Usage: check-installed.sh PREFIX [--bundled]}" && pwd)
shift
work=$(mktemp -d)
work=$(cd "$work" && pwd)
trap 'rm -rf "$work"' EXIT
cp "$script_dir/check_installed.ml" "$work/"
(cd "$work" && ocamlfind ocamlopt -package findlib,unix -linkpkg \
  check_installed.ml -o check_installed.exe)
mkdir "$work/run"
cd "$work/run"
# OCaml 5.4 lacks Unix.unsetenv; empty OCAMLLIB is not unset.
unset OCAMLLIB CAMLLIB OCAMLPATH CAML_LD_LIBRARY_PATH OCAMLFIND_CONF \
  OCAMLFIND_COMMANDS OCAMLFIND_TOOLCHAIN OPAM_SWITCH_PREFIX OPAMROOT
"$work/check_installed.exe" --lists-dir "$script_dir" "$prefix" "$@"
