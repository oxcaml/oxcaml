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

# Build with the bootstrap toolchain, then run the selected installation check.
# Usage: check-installed.sh metadata PREFIX | libraries PREFIX core|shipped.

set -euo pipefail
script_dir=$(cd "$(dirname "$0")" && pwd)
source_root=$(cd "$script_dir/../../.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$script_dir/check_installed.ml" "$work/"
(cd "$work" && ocamlfind ocamlopt -package findlib,unix -linkpkg \
  check_installed.ml -o check_installed.exe)
"$work/check_installed.exe" --source-root "$source_root" "$@"
