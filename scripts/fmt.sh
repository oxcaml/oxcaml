#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2025 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

set -euo pipefail

repo_root=$(git rev-parse --show-toplevel) || exit 1
cd "$repo_root" || exit 1

# Use configured opam switch from Makefile.config
if [ -f Makefile.config ]; then
  opam_exec=$(grep '^opam_exec = ' Makefile.config 2>/dev/null \
    | cut -d= -f2- | sed 's/^ *//')
fi
opam_exec=${opam_exec:-}

# needed for the root dune file to parse
touch dune.runtime_selection duneconf/dirs-to-ignore.inc duneconf/ox-extra.inc

exit_code=0
$opam_exec dune build @fmt --auto-promote || exit_code=1
# Format owee files explicitly (external/ is vendored so dune skips it)
$opam_exec ocamlformat -i external/owee/owee_archive.ml external/owee/owee_archive.mli || exit_code=1
scripts/80ch.sh || exit_code=1
exit $exit_code
