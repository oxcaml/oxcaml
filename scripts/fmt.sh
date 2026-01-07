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

# needed for the root dune file to parse
touch dune.runtime_selection duneconf/dirs-to-ignore.inc duneconf/ox-extra.inc

exit_code=0
dune build @fmt --auto-promote || exit_code=1
# Format owee files explicitly (external/ is vendored so dune skips it)
ocamlformat -i external/owee/owee_archive.ml external/owee/owee_archive.mli || exit_code=1
scripts/80ch.sh || exit_code=1
exit $exit_code
