#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                   Liam Stevenson, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Raise an error any tracked file contains a conflict marker (`<<<<<<<` or
# `>>>>>>>` at the start of a line).

set -euo pipefail

cd "$( dirname "$0" )"
repo_root=$(git rev-parse --show-toplevel 2>/dev/null)
cd "$repo_root"

start_marker="<<<<<<<"
middle_marker="\|\|\|\|\|\|\|"
end_marker=">>>>>>>"

matches=$(git grep --no-color -nE \
  -e "^${start_marker}" \
  -e "^${middle_marker}" \
  -e "^${end_marker}") || true

if [ -n "$matches" ]; then
  {
    printf 'Error: Found conflict markers in the following locations:\n\n'
    printf '%s\n' "$matches"
  } >&2
  exit 1
fi
