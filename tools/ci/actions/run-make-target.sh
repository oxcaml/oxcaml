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

# Run `make TARGET [ARGUMENT]...` for a build.yml step, with core dumps
# enabled.
#
# Environment:
#   COLLECT_METRICS   if "true", make the compiler write per-file profiling
#                     CSVs into $GITHUB_WORKSPACE/_profile_csv: while it is
#                     built, for the compilations of the main build context
#                     only (MAIN_CONTEXT_OCAMLPARAM, see Makefile.common-ox),
#                     and for those of the tests (OCAMLPARAM)
#   BUILD_OCAMLPARAM, OCAMLPARAM, OCAMLRUNPARAM, USE_RUNTIME
#                     passed through to make

set -euo pipefail
usage() { echo "Usage: $0 TARGET [ARGUMENT]..." >&2; exit 1; }
[ $# -ge 1 ] || usage

if [ "${COLLECT_METRICS:-}" = "true" ]; then
  profile_dir="$GITHUB_WORKSPACE/_profile_csv"
  mkdir -p "$profile_dir"
  profile_flags="dump-dir=$profile_dir,dump-into-csv=1,profile=1"
  # Not through BUILD_OCAMLPARAM, which also reaches the codegen-variant
  # contexts: their builds of the libraries are not part of the profile.
  export MAIN_CONTEXT_OCAMLPARAM="$profile_flags"
  export OCAMLPARAM="${OCAMLPARAM:-},$profile_flags"
fi

ulimit -c unlimited
make "$@"
