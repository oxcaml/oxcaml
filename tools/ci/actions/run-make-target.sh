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

# Run `make TARGET` for a build.yml step, with core dumps enabled.
#
# Environment:
#   COLLECT_METRICS   if "true", append flags to BUILD_OCAMLPARAM and
#                     OCAMLPARAM so the compiler writes per-file profiling
#                     CSVs into $GITHUB_WORKSPACE/_profile_csv
#   BUILD_OCAMLPARAM, OCAMLPARAM, OCAMLRUNPARAM, USE_RUNTIME
#                     passed through to make

set -euo pipefail
usage() { echo "Usage: $0 TARGET" >&2; exit 1; }
[ $# -eq 1 ] || usage
target=$1

if [ "${COLLECT_METRICS:-}" = "true" ]; then
  profile_dir="$GITHUB_WORKSPACE/_profile_csv"
  mkdir -p "$profile_dir"
  profile_flags="dump-dir=$profile_dir,dump-into-csv=1,profile=1"
  export BUILD_OCAMLPARAM="${BUILD_OCAMLPARAM:-},$profile_flags"
  export OCAMLPARAM="${OCAMLPARAM:-},$profile_flags"
fi

ulimit -c unlimited
make "$target"
