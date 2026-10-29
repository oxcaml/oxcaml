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

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

# Use configured opam switch from Makefile.config
if [ -f Makefile.config ]; then
  opam_exec=$(grep '^opam_exec = ' Makefile.config 2>/dev/null \
    | cut -d= -f2- | sed 's/^ *//')
fi
opam_exec=${opam_exec:-}

# needed for the root dune file to parse
touch duneconf/dirs-to-ignore.inc duneconf/ox-extra.inc
# Placeholders for the dune fragments Makefile.common-ox generates from the
# configuration. Not in a configured tree, where they would stop make from
# generating the real ones.
if [ ! -f Makefile.config ]; then
  touch otherlibs/unix/byte/stubdll.inc otherlibs/str/byte/stubdll.inc \
    middle_end/flambda2/numbers/floats/stub_dll.inc \
    middle_end/flambda2/algorithms/stub_dll.inc \
    external/gc-timings/stub_dll.inc external/owee/stub_dll.inc \
    compilerlibs_stubs.inc
fi

exit_code=0
$opam_exec dune build @fmt || exit_code=1
if [[ -z "${SKIP_80CH+x}" ]]; then # don't use `-v` to accommodate macOS (old bash)
  scripts/80ch.sh || exit_code=1
fi
exit $exit_code
