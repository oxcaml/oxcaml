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

# After a failed build.yml test step, run `make TARGET` (`promote` or
# `promote-failed`) and save any resulting changes to the expected test
# outputs as $GITHUB_WORKSPACE/test-diffs.patch. Sets DIFF_CREATED (true or
# false) in $GITHUB_ENV for the later upload steps.

set -euo pipefail
usage() { echo "Usage: $0 TARGET" >&2; exit 1; }
[ $# -eq 1 ] || usage
target=$1

# `make promote-failed` runs each failed test again to promote its output,
# and so reports those tests as failed once more: its status says nothing
# about whether it promoted.
make "$target" || echo "make $target exited with status $?"
# ocamltest promotes into _runtest/testsuite. `make install_for_test`
# hard-links that tree to the sources, but a tree unpacked from another
# machine's archive (test-build.sh) is a copy: bring its changes back.
# Where the two are linked, this finds nothing to copy.
if [ -d _runtest/testsuite/tests ]; then
  rsync -rc --existing --exclude='_ocamltest*' \
    _runtest/testsuite/tests/ testsuite/tests/
fi
if ! git diff --quiet; then
  git diff > "$GITHUB_WORKSPACE/test-diffs.patch"
  echo "Test output differences detected and saved to artifact"
  echo "DIFF_CREATED=true" >> "$GITHUB_ENV"
else
  echo "No test output differences found"
  echo "DIFF_CREATED=false" >> "$GITHUB_ENV"
fi
