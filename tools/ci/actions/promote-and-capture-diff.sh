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

make "$target"
if ! git diff --quiet; then
  git diff > "$GITHUB_WORKSPACE/test-diffs.patch"
  echo "Test output differences detected and saved to artifact"
  echo "DIFF_CREATED=true" >> "$GITHUB_ENV"
else
  echo "No test output differences found"
  echo "DIFF_CREATED=false" >> "$GITHUB_ENV"
fi
