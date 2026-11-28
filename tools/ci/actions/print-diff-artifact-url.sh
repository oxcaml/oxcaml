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

# Print the command that applies the test-output patch which a build.yml
# job has just uploaded as the artifact ARTIFACT_NAME of this workflow run.
# Needs GH_TOKEN with the `actions: read` permission.

set -euo pipefail
usage() { echo "Usage: $0 ARTIFACT_NAME" >&2; exit 1; }
[ $# -eq 1 ] || usage
name=$1

# Wait a moment for the artifact to be registered.
sleep 5
url=$(gh api -X GET \
  "repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID/artifacts" \
  -f name="$name" --jq '.artifacts[0].archive_download_url // empty')
if [ -n "$url" ]; then
  echo "./tools/apply-diff.sh $url"
else
  echo "Artifact uploaded but URL not yet available. Check:" \
    "https://github.com/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"
fi
