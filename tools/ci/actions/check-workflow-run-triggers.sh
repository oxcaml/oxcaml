#!/usr/bin/env bash

#******************************************************************************
#*                                  OxCaml                                    *
#* -------------------------------------------------------------------------- *
#*                               MIT License                                  *
#*                                                                            *
#* Copyright (c) 2026 Jane Street Group LLC                                   *
#* opensource-contacts@janestreet.com                                         *
#*                                                                            *
#* Permission is hereby granted, free of charge, to any person obtaining a    *
#* copy of this software and associated documentation files (the "Software"), *
#* to deal in the Software without restriction, including without limitation  *
#* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
#* and/or sell copies of the Software, and to permit persons to whom the      *
#* Software is furnished to do so, subject to the following conditions:       *
#*                                                                            *
#* The above copyright notice and this permission notice shall be included    *
#* in all copies or substantial portions of the Software.                     *
#*                                                                            *
#* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
#* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
#* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
#* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
#* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
#* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
#* DEALINGS IN THE SOFTWARE.                                                  *
#******************************************************************************

# Check that every name listed under a workflow's `on.workflow_run.workflows`
# is the name of exactly one workflow.
#
# GitHub matches those entries against the workflows' names: `name:`, or the
# workflow file's path from the root of the repository if it has none. When
# nothing matches, the trigger silently never fires, so renaming a workflow
# would silently stop every workflow triggered by it. A name shared by
# several workflows is rejected too, as the trigger would follow all of them.
#
# GitHub reads the entries as glob patterns. This check only understands
# literal names, so it rejects an entry that contains a pattern character.
#
# Run from the root of the repository. Needs yq v4
# (https://github.com/mikefarah/yq), which GitHub's Ubuntu runners have.

set -euo pipefail

usage() { echo "Usage: $0 [WORKFLOWS_DIR]" >&2; exit 2; }
[ $# -le 1 ] || usage
dir=${1:-.github/workflows}

shopt -s nullglob
files=("$dir"/*.yml "$dir"/*.yaml)
if [ ${#files[@]} -eq 0 ]; then
  echo "$0: no workflows in $dir" >&2
  exit 2
fi

names=()
for file in "${files[@]}"; do
  name=$(yq '.name // ""' "$file")
  names+=("${name:-$file}")
done

status=0
checked=0
for file in "${files[@]}"; do
  # Not a process substitution, whose failure would go unnoticed. `on` can
  # also be a string or a list of events, with no workflow_run filters.
  # flatten makes a lone string count like a list of one.
  wanted_names=$(
    yq '[.on | select(tag == "!!map") | .workflow_run.workflows // []]
        | flatten | .[]' "$file"
  )
  [ -n "$wanted_names" ] || continue
  while IFS= read -r wanted; do
    checked=$((checked + 1))
    case $wanted in
      *[]*?+[!\\]*)
        message="on.workflow_run.workflows lists \"$wanted\", which contains"
        message+=" a glob pattern character. This check only understands"
        message+=" literal workflow names."
        echo "::error file=$file::$message"
        status=1
        continue
        ;;
    esac
    matches=0
    for name in "${names[@]}"; do
      if [ "$name" = "$wanted" ]; then
        matches=$((matches + 1))
      fi
    done
    if [ "$matches" -ne 1 ]; then
      message="on.workflow_run.workflows lists \"$wanted\", which is the"
      message+=" name of $matches workflows in $dir rather than exactly one."
      message+=" If none has that name, this workflow is silently never"
      message+=" triggered."
      echo "::error file=$file::$message"
      status=1
    fi
  done <<< "$wanted_names"
done

echo "Checked $checked workflow_run trigger name(s) in $dir."
exit "$status"
