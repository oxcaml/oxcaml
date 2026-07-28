#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                    James Rayman, Jane Street, New York                 *#
#*                                                                        *#
#*   Copyright 2025 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# added-lines.sh: Helpers shared by GitHub-actions-friendly scripts (such as
#                 80ch.sh and check-xxx.sh) which examine the lines added in
#                 the current feature. This file is meant to be sourced, not
#                 executed. Sourcing it sets $feature_base.

# Compute the feature base as the most recent common ancestor with the main
# branch.
# For GitHub Actions with a merge commit, HEAD^1 is the correct base.
# For local development, use merge-base with oxcaml/main.
if [ -n "${GITHUB_ACTIONS:-}" ]; then
  # In GitHub Actions, use the first parent of the merge commit
  feature_base="HEAD^1"
else
  # Check if remote/main exists
  if ! git rev-parse --verify main@{upstream} >/dev/null 2>&1; then
    echo "Error: Cannot find oxcaml/main branch." >&2
    echo "Please set up the oxcaml remote:" >&2
    echo "  git remote add oxcaml https://github.com/oxcaml/oxcaml.git" >&2
    echo "  git fetch oxcaml" >&2
    exit 1
  fi

  # remote/main exists, use merge-base to find common ancestor
  feature_base="$(git merge-base HEAD main@{upstream})"
fi

# Usage: for_each_added_line <file> <callback>
# Invoke `<callback> <file> <line-number> <line-content>` for each line of
# <file> added since $feature_base.
for_each_added_line() {
  local changed_file="$1"
  local callback="$2"

  # Parse git diff output to find added lines and their line numbers
  # This approach is portable and works on both Linux and macOS
  # Use --no-ext-diff to ensure we get standard git diff format
  git diff --no-ext-diff -U0 "$feature_base" -- "$changed_file" | {
    in_hunk=false
    while IFS= read -r line; do
      case "$line" in
        @@*)
          # Parse the @@ header to get the starting line number
          # Format: @@ -old_start,old_count +new_start,new_count @@
          # We need the new_start number
          hunk_info="${line#*+}"  # Remove everything before the +
          hunk_info="${hunk_info%%,*}"  # Get just the number before the comma
          hunk_info="${hunk_info%% *}"  # Remove anything after a space
          current_line="$hunk_info"
          in_hunk=true
          ;;
        +*)
          if [ "$in_hunk" = true ] && [ "${line#+++}" = "$line" ]; then
            # This is an added line (not the +++ header)
            "$callback" "$changed_file" "$current_line" "${line#+}"
            current_line=$((current_line + 1))
          fi
          ;;
        *)
          # Not an added line
          ;;
      esac
    done
  }
}

# Usage: run_added_lines_check <should_check_file> <line_callback>
# Check every line added since $feature_base in each changed file for which
# `<should_check_file> <file>` succeeds, passing the line to <line_callback>
# as in for_each_added_line. Then exit the script: with status 1 if the
# callback printed anything, and 0 otherwise.
run_added_lines_check() {
  local should_check_file="$1"
  local line_callback="$2"
  local output
  output="$(
    # Iterate through all files changed since this branch forked off of main.
    git diff --no-ext-diff --name-only "$feature_base" -z | \
    while read -d $'\0' -r changed_file
    do
      # Only check regular files that currently exist
      [ -f "$changed_file" ] || continue

      "$should_check_file" "$changed_file" || continue

      for_each_added_line "$changed_file" "$line_callback"
    done
  )"
  if [ -n "$output" ]; then
    printf '%s\n' "$output"
    exit 1
  else
    exit 0
  fi
}
