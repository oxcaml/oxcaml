#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                    Luke Maurer, Jane Street Europe                     *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# check-xxx.sh: A GitHub-actions-friendly script which finds lines added in
#               the current feature that contain an upper-case "xxx" marker,
#               conventionally meaning "must fix before merging".

set -u

. "$(dirname "$0")/added-lines.sh"

# The marker is spelled in pieces so that this script does not flag itself.
marker='X''X''X'
# Match only runs of exactly three, so that longer runs (as in mktemp
# templates) are not flagged.
marker_regexp='(^|[^X])'$marker'([^X]|$)'

check_marker() {
  local changed_file="$1"
  local current_line="$2"
  local line_content="$3"
  if [[ $line_content =~ $marker_regexp ]]; then
    printf \
'::error file=%s,line=%s,title=%s::Line %s contains %s in %s\n' \
      "$changed_file" "$current_line" \
      "New $marker marker" \
      "$current_line" \
      "$marker" \
      "$changed_file"
  fi
}

should_check_file() {
  # Don't check the external directory, which holds vendored code.
  case "$1" in
    external/*)
      return 1 ;;
  esac
  return 0
}

run_added_lines_check should_check_file check_marker
