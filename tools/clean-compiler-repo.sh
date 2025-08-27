#!/bin/sh

# clean-compiler-repo.sh - Clean the repository while preserving specified
# directories
#
# This script runs `git clean -dfX` while temporarily preserving the .claude
# and .vscode directories (by default), plus any additional files/directories
# specified as command-line arguments.
#
# Usage: clean-compiler-repo.sh [file1] [dir2] [...]

set -eu

# Get the top-level directory of the git repository
TOPLEVEL=$(git rev-parse --show-toplevel)

# Ensure we're in a git repository
if [ -z "$TOPLEVEL" ] || [ ! -d "$TOPLEVEL" ]; then
  echo "Error: Not in a git repository or could not determine repository root" >&2
  exit 1
fi

# Build list of items to preserve: default ones + command-line arguments
PRESERVE_ITEMS=".claude .vscode"
for arg in "$@"; do
    PRESERVE_ITEMS="$PRESERVE_ITEMS $arg"
done

# Array to track moved items and their temporary names
MOVED_ITEMS=""

# Function to get temporary name for an item
get_temp_name() {
  echo "$1-tmp-$$"
}

# Preserve each item by moving it temporarily
for item in $PRESERVE_ITEMS; do
  item_path="$TOPLEVEL/$item"
  if [ -e "$item_path" ]; then
    temp_name=$(get_temp_name "$item")
    temp_path="$TOPLEVEL/$temp_name"
    echo "Preserving $item..."
    mv "$item_path" "$temp_path"
    MOVED_ITEMS="$MOVED_ITEMS $item:$temp_name"
  fi
done

# Run git clean and capture exit code
EXIT_CODE=0
git clean -dfX || EXIT_CODE=$?

# Restore all moved items
for moved_item in $MOVED_ITEMS; do
  original_name=$(echo "$moved_item" | cut -d: -f1)
  temp_name=$(echo "$moved_item" | cut -d: -f2)
  temp_path="$TOPLEVEL/$temp_name"
  original_path="$TOPLEVEL/$original_name"

  if [ -e "$temp_path" ]; then
    mv "$temp_path" "$original_path"
  else
    echo "Warning: could not restore $original_name" >&2
  fi
done

exit $EXIT_CODE
