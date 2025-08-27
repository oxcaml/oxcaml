#!/bin/sh

# clean-compiler-repo.sh - Clean the repository while preserving .claude and .vscode directories
#
# This script runs `git clean -dfX` while temporarily preserving the .claude
# and .vscode directories, which contain project-specific configuration that
# should not be removed during cleaning operations.

set -eu

# Get the top-level directory of the git repository
TOPLEVEL=$(git rev-parse --show-toplevel)

# Ensure we're in a git repository
if [ -z "$TOPLEVEL" ] || [ ! -d "$TOPLEVEL" ]; then
    echo "Error: Not in a git repository or could not determine repository root" >&2
    exit 1
fi

# Track whether we moved the directories
MOVED_CLAUDE=false
MOVED_VSCODE=false

# Check if .claude directory exists and move it temporarily
if [ -d "$TOPLEVEL/.claude" ]; then
    echo "Preserving .claude directory..."
    mv "$TOPLEVEL/.claude" "$TOPLEVEL/.claude-tmp"
    MOVED_CLAUDE=true
fi

# Check if .vscode directory exists and move it temporarily
if [ -d "$TOPLEVEL/.vscode" ]; then
    echo "Preserving .vscode directory..."
    mv "$TOPLEVEL/.vscode" "$TOPLEVEL/.vscode-tmp"
    MOVED_VSCODE=true
fi

# Run git clean and capture exit code
EXIT_CODE=0
git clean -dfX || EXIT_CODE=$?

# Restore .claude directory if it was moved
if [ "$MOVED_CLAUDE" = true ] && [ -d "$TOPLEVEL/.claude-tmp" ]; then
    mv "$TOPLEVEL/.claude-tmp" "$TOPLEVEL/.claude"
fi

# Restore .vscode directory if it was moved
if [ "$MOVED_VSCODE" = true ] && [ -d "$TOPLEVEL/.vscode-tmp" ]; then
    mv "$TOPLEVEL/.vscode-tmp" "$TOPLEVEL/.vscode"
fi

exit $EXIT_CODE