#!/bin/bash

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "Usage: $0 <artifact-url>"
  echo "Downloads a GitHub Actions artifact containing a patch and applies it"
  exit 1
fi

ARTIFACT_URL="$1"
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

echo "Downloading artifact from GitHub..."
gh api "$ARTIFACT_URL" > "$TEMP_DIR/artifact.zip"

echo "Extracting patch file..."
unzip -q "$TEMP_DIR/artifact.zip" -d "$TEMP_DIR"

PATCH_FILE="$TEMP_DIR/test-diffs.patch"
if [ ! -f "$PATCH_FILE" ]; then
  echo "Error: test-diffs.patch not found in artifact"
  exit 1
fi

echo "Applying patch..."
patch -p1 < "$PATCH_FILE"

echo "Patch applied successfully!"
