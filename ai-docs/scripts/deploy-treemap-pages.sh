#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_REPO="$(cd "${SCRIPT_DIR}/../.." && pwd)"

PAGES_REPO="${PAGES_REPO:-$HOME/git/julesjacobs.github.io}"
TARGET_DIR="${PAGES_REPO}/misc/oxcaml-treemap"

SOURCE_HTML="${SOURCE_REPO}/ai-docs/source-treemap.html"
SOURCE_DATA="${SOURCE_REPO}/ai-docs/source-treemap-data.json"
SOURCE_SUMMARIES="${SOURCE_REPO}/ai-docs/source-treemap-typing-summaries.json"
DATA_GENERATOR="${SOURCE_REPO}/ai-docs/scripts/build-treemap-data.mjs"

REGENERATE_DATA="${REGENERATE_DATA:-1}"
PAGES_BRANCH="${PAGES_BRANCH:-$(git -C "$PAGES_REPO" rev-parse --abbrev-ref HEAD)}"
COMMIT_MESSAGE="${COMMIT_MESSAGE:-Update OxCaml treemap page}"

if [[ ! -d "$PAGES_REPO/.git" ]]; then
  echo "Missing github pages repo at $PAGES_REPO"
  exit 1
fi

if [[ ! -d "$SOURCE_REPO/.git" ]]; then
  echo "Missing source repo at $SOURCE_REPO"
  exit 1
fi

if [[ "${REGENERATE_DATA}" == "1" ]]; then
  node "$DATA_GENERATOR" "$SOURCE_REPO" "$SOURCE_DATA"
fi

mkdir -p "$TARGET_DIR"
mkdir -p "$TARGET_DIR/scripts"

cp "$SOURCE_HTML" "$TARGET_DIR/index.html"
cp "$SOURCE_HTML" "$TARGET_DIR/source-treemap.html"
cp "$SOURCE_DATA" "$TARGET_DIR/source-treemap-data.json"
cp "$SOURCE_SUMMARIES" "$TARGET_DIR/source-treemap-typing-summaries.json"
cp "$DATA_GENERATOR" "$TARGET_DIR/scripts/build-treemap-data.mjs"
cp "$SCRIPT_DIR/deploy-treemap-pages.sh" "$TARGET_DIR/scripts/deploy-treemap-pages.sh"

git -C "$PAGES_REPO" add "$TARGET_DIR"

if git -C "$PAGES_REPO" diff --cached --quiet "$TARGET_DIR"; then
  echo "No treemap updates to publish."
  exit 0
fi

git -C "$PAGES_REPO" commit -m "$COMMIT_MESSAGE"
git -C "$PAGES_REPO" push origin "$PAGES_BRANCH"
echo "Published treemap to https://julesjacobs.github.io/misc/oxcaml-treemap/"
