#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "$script_dir/../.." && pwd)
pages_repo="${PAGES_REPO:-$HOME/git/julesjacobs.github.io}"
target_rel="misc/oxcaml/playground"
target_dir="${pages_repo}/${target_rel}"
build_dir="${repo_root}/_build/default/experiments/web_bytecode"
stage_dir=$(mktemp -d)
publish="${PUBLISH:-0}"
commit_message="${COMMIT_MESSAGE:-Deploy OxCaml playground}"
pages_branch="${PAGES_BRANCH:-$(git -C "$pages_repo" rev-parse --abbrev-ref HEAD)}"

cleanup() {
  rm -rf "$stage_dir"
}

trap cleanup EXIT

require_file() {
  local path=$1
  if [[ ! -e "$path" ]]; then
    echo "Missing required file: $path" >&2
    exit 1
  fi
}

if [[ ! -e "${repo_root}/.git" ]]; then
  echo "Missing source repo at $repo_root" >&2
  exit 1
fi

if [[ ! -d "${pages_repo}/.git" ]]; then
  echo "Missing github pages repo at $pages_repo" >&2
  exit 1
fi

if [[ "${SKIP_BUILD:-0}" != "1" ]]; then
  "$script_dir/build_browser_switch.sh"
fi

require_file "$script_dir/index.html"
require_file "$script_dir/embed_demo.html"
require_file "$script_dir/app.js"
require_file "$script_dir/backend.js"
require_file "$script_dir/oxcaml-embed.js"
require_file "$script_dir/oxcaml-embed-module.js"
require_file "$script_dir/runtime_shims.js"
require_file "$script_dir/sample_catalog.js"
require_file "$script_dir/unsupported_samples.js"
require_file "$build_dir/web_bytecode_js.bc.js"
require_file "$build_dir/browser_fs_manifest.json"
if [[ ! -d "$build_dir/browser_fs" ]]; then
  echo "Missing required directory: $build_dir/browser_fs" >&2
  exit 1
fi

mkdir -p "$stage_dir/build"
cp "$script_dir/index.html" "$stage_dir/index.html"
cp "$script_dir/embed_demo.html" "$stage_dir/embed_demo.html"
cp "$script_dir/app.js" "$stage_dir/app.js"
cp "$script_dir/backend.js" "$stage_dir/backend.js"
cp "$script_dir/oxcaml-embed.js" "$stage_dir/oxcaml-embed.js"
cp "$script_dir/oxcaml-embed-module.js" "$stage_dir/oxcaml-embed-module.js"
cp "$script_dir/runtime_shims.js" "$stage_dir/runtime_shims.js"
cp "$script_dir/sample_catalog.js" "$stage_dir/sample_catalog.js"
cp "$script_dir/unsupported_samples.js" "$stage_dir/unsupported_samples.js"
cp "$build_dir/web_bytecode_js.bc.js" "$stage_dir/build/web_bytecode_js.bc.js"
cp "$build_dir/browser_fs_manifest.json" "$stage_dir/build/browser_fs_manifest.json"
cp -R "$build_dir/browser_fs" "$stage_dir/build/browser_fs"

perl -0pi -e 's#^const buildBase = .*;$#const buildBase = "./build";#m' \
  "$stage_dir/app.js"
perl -0pi -e 's#^const buildBase = .*;$#const buildBase = "./build";#m' \
  "$stage_dir/backend.js"

mkdir -p "$target_dir"
rsync -a --delete "$stage_dir/" "$target_dir/"

changes=$(git -C "$pages_repo" status --short --ignored -- "$target_rel")
if [[ -z "$changes" ]]; then
  echo "No website changes for $target_rel"
  exit 0
fi

echo "Updated $target_dir"
echo "$changes"

if [[ "$publish" != "1" ]]; then
  echo "Local sync only. Re-run with PUBLISH=1 to commit and push ${target_rel}."
  exit 0
fi

git -C "$pages_repo" add -f --all -- "$target_rel"

if git -C "$pages_repo" diff --cached --quiet -- "$target_rel"; then
  echo "No staged website changes for $target_rel"
  exit 0
fi

git -C "$pages_repo" commit -m "$commit_message" -- "$target_rel"
git -C "$pages_repo" push origin "$pages_branch"
echo "Published playground to https://julesjacobs.github.io/${target_rel}/"
