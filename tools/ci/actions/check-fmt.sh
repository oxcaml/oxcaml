#!/usr/bin/env bash
set -euo pipefail

# Derived from the former 'check-fmt' target in the Makefile.

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "$0: Tree must be clean to check formatting" >&2
  exit 1
fi

dune build @fmt

if [ -z "${SKIP_80CH:-}" ]; then
  bash "$repo_root/scripts/80ch.sh"
fi


if ! git diff --no-ext-diff --quiet; then
  echo >&2
  echo "The following code was not formatted correctly:" >&2
  echo "(the + side of the diff is how it should be formatted)" >&2
  echo "(working copy now contains correctly-formatted code)" >&2
  echo >&2
  git diff --no-ext-diff
  exit 1
fi
