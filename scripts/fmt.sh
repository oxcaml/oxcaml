#!/usr/bin/env bash
set -euo pipefail

# Derived from the former 'fmt' target in the Makefile.

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd "${script_dir}/.." && pwd)
cd "$repo_root"

find . -not -path "./external/js_of_ocaml/*" \
  \( -name "*.ml" -or -name "*.mli" \) | \
  xargs -P "$(nproc 2>/dev/null || echo 1)" -n 20 ocamlformat -i

if [ -z "${SKIP_80CH:-}" ]; then
  bash "$repo_root/scripts/80ch.sh"
fi
