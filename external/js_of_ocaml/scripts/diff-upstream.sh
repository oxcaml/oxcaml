#!/bin/bash
# Show how external/js_of_ocaml diverges from its upstream base.
#
# Usage: ./external/js_of_ocaml/scripts/diff-upstream.sh [--full]
#   default : the upstream base revision, every patch commit ever applied on
#             top of the subtree, and a diffstat of the current divergence
#   --full  : print the full diff instead of the diffstat
set -e -u -o pipefail

prefix=external/js_of_ocaml
cd "$(git rev-parse --show-toplevel)"

sq=$(git log --max-count=1 --format=%H --grep="^git-subtree-dir: $prefix\$")
rev=$(git show --no-patch --format=%B "$sq" | sed -n 's/^git-subtree-split: //p')

echo "upstream base: $rev"
echo
echo "== patch commits applied on top of the subtree (whole history) =="
git log --oneline --no-merges HEAD -- "$prefix"
echo
echo "== current diff vs upstream base =="
if [ "${1:-}" = "--full" ]; then
  git diff "$sq" "HEAD:$prefix"
else
  git diff --stat "$sq" "HEAD:$prefix"
fi
