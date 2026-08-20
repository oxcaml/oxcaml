#!/bin/bash
# Update the external/js_of_ocaml subtree to a new upstream revision.
#
# Usage: ./external/js_of_ocaml/scripts/update-js_of_ocaml.sh <upstream-rev>
#
# Run it from a clean tree, on a fresh branch off main. It creates a squash
# commit of <upstream-rev> and merges it into the subtree, carrying the
# patches applied on top of the subtree through the merge. Then resolve any
# conflicts, commit, and open a PR from the branch.
#
# A conflict means a patch overlaps an upstream change. Most of the time the
# patch (or an equivalent) has landed upstream: take the upstream side, which
# retires the patch. Otherwise, reconcile the patch with the new upstream
# code.
set -e -u -o pipefail

prefix=external/js_of_ocaml
upstream=${JSOO_UPSTREAM_URL:-https://github.com/ocsigen/js_of_ocaml.git}

rev=${1:?usage: $0 <upstream-rev>}

cd "$(git rev-parse --show-toplevel)"
git diff-index --quiet HEAD -- \
  || { echo "error: working tree is not clean" >&2; exit 1; }

git fetch "$upstream" "$rev"
rev=$(git rev-parse FETCH_HEAD)

# The current base: the newest squash commit, which records the upstream
# revision it imported in its message.
old_sq=$(git log --max-count=1 --format=%H --grep="^git-subtree-dir: $prefix\$")
old_rev=$(git show --no-patch --format=%B "$old_sq" | sed -n 's/^git-subtree-split: //p')

if [ "$rev" = "$old_rev" ]; then
  echo "Already based on $rev."
  exit 0
fi

# Squash commit for the new upstream revision. Its parent is the previous
# squash commit, so the merge below three-way-merges old-upstream ->
# new-upstream against our patched subtree: upstream changes come in, our
# patches are preserved, and patches that landed upstream melt away.
sq=$(git commit-tree "$rev^{tree}" -p "$old_sq" -m "Squashed '$prefix/' changes from $old_rev..$rev

git-subtree-dir: $prefix
git-subtree-split: $rev")

if git merge --no-ff -Xsubtree="$prefix" \
     --message "Update '$prefix/' subtree to upstream ${rev:0:12}" "$sq"; then
  echo
  echo "Done. Review with: ./$prefix/scripts/diff-upstream.sh"
else
  echo
  echo "Resolve the conflicts (usually: take the upstream side when the patch"
  echo "landed upstream), then finish with: git commit"
  exit 1
fi
