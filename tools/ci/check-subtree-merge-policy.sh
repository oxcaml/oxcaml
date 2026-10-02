#!/usr/bin/env bash
# Enforce: a PR is squash-merged iff it does not bring in git-subtree history.
#
# Usage: check-subtree-merge-policy.sh BASE PR_HEAD [LANDING_HEAD]
#
#   BASE          tip of the target branch the PR is being merged into
#   PR_HEAD       head commit of the PR
#   LANDING_HEAD  commit that will become the new target branch tip (the merge
#                 group head). If omitted, only report what kind of PR this is.
#
# A PR "brings in subtree history" if any of its commits (BASE..PR_HEAD) does
# not descend from the root of BASE's first-parent history. Commits authored
# against oxcaml always descend from it; commits imported by `git subtree
# add/pull/merge` (with or without --squash) belong to the subtree's own
# history and do not. We additionally report `git-subtree-dir:` trailers so the
# error message can name the subtree.
#
# A PR's history is "preserved" if PR_HEAD is an ancestor of LANDING_HEAD
# (merge commit or fast-forward). Squash and rebase both rewrite it.
#
# Needs the full commit graph (but no trees or blobs), e.g.
# `git fetch --filter=tree:0`.

set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "usage: $0 BASE PR_HEAD [LANDING_HEAD]" >&2
  exit 2
fi
base=$(git rev-parse --verify "$1^{commit}")
pr_head=$(git rev-parse --verify "$2^{commit}")
landing_head=${3:+$(git rev-parse --verify "$3^{commit}")}

root=$(git rev-list --first-parent --max-parents=0 "$base")

pr_commits=$(git rev-list "$base..$pr_head" | sort)
# Not `--not "$base"`: --ancestry-path only propagates through listed commits.
native_commits=$(git rev-list --ancestry-path "$root..$pr_head" | sort)
foreign_commits=$(comm -23 <(echo "$pr_commits") <(echo "$native_commits") \
                  | sed '/^$/d')
num_foreign=$(echo -n "$foreign_commits" | grep -c . || true)

subtree_dirs=$(git log --format=%B "$base..$pr_head" \
               | sed -n 's/^git-subtree-dir: *//p' | sort -u)

if [[ $num_foreign -gt 0 || -n $subtree_dirs ]]; then
  is_subtree_pr=true
  echo "This PR brings in git-subtree history:"
  echo "  $num_foreign commit(s) not descended from $root"
  for d in $subtree_dirs; do echo "  git-subtree-dir: $d"; done
else
  is_subtree_pr=false
  echo "This PR does not bring in git-subtree history."
fi

if [[ -z $landing_head ]]; then
  if $is_subtree_pr; then
    echo "::warning title=Subtree PR::This PR brings in git-subtree history" \
         "and must not be squash-merged, so it cannot land through the" \
         "(squashing) merge queue. Ask a maintainer to land it with history" \
         "preserved."
  fi
  exit 0
fi

if git merge-base --is-ancestor "$pr_head" "$landing_head"; then
  preserved=true
  echo "The merge preserves the PR's commits."
else
  preserved=false
  echo "The merge rewrites the PR's commits (squash or rebase)."
fi

if $is_subtree_pr && ! $preserved; then
  echo "::error title=Subtree PR would be squashed::This PR brings in" \
       "git-subtree history, which would be lost by squashing it. It must be" \
       "landed outside the merge queue, by a maintainer, with history preserved."
  exit 1
elif ! $is_subtree_pr && $preserved; then
  echo "::error title=PR would not be squashed::This PR does not bring in" \
       "git-subtree history, so it must be squash-merged."
  exit 1
fi
echo "OK"
