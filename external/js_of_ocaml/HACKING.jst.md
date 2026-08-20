The OxCaml repo vendors js_of_ocaml as a git subtree at `external/js_of_ocaml/`.

# Layout

* The base of the subtree is a squashed import of upstream js_of_ocaml. Each
  import is a *squash commit* (a commit whose tree is the upstream repo and
  whose message records the imported revision as `git-subtree-dir:
  external/js_of_ocaml` / `git-subtree-split: <rev>`) brought into main by a
  merge commit. The subtree carries the pristine upstream tree: nothing is
  removed, even files unused here (examples, manual, upstream test
  fixtures, ...).

* On top of the base live the OxCaml-specific patches. They are ordinary
  commits in main history that touch `external/js_of_ocaml/` (by convention
  their subjects start with `js_of_ocaml:` or `wasm_of_ocaml:`), interleaved
  with the rest of main like any other work. Successive upstream imports do
  not rewrite them: the update merge carries their content forward.

# Checking how we diverge from upstream

```
./external/js_of_ocaml/scripts/diff-upstream.sh [--full]
```

Prints:

* the upstream revision the subtree is currently based on;
* every patch commit ever applied on top of the subtree, over the whole
  history of the repo (`git log --no-merges -- external/js_of_ocaml` —
  squash commits and update merges are merges, so only real patches show);
* the *current* net divergence from the upstream base, as a diffstat
  (`--full` for the whole diff). This is the source of truth for "what are
  we carrying right now": patches that have landed upstream disappear from
  this diff at the next update, and conflict resolutions made inside update
  merges show up here even though they don't appear in the commit list.

# Upgrading to a new upstream revision

On a fresh branch off main, with a clean tree:

```
./external/js_of_ocaml/scripts/update-js_of_ocaml.sh <upstream-rev>
```

The script fetches the revision, creates the new squash commit (parented on
the previous one, so git knows the old upstream tree is the merge base), and
merges it with `git merge -Xsubtree=external/js_of_ocaml`. The three-way
merge brings in the upstream changes while preserving the patches applied on
top of the subtree — nothing is replayed and no history is rewritten.

If the merge stops on conflicts, a patch overlaps an upstream change. Most of
the time the patch (or an equivalent) has landed upstream: take the upstream
side, which retires the patch. Otherwise reconcile the patch with the new
upstream code. Then `git commit`.

Validate with `diff-upstream.sh` (the divergence should only shrink or stay
put, never grow with things you don't recognize), build, test, and open a PR
from the branch.

# Branches and PRs

Everything goes through the normal PR flow, with two rules for branches that
contain a subtree squash/merge (i.e. update branches):

* Merge those PRs with a real merge commit ("Create a merge commit"), never
  "Squash and merge" or "Rebase and merge": flattening the branch drops the
  squash commit and its `git-subtree-split` marker, which future updates rely
  on to find the merge base.

* If main moves while the PR is open, update the branch with `git merge
  main`; do not rebase it. Plain `git rebase` tries to replay the squash
  commit (whose tree is the js_of_ocaml *root*) onto the OxCaml repo root and
  produces hundreds of bogus add/add conflicts.

Ordinary patches to the subtree are normal commits on normal branches; the
usual PR flow, squash included, is fine for those.

