# Hacking within the Oxcaml repo

In the Oxcaml repo, ppxlib is brought in as a git subtree.

## Creating a ppxlib upgrade PR

In order to upgrade to the latest ppxlib, you should do the following:

1. (one time) `git remote add ocaml-ppx-ppxlib https://github.com/ocaml-ppx/ppxlib.git`
2. `git fetch ocaml-ppx-ppxlib`
3. `export PPXLIB_UPSTREAM_REV="$(git rev-parse ocaml-ppx-ppxlib/main)"`
4. `git branch "$USER.upgrade-ppxlib.$PPXLIB_UPSTREAM_REV"`
5. Checkout the branch you just created (via a workspace or `git checkout "$USER.upgrade-jsoo.$PPXLIB_UPSTREAM_REV"`)
6. Revert downstream commits that are now present or superseded upstream (this will limit the potential merge conflicts during the next step)
7. `git subtree merge --prefix=external/ppxlib ocaml-ppx-ppxlib $PPXLIB_UPSTREAM_REV`
8. `git push -u origin HEAD`
9. Open a pull request and **use a merge commit** to merge it

## Listing commits differing from upstream

This may be useful for upstream maintainers to see the changes done in this repository:

1. `git fetch ocaml-ppx-ppxlib`
2. `export PPXLIB_UPSTREAM_REV="$(git rev-parse ocaml-ppx-ppxlib/main)"`
3. `git cherry -v "$PPXLIB_UPSTREAM_REV" "$(git subtree split --ignore-joins --prefix=external/ppxlib)"`
