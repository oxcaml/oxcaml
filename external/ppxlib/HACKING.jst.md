# Hacking within the Oxcaml repo

In the Oxcaml repo, ppxlib is brought in as a git subtree.

## Creating a ppxlib upgrade PR

In order to upgrade to the latest ppxlib, you should do the following:

1. (one time) `git remote add ocaml-ppx-ppxlib https://github.com/ocaml-ppx/ppxlib.git`
2. `git fetch ocaml-ppx-ppxlib`
3. `export PPXLIB_UPSTREAM_REV="$(git rev-parse ocaml-ppx-ppxlib/main)"`
4. `git branch "$USER.upgrade-ppxlib.$PPXLIB_UPSTREAM_REV"`
5. Checkout the branch you just created (via a workspace or `git checkout "$USER.upgrade-ppxlib.$PPXLIB_UPSTREAM_REV"`)
6. Revert downstream commits that are now present or superseded upstream (this will limit the potential merge conflicts during the next step)
7. `git subtree merge --prefix=external/ppxlib ocaml-ppx-ppxlib $PPXLIB_UPSTREAM_REV`
8. `git push -u origin HEAD`
9. Open a pull request and **use a merge commit** to merge it

## Listing commits differing from upstream

This may be useful for upstream maintainers to see the changes done in this repository:

1. `git fetch ocaml-ppx-ppxlib`
2. `export PPXLIB_UPSTREAM_REV="$(git rev-parse ocaml-ppx-ppxlib/main)"`
3. `git cherry -v "$PPXLIB_UPSTREAM_REV" "$(git subtree split --ignore-joins --prefix=external/ppxlib)"`

## Private dependencies

ppx_derivers is private to this project when it is built with
`make ppxlib-build` or `make jsoo-build`: the top-level Makefile links its
nix-provided sources into `oxcaml-private/`, and default.nix makes it the
wrapped library `oxcaml_private_ppx_derivers`, installed as
`ppxlib.private.ppx_derivers`. `src` sees the usual module name through
`-open Oxcaml_private_ppx_derivers`. See `external/js_of_ocaml/HACKING.jst.md`.

sexp_type is shared on purpose, as its own public `sexp_type` package: its
type appears in ppxlib's interface (`Stdppx.Sexp.t`), and has to be the same
as the one users' sexplib0 builds on.
