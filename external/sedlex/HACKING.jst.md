# Hacking within the Oxcaml repo

In the Oxcaml repo, sedlex is brought in as a git subtree.

## Creating a sedlex upgrade PR

In order to upgrade to the latest sedlex, you should do the following:

1. (one time) `git remote add ocaml-community-sedlex https://github.com/ocaml-community/sedlex.git`
2. `git fetch ocaml-community-sedlex`
3. `export SEDLEX_UPSTREAM_REV="$(git rev-parse ocaml-community-sedlex/master)"`
4. `git branch "$USER.upgrade-sedlex.$SEDLEX_UPSTREAM_REV"`
5. Checkout the branch you just created (via a workspace or `git checkout "$USER.upgrade-sedlex.$SEDLEX_UPSTREAM_REV"`)
6. Revert downstream commits that are now present or superseded upstream (this will limit the potential merge conflicts during the next step)
7. `git subtree merge --prefix=external/sedlex ocaml-community-sedlex $SEDLEX_UPSTREAM_REV`
8. `git push -u origin HEAD`
9. Open a pull request and **use a merge commit** to merge it

## Listing commits differing from upstream

This may be useful for upstream maintainers to see the changes done in this repository:

1. `git fetch ocaml-community-sedlex`
2. `export SEDLEX_UPSTREAM_REV="$(git rev-parse ocaml-community-sedlex/master)"`
3. `git cherry -v "$SEDLEX_UPSTREAM_REV" "$(git subtree split --ignore-joins --prefix=external/sedlex)"`
