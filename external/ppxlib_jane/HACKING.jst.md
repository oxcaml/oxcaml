# Hacking within the Oxcaml repo

In the Oxcaml repo, ppxlib_jane is brought in as a git subtree, tracking the
`oxcaml` branch of the upstream repository.

## Creating a ppxlib_jane upgrade PR

In order to upgrade to the latest ppxlib_jane, you should do the following:

1. (one time) `git remote add janestreet-ppxlib_jane https://github.com/janestreet/ppxlib_jane.git`
2. `git fetch janestreet-ppxlib_jane`
3. `export PPXLIB_JANE_UPSTREAM_REV="$(git rev-parse janestreet-ppxlib_jane/oxcaml)"`
4. `git branch "$USER.upgrade-ppxlib-jane.$PPXLIB_JANE_UPSTREAM_REV"`
5. Checkout the branch you just created (via a workspace or `git checkout "$USER.upgrade-ppxlib-jane.$PPXLIB_JANE_UPSTREAM_REV"`)
6. Revert downstream commits that are now present or superseded upstream (this will limit the potential merge conflicts during the next step)
7. `git subtree merge --prefix=external/ppxlib_jane janestreet-ppxlib_jane $PPXLIB_JANE_UPSTREAM_REV`
8. `git push -u origin HEAD`
9. Open a pull request and **use a merge commit** to merge it

## Listing commits differing from upstream

This may be useful for upstream maintainers to see the changes done in this repository:

1. `git fetch janestreet-ppxlib_jane`
2. `export PPXLIB_JANE_UPSTREAM_REV="$(git rev-parse janestreet-ppxlib_jane/oxcaml)"`
3. `git cherry -v "$PPXLIB_JANE_UPSTREAM_REV" "$(git subtree split --ignore-joins --prefix=external/ppxlib_jane)"`
