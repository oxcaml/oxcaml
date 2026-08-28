# Hacking within the Oxcaml repo

In the Oxcaml repo, js_of_ocaml is brought in as a git subtree.

## Creating a JSOO upgrade PR

In order to upgrade to the latest JSOO, you should do the following:

1. (one time) `git remote add ocsigen-js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git`
2. `git fetch ocsigen-js_of_ocaml`
3. `export JSOO_UPSTREAM_REV="$(git rev-parse ocsigen-js_of_ocaml/master)"`
4. `git branch "$USER.upgrade-jsoo.$JSOO_UPSTREAM_REV"`
5. Checkout the branch you just created (via a workspace or `git checkout "$USER.upgrade-jsoo.$JSOO_UPSTREAM_REV"`)
6. Revert downstream commits that are now present or superseded upstream (this will limit the potential merge conflicts during the next step)
7. `git subtree merge --prefix=external/js_of_ocaml ocsigen-js_of_ocaml $JSOO_UPSTREAM_REV`
8. `git push -u origin HEAD`
9. Open a pull request and **use a merge commit** to merge it

## Listing commits differing from upstream

This may be useful for upstream maintainers to see the changes done in this repository:

1. `git fetch ocsigen-js_of_ocaml`
2. `export JSOO_UPSTREAM_REV="$(git rev-parse ocsigen-js_of_ocaml/master)"`
3. `git cherry -v "$JSOO_UPSTREAM_REV" "$(git subtree split --ignore-joins --prefix=external/js_of_ocaml)"`

The output should look like this:

```
+ 4d518bb480d4c3a67dabe1b20c24b07303875060 js_of_ocaml: remove the need for dune tuareg files
+ 5efb29d2c0373d929e742907fdfd0591446f6c66 js_of_ocaml: explicitely don't ignore committed .map files
...
```
