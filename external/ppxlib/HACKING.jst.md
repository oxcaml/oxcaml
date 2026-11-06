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

## Shared dependencies

Unlike js_of_ocaml's dependencies (see `external/js_of_ocaml/HACKING.jst.md`),
ppx_derivers and sexp_type are not private to this project: both are shipped
as their own public packages, built from the nix-provided sources that the
top-level Makefile links into `external/ast-dependent-libs/deps/`.

ppx_derivers is the registry through which ppxlib's `Deriving` and
ppx_deriving see each other's derivers when they are linked into the same
driver: `Ppx_derivers.deriver` is an extensible type, and each framework
ignores the derivers it doesn't own. That only works with a single copy of it,
so it has to be the one users' ppx_deriving builds on.

sexp_type's type appears in ppxlib's interface (`Stdppx.Sexp.t`), and has to
be the same as the one users' sexplib0 builds on.

## Location check documentation

`src/location_check_doc.ml` holds the optional URL that location check errors
end with. It is `None` here; a distribution can replace that file to point at
its own documentation.

## Syntax errors explained by lrgrep

When the compiler's parser rejects a source file, ppxlib parses it again with
`lrgrep/frontend.ml` and, if one of the rules of `lrgrep/parse_errors.mlyl`
matches, reports `Syntax Error. LRGrep analysis: <message>` instead of the
compiler's error.

[lrgrep](https://github.com/let-def/lrgrep) itself is a private dependency:
default.nix provides its sources, and its runtime is installed as
`ppxlib.private.lrgrep_runtime`. Its generator, and the libraries the generator
needs (menhirSdk, pprint, cmon, grenier), are only used while building and are
not installed.

`lrgrep/` is the library `oxcaml_private_lrgrep`, installed as
`ppxlib.private.lrgrep`:

- `parse_errors.mlyl` holds the rules, written for the OxCaml grammar.
  `parse_errors.ml` is generated from it and the grammar of the compiler it is
  built with, `%{ocaml_where}/compiler-libs/parser.cmly` (in the compiler's own
  build, the `parser.cmly` it generates).
- `frontend.ml` (based on upstream's `ocaml/frontend.ml`) runs the compiler's
  own `Lexer` and `Parser` in incremental mode, and returns the message of the
  first matching rule. Parse errors that no rule explains, and failures other
  than parse errors, such as lexer errors and errors raised by the parser's
  semantic actions, return exactly "Syntax error": ppxlib then reports the
  compiler's own error.

A change to `parsing/parser.mly` can make a rule of `parse_errors.mlyl`
invalid, which fails the build of ppxlib. Fix the rule in the same change. See
upstream's `DEVISING-RULES.md` for the rule language.
