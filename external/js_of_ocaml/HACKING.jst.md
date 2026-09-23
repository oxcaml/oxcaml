# Working on JSOO in OxCaml

This is a git subtree of <https://github.com/ocsigen/js_of_ocaml>, with
downstream patches.

## Build

From a configured OxCaml checkout:

```sh
nix develop --command make jsoo-build
```

This builds the `@jsoo-libs` alias of `external/ast-dependent-libs/dune`,
with that directory as the dune root: the js_of_ocaml and wasm_of_ocaml
compilers, PPX and libraries, together with ppxlib and every other
dependency, all compiled with OxCaml. Find the executables under
`_build/ast-dependent-libs/default/js_of_ocaml/` and the installed layout
under `_build/ast-dependent-libs/install/default/`. Dependencies that are not
part of this repository (sedlex, cmdliner, menhirLib, yojson, ...) are
provided by Nix and symlinked into `external/ast-dependent-libs/deps/`; Nix
also supplies the Menhir, Node.js and Binaryen tools. `make ppxlib-build`
builds only the ppxlib stack.

Make refreshes the local `_install`. Set `OXCAML_INSTALL` to use an existing
installation without modifying it. Its `bin` and `lib/ocaml` select the
compiler, and an empty findlib configuration keeps host packages out. The Nix
`jsoo` check builds the same targets but intentionally installs nothing.

## Test

```sh
nix develop --command make jsoo-test
```

This runs the core compiler/property tests, PPX harness tests, and library
and runtime regressions for JS and Wasm, using the upstream CPS profile
(the `@jsoo-test` alias in `external/ast-dependent-libs/dune`). Test builds live in
`_build/jsoo-test`; test dependencies are also built from source with
OxCaml. Failures are not automatically promoted.

Optional-package integrations, browsers, native Wasm effects and C/Wasm
runtime regeneration are outside this target. Run compiler regressions
separately with `nix develop --command make test`.

## Upgrade upstream in a clean PR

1. Start an upgrade branch from up-to-date `main`, with a clean working tree.
2. Once, add the remote:
   `git remote add ocsigen-js_of_ocaml https://github.com/ocsigen/js_of_ocaml.git`.
3. `git fetch ocsigen-js_of_ocaml`, then select a revision:
   `export JSOO_UPSTREAM_REV="$(git rev-parse ocsigen-js_of_ocaml/master)"`.
4. Compare the committed subtree patches against the selected upstream revision:

   ```sh
   git cherry -v "$JSOO_UPSTREAM_REV" \
     "$(git subtree split --ignore-joins --prefix=external/js_of_ocaml)"
   ```

   `-` marks an equivalent upstream patch; `+` means none was found.
   The hashes belong to the synthesized subtree history. Revert the original
   downstream commits whose changes are now present or superseded upstream.
5. `git subtree merge --prefix=external/js_of_ocaml "$JSOO_UPSTREAM_REV"`.
6. Resolve conflicts, build and test, and review the diff against `main`.
   Keep the import merge separate from remaining compatibility fixes and
   preserve patch attribution.
7. Merge the PR **with a merge commit**, never squash or rebase: future subtree
   updates need the imported history and subtree metadata.
