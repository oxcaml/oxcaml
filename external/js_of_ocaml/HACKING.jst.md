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
`external/ast-dependent-libs/_build/default/js_of_ocaml/` and the installed
layout under `external/ast-dependent-libs/_build/install/default/`. Dependencies that are not
part of this repository (sedlex, cmdliner, menhirLib, yojson, ...) are
provided by Nix and symlinked into `external/ast-dependent-libs/deps/`; Nix
also supplies the Menhir, Node.js and Binaryen tools. `make ppxlib-build`
builds only the ppxlib stack.

Make refreshes the local `_install`. Set `OXCAML_INSTALL` to use an existing
installation without modifying it. Its `bin` and `lib/ocaml` select the
compiler, and an empty findlib configuration keeps host packages out.

`make jsoo-install` (or `ppxlib-install`) installs the packages into
`AST_DEPENDENT_LIBS_PREFIX` (default: `OXCAML_INSTALL`), as findlib packages
under `lib/` and executables under `bin/`. The Nix `jsoo` and `ppxlib`
packages are built this way.

The Nix `oxcaml` compiler package ships a subset, installed by
`make jsoo-install-shipped`: the `js_of_ocaml`, `jsoo_minify` and
`wasm_of_ocaml` executables, and the `js_of_ocaml`, `js_of_ocaml-runtime` and
`js_of_ocaml-ppx` libraries with the ppxlib stack they need. The compiler
library and its dependencies (yojson, sedlex, ...) are not shipped. The Nix
`jsoo-smoke-test` check builds and runs `external/ast-dependent-libs/smoke`
against that package.

Downstream packaging change: `lib/runtime` is its own package,
`js_of_ocaml-runtime`, instead of upstream's `js_of_ocaml-compiler.runtime`,
so that the `js_of_ocaml` library installs without the compiler library.

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
