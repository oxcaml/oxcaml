# OxCaml Compiler Development Guide for Claude

You are working on the OxCaml compiler, implemented in OCaml.
It's a branch of the OCaml compiler with Jane Street extensions.

Do not stage or commit your changes unless prompted to.
Always check that your changes build with `make boot-compiler`.

## Critical Implementation Guidelines
- NEVER leave TODO comments in production code. Always complete implementations.
- When implementing new features that should be used by existing code, ALWAYS:
  1. Search for where the feature should be generated/used
  2. Wire it up completely
  3. Test that it's actually being used (not just that it compiles)
- When adding new primitives or operations:
  1. Ensure they're actually generated somewhere in the compiler
  2. Verify the code path from source to backend
  3. Check that optimizations/transformations handle them
- Before saying "implementation complete", verify:
  1. No TODO comments remain
  2. The feature is actually reachable/usable
  3. All relevant code paths have been updated

Some style hints:
- Make use of pattern-matching and other functional programming idioms.
- Don't add comments unless prompted.
- Don't disable warnings unless prompted.
- Don't disable tests or add temporary workarounds unless prompted.
- Keep lines under 80 characters, unless auto-formatting is enabled according to .ocamlformat-enable.

## Build Commands
```bash
make boot-compiler         # Quick build
make                       # Full build
```

## Test Commands
```bash
make test-one TEST=test-dir/path.ml      # Run a single test testsuite/tests/test-dir/path.ml
make test-one DIR=test-dir               # Run all tests in testsuite/tests/test-dir
make promote-one TEST=test-dir/path.ml   # Update expected test output
make runtest                             # Run all tests in oxcaml/tests
```

## Configuration Commands
```bash
autoconf                  # Generate configure script, needs to be version 2.71
                          # or higher; if available, just use autoconf27 directly

./configure               # Configure the compiler
```

Configuration is needed after changing files with extension `.in` or modifying the autoconf script.
To avoid issues with dune caching, removing the `_build/` directory may be needed after configuring.
By default, unless there is a good reason to omit them, the following options should be used for configuring:
```bash
./configure --enable-ocamltest --enable-warn-error --enable-dev --enable-runtime5 --prefix="$(pwd)/_install"
```
If previously a different install directory was used, prefer the old one over `$(pwd)/_install` for `--prefix`.


## Misc
```bash
make fmt                   # Code formatting
```
