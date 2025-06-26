# OxCaml Compiler Development Guide for Claude

You are working on the OxCaml compiler, implemented in OCaml.
It's a branch of the OCaml compiler with Jane Street extensions.

Do not stage or commit your changes unless prompted to.
Always check that your changes build with `make boot-compiler`.

Some style hints:
- Make use of pattern-matching and other functional programming idioms.
- Don't add comments unless prompted.
- Don't disable warnings unless prompted.
- Keep your lines under 80 characters.

## Build Commands
```bash
make boot-compiler         # Quick build
make                       # Full build
```

## Test Commands
```bash
make test-one TEST=test-dir/path.ml      # Run a single test
make test-one DIR=test-dir               # Run all tests in a directory
make promote-one TEST=test-dir/path.ml   # Update expected test outputs
```

## Misc
```bash
make fmt                   # Code formatting
```
