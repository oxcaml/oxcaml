# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# OxCaml Compiler Development Guide

Do not stage or commit your changes unless prompted to.
Always check that your changes build with both:
1. `make boot-compiler` - Quick build check
2. `make test` - Full test suite (required before declaring success)

You are working on the OxCaml compiler, a performance-focused fork of OCaml with Jane Street extensions, including the Flambda 2 optimizer and CFG backend.

## Key Architecture

**Directory Structure:**
- `middle_end/flambda2/` - Flambda 2 optimizer implementation
- `backend/cfg/` - Control Flow Graph backend
- `driver/` - Compiler driver, including `oxcaml_*` files for OxCaml-specific options
- `jane/` - Jane Street specific extensions and documentation
- `testsuite/tests/` - Upstream OCaml test suite
- `oxcaml/tests/` - OxCaml-specific tests

**Important Files:**
- `driver/oxcaml_flags.ml` - OxCaml compiler flags definitions
- `driver/oxcaml_args.ml` - Command-line argument handling
- Files ending in `.in` require configuration via `./configure`

## Build Commands
```bash
make boot-compiler         # Quick build (recommended for development)
make                       # Full build
make install               # Install the compiler to $(pwd)/_install
make fmt                   # Auto-format code (always run before committing)
```

## Test Commands
```bash
make test-one TEST=test-dir/path.ml      # Run a single test testsuite/tests/test-dir/path.ml
make test-one DIR=test-dir               # Run all tests in testsuite/tests/test-dir
make promote-one TEST=test-dir/path.ml   # Update expected test output
make test                                # Run all tests
```

## Configuration Commands
```bash
autoconf                  # Generate configure script
./configure               # Configure the compiler
```

Configuration is needed after changing `.in` files or the autoconf script.

## Development Guidelines
- Always verify changes build with `make boot-compiler`
- Run `make fmt` to ensure code formatting
- Keep lines under 80 characters
- Don't add excessive comments unless prompted
- Don't disable warnings or tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `assert false` and other unreachable code

## Important Notes

- NEVER create files unless absolutely necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested

