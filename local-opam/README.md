# Local Opam Repository

This is a local opam repository containing a patched OCaml compiler for the OxCaml CI builds.

## Purpose

The GitHub Actions CI needs OCaml 4.14.2 with a specific patch applied. Rather than manually building the compiler, we define it as an opam package here so that the `ocaml/setup-ocaml` action can install it directly.

## Contents

- `packages/ocaml-base-compiler/ocaml-base-compiler.4.14.2+patched/` - Patched OCaml 4.14.2 compiler
  - Downloads OCaml 4.14.2 source from upstream
  - Applies `oxcaml-setup.patch` during build
  - Builds and installs the patched compiler
