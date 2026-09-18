# Local Opam Repository

This is a local Opam repository containing a patched OCaml compiler for the OxCaml CI builds.

## Purpose

The GitHub Actions CI needs OCaml 4.14.2 with a specific patch applied. Therefore, we define it as an opam package here so that the `ocaml/setup-ocaml` action can install it directly.

Additionally, if we ever need other dependencies from opam, we can add them here. and they
will be automatically cached by ocaml/setup-ocaml.
