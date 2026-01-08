# OxCaml

A performance-focused version of OCaml.
This is also the home of the Flambda 2 optimiser and the Cfg backend.

OxCaml is currently based on OCaml 5.2.0 (plus some patches from later
upstream revisions, mainly in the runtime).  It supports both the OCaml 4 and OCaml 5
runtime systems, although support for the OCaml 4 runtime is expected to be removed
in autumn 2025.

The following gives basic instructions for getting set up.  Please see
[`HACKING.md`](HACKING.md) for more detailed instructions if you want to develop in this repo.
That file also contains instructions for installing the OxCaml compiler in a way
that it can be used to build OPAM packages.

## One-time setup for dev work or installation

The supported platforms are x86-64 and arm64 Linux; and arm64 macOS.  x86 macOS may still work.

One-time setup:
```
$ opam switch 5.4.0  # or "opam switch create 5.4.0" if you haven't got that switch already
$ eval $(opam env)
$ opam install dune.3.20.2 ocamlformat.0.28.1
$ opam pin menhir 20231231
```

You probably then want to fork the `oxcaml/oxcaml` repo to your own Github org.

## Branching and configuring

Use normal commands to make a branch from the desired upstream branch (typically `main`), e.g.:
```
$ git clone https://github.com/oxcaml/oxcaml
$ cd oxcaml
$ git checkout -b myfeature origin/main
```

The OxCaml tree has to be configured before building.  The configure script is not checked
in; you have to run `autoconf`.  For example:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir --enable-runtime5
```

## Building and installing

To build and install OxCaml, which produces a compiler installation directory whose
layout is compatible with upstream, run:
```
$ make install
```
