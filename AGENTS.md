# OxCaml agent guide

Do not stage or commit changes unless asked.

Use the development commands below while editing. They keep a boot-compiler
watcher in the background, wait for it before using its output, and report
compiler errors before running a test or compiler command.

## Initial setup

A fresh worktree needs one complete build and test installation:

```sh
autoconf
./configure --prefix="$PWD/_install"
make install
```

On the measured Apple Silicon machine, configuration took about 15 seconds and
the first optimized install took about 4.5 minutes.

## Development loop

Start speculative compilation before editing:

```sh
make dev
```

`make dev` returns immediately. Starting it is optional because every command
below starts or restarts it automatically. The watcher is local to this
worktree and exits after 30 idle minutes.

Run one test or test directory:

```sh
make dev-test TEST=typing-local/regression_class_type.ml
make dev-test DIR=lib-bool
```

Promote a focused test:

```sh
make dev-promote TEST=typing-local/regression_class_type.ml
```

Compile files with the watched compiler:

```sh
make dev-ocamlc ARGS='-c /tmp/probe.ml -o /tmp/probe.cmo'
make dev-ocamlopt ARGS='/tmp/probe.ml -o /tmp/probe.exe'
```

`ARGS` has the same contents as an `ocamlc` or `ocamlopt` command line. Quote
it so Make and the shell preserve spaces. These targets select the watched
compiler and its matching development standard library.

The normal loop is therefore:

```text
make dev
edit
make dev-test TEST=...
edit
make dev-test TEST=...
```

The watcher begins compiling on each save. `dev-test`, `dev-promote`,
`dev-ocamlc`, and `dev-ocamlopt` wait for the latest saved source state. They
do not run when that build has a type error.

Existing and newly added tests are read from the live `testsuite/tests` tree.
No `_runtest` refresh is needed. Standard-library and runtime edits are also
detected; the next development command refreshes those components before
continuing.

Expect tests embed compiler libraries in their runner. `dev-test` detects
`expect` and `expectnat` actions and refreshes a stale runner automatically.
The watcher uses an isolated Dune build directory, so it does not invalidate
the main Dune context. A measured runner refresh after a compiler edit took
about 38 seconds.

The development test root intentionally has no `ocamlopt.byte`. Use the normal
final-compiler test path for a test that explicitly requires the
bytecode-hosted native compiler.

Useful recovery commands are:

```sh
make dev-status
make dev-log
make dev-stop
```

Do not run another Dune or ordinary Make build while the watcher is active.
Use `make dev-stop` first. The development commands coordinate with the
watcher themselves.

## Final validation

The development compiler is an unoptimized boot compiler. It is intended for
fast typechecking and functional tests, not final validation.

Use the final compiler when testing self-hosting, bytecode-only compiler
variants, release behavior, compiler performance, benchmarks, or memtraces:

```sh
make dev-stop
make install
make test-one TEST=test-dir/path.ml
```

Use targeted tests while iterating. Run `make test` only for broad changes or
when asked. On the measured machine, a small warm `dev-test` took about one
second, while the complete test suite took about 6 minutes 43 seconds.

Always benchmark or memtrace with the compiler produced by `make install`,
never with the boot compiler. Configure without `--enable-dev` for performance
measurements.

Run `make fmt` before committing when the changed files are covered by the
formatter. Do not disable warnings or tests unless asked.

## Repository map

- `middle_end/flambda2/`: Flambda 2 optimizer
- `backend/cfg/`: CFG backend
- `driver/`: compiler drivers and OxCaml command-line handling
- `jane/`: Jane Street extensions and documentation
- `testsuite/tests/`: upstream OCaml tests
- `oxcaml/tests/`: OxCaml-specific tests

Files ending in `.in` require configuration after they change. Keep lines
under 80 characters and prefer the existing functional OCaml style.
