# OxCaml agent guide

## Initial setup

A fresh worktree needs one complete build and test installation:
```sh
autoconf
./configure --prefix="$PWD/_install"
make dev
```
This takes about 5 minutes. Subsequent `make dev` invocations are much faster
because they use an incremental background watcher.

## Development loop
```sh
# edit compiler code, then typecheck/build:
make dev
# edit compiler code or a test, then build & run a test:
make dev-test TEST=typing-local/regression_class_type.ml
# edit again, test an entire dir:
make dev-test DIR=typing-local
# promote current outputs as expect goldens
make dev-promote TEST=path/to/test.ml
# compile separate files
make dev-ocamlc ARGS='-c file.ml'
make dev-ocamlopt ARGS='file.ml -o file.exe'
# run the full compiler test suite
make dev-test-all
```

## Release builds

Benchmark and memtrace only with the compiler produced by `make install`, never
the development boot compiler.
