#!/usr/bin/env bash
set -euo pipefail

formatter_root="$RUNNER_TEMP/ocamlformat-ox"
formatter_switch="$formatter_root/switch"
formatter_source="$formatter_root/source"

git init "$formatter_source"
git -C "$formatter_source" remote add origin \
  https://github.com/oxcaml/ocamlformat.git
git -C "$formatter_source" fetch --depth=1 origin \
  48863c1bf753474dffbca831f66024535a0ec982
git -C "$formatter_source" checkout --detach FETCH_HEAD

# The formatter fork uses the OCaml 5.2 compiler-libs API.
opam switch create "$formatter_switch" ocaml-base-compiler.5.2.1 \
  --yes --no-switch
opam install --switch="$formatter_switch" --yes \
  dune.3.23.1 menhir.20231231
opam install --switch="$formatter_switch" --yes --deps-only \
  "$formatter_source/ocamlformat.opam" \
  "$formatter_source/ocamlformat-lib.opam"
opam exec --switch="$formatter_switch" -- dune build \
  --root="$formatter_source" bin/ocamlformat/main.exe

mkdir -p "$formatter_root/bin"
install -m 755 "$formatter_source/_build/default/bin/ocamlformat/main.exe" \
  "$formatter_root/bin/ocamlformat-ox"
