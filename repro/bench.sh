#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repro_dir="$repo_root/repro"

boot_ocamlc="$repo_root/_build/_bootinstall/bin/ocamlc"
boot_ocamlopt="$repo_root/_build/_bootinstall/bin/ocamlopt"
runtime_stdlib="$repo_root/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"

if [[ ! -x "$boot_ocamlc" ]]; then
  echo "Missing $boot_ocamlc; run: make boot-compiler" >&2
  exit 1
fi

if [[ ! -d "$runtime_stdlib" ]]; then
  echo "Missing $runtime_stdlib; run: make runtime-stdlib" >&2
  exit 1
fi

benchdir="$(mktemp -d)"
cleanup() { rm -rf "$benchdir"; }
trap cleanup EXIT

cp "$repro_dir/foo.ml" "$benchdir/"
cp "$repro_dir/bar.ml" "$benchdir/"
cp "$repro_dir/foobar.ml" "$benchdir/"

prepare_cmd='cd "$BENCHDIR" && rm -f -- *.cmi *.cmo *.cmx *.o *.annot 2>/dev/null || true'

ocamlc_split_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" -c foo.ml &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" -c bar.ml'

ocamlc_combined_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" -c foobar.ml'

ocamlopt_split_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" -c foo.ml &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" -c bar.ml'

ocamlopt_combined_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" -c foobar.ml'

echo "benchdir=$benchdir"
echo "boot_ocamlc=$boot_ocamlc"
echo "boot_ocamlopt=$boot_ocamlopt"
echo "OCAMLLIB=$runtime_stdlib"

if command -v hyperfine >/dev/null 2>&1; then
  export BENCHDIR="$benchdir"
  export BOOT_OCAMLC="$boot_ocamlc"
  export BOOT_OCAMLOPT="$boot_ocamlopt"
  export OCAMLLIB_DIR="$runtime_stdlib"

  echo
  echo "== hyperfine: ocamlc (bytecode) =="
  hyperfine --warmup 3 --runs 30 \
    --prepare "$prepare_cmd" \
    "$ocamlc_split_cmd" \
    "$ocamlc_combined_cmd"

  echo
  echo "== hyperfine: ocamlopt (native) =="
  hyperfine --warmup 2 --runs 15 \
    --prepare "$prepare_cmd" \
    "$ocamlopt_split_cmd" \
    "$ocamlopt_combined_cmd"
else
  echo "hyperfine not found; falling back to /usr/bin/time" >&2
  (
    set +e
    cd "$benchdir"
    rm -f -- *.cmi *.cmo *.cmx *.o *.annot 2>/dev/null || true
    /usr/bin/time -p env OCAMLLIB="$runtime_stdlib" "$boot_ocamlc" -c foobar.ml
    rm -f -- *.cmi *.cmo *.cmx *.o *.annot 2>/dev/null || true
    /usr/bin/time -p sh -c \
      "env OCAMLLIB='$runtime_stdlib' '$boot_ocamlc' -c foo.ml && \
       env OCAMLLIB='$runtime_stdlib' '$boot_ocamlc' -c bar.ml"
  )
fi

