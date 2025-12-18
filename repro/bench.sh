#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repro_dir="$repo_root/repro"

boot_ocamlc="${OCAMLC:-$repo_root/_build/_bootinstall/bin/ocamlc}"
boot_ocamlopt="${OCAMLOPT:-$repo_root/_build/_bootinstall/bin/ocamlopt}"
runtime_stdlib="$repo_root/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"
extra_flags="${EXTRA_FLAGS:--extension-universe stable -short-paths -opaque -no-alias-deps}"

if [[ ! -x "$boot_ocamlc" ]]; then
  echo "Missing $boot_ocamlc; run: make boot-compiler" >&2
  exit 1
fi

benchdir="$(mktemp -d)"
cleanup() { rm -rf "$benchdir"; }
trap cleanup EXIT

cp "$repro_dir/foo.ml" "$benchdir/"
cp "$repro_dir/bar.ml" "$benchdir/"
cp "$repro_dir/foobar.ml" "$benchdir/"

boot_flags=""
ocamllib_dir=""
if (
  cd "$benchdir"
  "$boot_ocamlc" $extra_flags -nopervasives -nostdlib -c foobar.ml \
    >/dev/null 2>&1
); then
  boot_flags="-nopervasives -nostdlib"
else
  if [[ ! -d "$runtime_stdlib" ]]; then
    echo "Missing $runtime_stdlib; run: make runtime-stdlib" >&2
    exit 1
  fi
  ocamllib_dir="$runtime_stdlib"
fi

prepare_cmd='cd "$BENCHDIR" && rm -f -- *.cmi *.cmo *.cmx *.o *.annot 2>/dev/null || true'

ocamlc_split_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" $EXTRA_FLAGS $BOOT_FLAGS \
    -c foo.ml &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" $EXTRA_FLAGS $BOOT_FLAGS \
    -c bar.ml'

ocamlc_combined_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLC" $EXTRA_FLAGS $BOOT_FLAGS \
    -c foobar.ml'

ocamlopt_split_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" $EXTRA_FLAGS $BOOT_FLAGS \
    -c foo.ml &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" $EXTRA_FLAGS $BOOT_FLAGS \
    -c bar.ml'

ocamlopt_combined_cmd='cd "$BENCHDIR" &&
  env OCAMLLIB="$OCAMLLIB_DIR" "$BOOT_OCAMLOPT" $EXTRA_FLAGS $BOOT_FLAGS \
    -c foobar.ml'

echo "benchdir=$benchdir"
echo "boot_ocamlc=$boot_ocamlc"
echo "boot_ocamlopt=$boot_ocamlopt"
if [[ -n "$ocamllib_dir" ]]; then
  echo "OCAMLLIB=$ocamllib_dir"
else
  echo "OCAMLLIB=(unused)"
fi
if [[ -n "$boot_flags" ]]; then
  echo "boot_flags=$boot_flags"
else
  echo "boot_flags=(none)"
fi
echo "extra_flags=$extra_flags"

if command -v hyperfine >/dev/null 2>&1; then
  export BENCHDIR="$benchdir"
  export BOOT_OCAMLC="$boot_ocamlc"
  export BOOT_OCAMLOPT="$boot_ocamlopt"
  export OCAMLLIB_DIR="$ocamllib_dir"
  export BOOT_FLAGS="$boot_flags"
  export EXTRA_FLAGS="$extra_flags"

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
    /usr/bin/time -p env OCAMLLIB="$ocamllib_dir" \
      "$boot_ocamlc" $extra_flags $boot_flags -c foobar.ml
    rm -f -- *.cmi *.cmo *.cmx *.o *.annot 2>/dev/null || true
    /usr/bin/time -p sh -c \
      "env OCAMLLIB='$ocamllib_dir' '$boot_ocamlc' $extra_flags $boot_flags \
         -c foo.ml && \
       env OCAMLLIB='$ocamllib_dir' '$boot_ocamlc' $extra_flags $boot_flags \
         -c bar.ml"
  )
fi
