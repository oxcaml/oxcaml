#!/usr/bin/env bash
set -euo pipefail

prefix=$(cd "$1" && pwd)
script_dir=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Findlib uses the bootstrap compiler's ABI, not the installed compiler's.
cp "$script_dir/check_installed_metadata.ml" "$work/"
cd "$work"
ocamlfind ocamlc -package findlib,unix -linkpkg check_installed_metadata.ml \
  -o check_metadata.exe
./check_metadata.exe "$prefix/lib/ocaml" meta-packages.txt \
  "$script_dir/installed-unreferenced-archives.txt"

cat > findlib.conf <<EOF
path="$prefix/lib:$prefix/lib/ocaml"
stdlib="$prefix/lib/ocaml"
ocamlopt="$prefix/bin/ocamlopt"
ldconf="ignore"
EOF
export OCAMLFIND_CONF="$work/findlib.conf"
export OCAMLLIB="$prefix/lib/ocaml"
unset CAMLLIB OCAMLPATH OCAMLFIND_COMMANDS OCAMLFIND_TOOLCHAIN

while IFS=$'\t' read -r name kind; do
  case "$kind" in
    ppx_deriver|ppx_rewriter) predicate=ppx_driver ;;
    *) predicate= ;;
  esac
  for mode in byte native; do
    predicates="$mode"
    [ -z "$predicate" ] || predicates="$predicate,$mode"
    ocamlfind query -recursive -predicates "$predicates" "$name" > /dev/null
  done
done < meta-packages.txt

cat > native_toplevel.ml <<'EOF'
let () =
  Opttoploop.initialize_toplevel_env ();
  print_endline "native-toplevel linked and ran"
EOF
ocamlfind ocamlopt -package compiler-libs.native-toplevel -linkpkg \
  native_toplevel.ml -o smoke.exe
./smoke.exe

cat > jit_link.ml <<'EOF'
let () =
  Jit.init_top ();
  print_endline "ocaml-jit linked and ran"
EOF
ocamlfind ocamlopt -package ocaml-jit -linkpkg jit_link.ml -o smoke.exe
./smoke.exe

cat > eval_link.ml <<'EOF'
let () = print_endline "eval linked and ran"
EOF
ocamlfind ocamlopt -package eval -linkpkg \
  -passopt -extension -passopt runtime_metaprogramming \
  -passopt -uses-metaprogramming eval_link.ml -o smoke.exe
./smoke.exe
