#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Compare installed library inventories and build findlib/Dune consumers.
# Usage: check-installed-libraries.sh PREFIX core|shipped

set -euo pipefail

[ -d "$1/lib/ocaml" ] || { echo "No install at $1" >&2; exit 1; }
prefix=$(cd "$1" && pwd)
script_dir=$(cd "$(dirname "$0")" && pwd)
case "${2:-}" in
  core|shipped) inventory=$2 ;;
  *) echo "Usage: $0 PREFIX core|shipped" >&2; exit 1 ;;
esac
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"

# Neither findlib nor Dune may find packages in the bootstrap environment.
cat > findlib.conf <<EOF
path="$prefix/lib:$prefix/lib/ocaml"
stdlib="$prefix/lib/ocaml"
ocamlc="$prefix/bin/ocamlc"
ocamlopt="$prefix/bin/ocamlopt"
ldconf="ignore"
EOF
export PATH="$prefix/bin:$PATH"
export OCAMLFIND_CONF="$work/findlib.conf"
export DUNE_CACHE=disabled
unset OCAMLLIB CAMLLIB CAML_LD_LIBRARY_PATH OCAMLPATH OCAMLFIND_COMMANDS \
  OCAMLFIND_TOOLCHAIN OPAM_SWITCH_PREFIX OPAMROOT

compiler_stdlib=$(ocamlc -where)
if [ "$compiler_stdlib" != "$prefix/lib/ocaml" ]; then
  echo "Installed ocamlc uses $compiler_stdlib, expected $prefix/lib/ocaml" >&2
  exit 1
fi
printf '%s\n' '(lang dune 3.23)' '(name installed_libraries_probe)' \
  > dune-project

echo 'Checking installed findlib and Dune inventories'
ocamlfind list > findlib.list
dune installed-libraries > dune.list
awk '$2 == "(version:" { print $1 }' findlib.list \
  | LC_ALL=C sort > findlib-names
awk '$2 == "(version:" { print $1 }' dune.list | LC_ALL=C sort > dune-names
lists=("$script_dir/installed-core-libraries.txt")
findlib_only=()
if [ "$inventory" = shipped ]; then
  lists+=("$script_dir/installed-shipped-libraries.txt")
  findlib_only=("$script_dir/installed-shipped-findlib-extras.txt")
fi
LC_ALL=C sort "${lists[@]}" > expected-dune-names
LC_ALL=C sort "${lists[@]}" "${findlib_only[@]}" > expected-findlib-names
diff -u expected-findlib-names findlib-names
diff -u expected-dune-names dune-names

targets=()
while read -r name; do
  direct=$(mktemp -d "$work/direct.XXXXXX")
  printf '%s\n' 'let () = ()' > "$direct/main.ml"
  kind=$(ocamlfind query -format '%(library_kind)' "$name")
  case "$kind" in
    ppx_rewriter|ppx_deriver) predicate=ppx_driver ;;
    *) predicate= ;;
  esac
  link_flags=()
  # Eval requires runtime metaprogramming support at link time.
  [ "$name" != eval ] || \
    link_flags=(-extension runtime_metaprogramming -uses-metaprogramming)
  findlib_flags=()
  for flag in "${link_flags[@]}"; do findlib_flags+=(-passopt "$flag"); done
  package_targets=()
  for mode in byte native; do
    case "$mode" in
      byte) compiler=ocamlc; target=main.bc ;;
      native) compiler=ocamlopt; target=main.exe ;;
    esac
    archive=$(ocamlfind query -predicates "${predicate:+$predicate,}$mode" \
      -format '%A' "$name")
    [ -n "$archive" ] || continue
    package_targets+=("$name/$target")
    if [ -z "$predicate" ]; then
      echo "Checking $name with findlib ($mode)"
      (cd "$direct" && ocamlfind "$compiler" -package "$name" \
        -linkpkg -linkall "${findlib_flags[@]}" -o "$target" main.ml)
    fi
  done
  if [ "$kind" = ppx_rewriter ]; then
    echo "Checking $name with findlib (preprocessor)"
    (cd "$direct" && ocamlfind ocamlc -package "$name" -c main.ml)
  fi
  if grep -Fxq "$name" dune-names; then
    mkdir "$name"
    cp "$direct/main.ml" "$name/main.ml"
    dependency="(libraries $name)"
    [ -z "$predicate" ] || dependency="(preprocess (pps $name))"
    printf '%s\n' '(executable (name main) (modes exe byte)' \
      " $dependency" \
      " (link_flags (:standard -linkall ${link_flags[*]})))" > "$name/dune"
    if [ "${#package_targets[@]}" = 0 ]; then
      package_targets=("$name/main.bc" "$name/main.exe")
    fi
    targets+=("${package_targets[@]}")
  fi
done < findlib-names

echo 'Checking installed libraries with Dune'
dune build --display=short "${targets[@]}"

smoke_bytecode() {
  local package=$1 source=$2
  echo "Checking bytecode stub loading for $package"
  printf '%s\n' "$source" > main.ml
  ocamlfind ocamlc -package "$package" -linkpkg -o "$package.bc" main.ml
  "$prefix/bin/ocamlrun" "$package.bc" > /dev/null
}

if [ "$inventory" = shipped ]; then
  smoke_bytecode js_of_ocaml-runtime \
    'let () = print_endline Jsoo_runtime.Sys.version'
  smoke_bytecode js_of_ocaml 'let () = ()'
  # Ensure these runs exercised DLL lookup rather than passing vacuously.
  "$prefix/bin/ocamlobjinfo" js_of_ocaml.bc > js_stub.objinfo
  grep -Fq dlljs_of_ocaml_stubs js_stub.objinfo
  grep -Fq dlljsoo_runtime_stubs js_stub.objinfo
fi
