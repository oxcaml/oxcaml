#!/usr/bin/env bash
set -euo pipefail

prefix=$(cd "$1" && pwd)
script_dir=$(cd "$(dirname "$0")" && pwd)
case "${OXCAML_EXPECT_SHIPPED_LIBRARIES:-}" in
  0|1) ;;
  *) echo 'Set OXCAML_EXPECT_SHIPPED_LIBRARIES to 0 or 1' >&2; exit 1 ;;
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
printf '%s\n' 'let () = ()' > main.ml
mkdir direct
cp main.ml direct/main.ml
ocamlfind list > findlib.list
dune installed-libraries > dune.list
awk '$2 == "(version:" { print $1 }' findlib.list \
  | LC_ALL=C sort > findlib-names
awk '$2 == "(version:" { print $1 }' dune.list | LC_ALL=C sort > dune-names
[ -s findlib-names ] || { echo 'No findlib packages found' >&2; exit 1; }
[ -s dune-names ] || { echo 'No Dune libraries found' >&2; exit 1; }

findlib_lists=("$script_dir/installed-core-libraries.txt")
dune_lists=("$script_dir/installed-core-libraries.txt")
if [ "$OXCAML_EXPECT_SHIPPED_LIBRARIES" = 1 ]; then
  findlib_lists+=("$script_dir/installed-shipped-findlib-libraries.txt")
  dune_lists+=("$script_dir/installed-shipped-dune-libraries.txt")
fi
LC_ALL=C sort "${findlib_lists[@]}" > expected-findlib-names
LC_ALL=C sort "${dune_lists[@]}" > expected-dune-names
if ! diff -u expected-findlib-names findlib-names; then
  echo 'Installed findlib library names differ from the expected inventory' >&2
  exit 1
fi
if ! diff -u expected-dune-names dune-names; then
  echo 'Installed Dune library names differ from the expected inventory' >&2
  exit 1
fi

for meta in "$prefix"/lib/*/META; do
  [ ! -f "$meta" ] || [ -f "${meta%/META}/dune-package" ] || {
    echo "Missing dune-package next to $meta" >&2
    exit 1
  }
done

while read -r name; do
  kind=$(ocamlfind query -format '%(library_kind)' "$name")
  case "$kind" in
    ppx_rewriter|ppx_deriver) predicate=ppx_driver ;;
    *) predicate= ;;
  esac
  byte_archive=
  native_archive=
  for mode in byte native; do
    predicates="$mode"
    [ -z "$predicate" ] || predicates="$predicate,$mode"
    dependencies=$(ocamlfind query -recursive -predicates "$predicates" \
      -format '%p|%d' "$name")
    while IFS='|' read -r dependency directory; do
      case "$directory" in
        "$prefix/lib"|"$prefix/lib/"*) ;;
        *)
          echo "$name ($mode): $dependency escaped prefix: $directory" >&2
          exit 1
          ;;
      esac
    done <<< "$dependencies"
    archive=$(ocamlfind query -predicates "$predicates" -format '%A' "$name")
    if [ -n "$archive" ]; then
      if [ "$mode" = byte ]; then
        byte_archive=1
      else
        native_archive=1
      fi
      if [ -z "$predicate" ]; then
        if [ "$mode" = byte ]; then
          compiler=ocamlc
          target=main.bc
        else
          compiler=ocamlopt
          target=main.exe
        fi
        link_flags=()
        # Eval requires runtime metaprogramming support at link time.
        if [ "$name" = eval ]; then
          link_flags=(-passopt -extension -passopt runtime_metaprogramming
            -passopt -uses-metaprogramming)
        fi
        if ! (cd direct && ocamlfind "$compiler" -package "$name" \
          -linkpkg -linkall "${link_flags[@]}" -o "$target" main.ml) \
          > findlib-link.log 2>&1; then
          cat findlib-link.log >&2
          echo "$name: findlib $mode link failed" >&2
          exit 1
        fi
      fi
    fi
  done

  if [ "$kind" = ppx_rewriter ]; then
    if ! (cd direct && ocamlfind ocamlc -package "$name" -c main.ml) \
      > findlib-ppx.log 2>&1; then
      cat findlib-ppx.log >&2
      echo "$name: findlib preprocessor failed" >&2
      exit 1
    fi
  fi

  if grep -Fxq "$name" dune-names; then
    if [ -n "$predicate" ]; then
      printf '%s\n' '(executable' ' (name main)' ' (modes exe byte)' \
        ' (link_flags (:standard -linkall))' \
        " (preprocess (pps $name)))" > dune
    elif [ "$name" = eval ]; then
      printf '%s\n' '(executable (name main) (modes exe)' \
        ' (libraries eval)' \
        ' (link_flags (:standard -linkall' \
        '              -extension runtime_metaprogramming' \
        '              -uses-metaprogramming)))' > dune
    else
      printf '%s\n' '(executable' ' (name main)' ' (modes exe byte)' \
        " (libraries $name)" ' (link_flags (:standard -linkall)))' \
        > dune
    fi
    targets=()
    if [ -z "$byte_archive" ] && [ -z "$native_archive" ]; then
      targets=(main.bc main.exe)
    else
      [ -z "$byte_archive" ] || targets+=(main.bc)
      [ -z "$native_archive" ] || targets+=(main.exe)
    fi
    dune build --display=quiet "${targets[@]}"
  fi
  printf 'Installed library checked: %s\n' "$name"
  rm -f direct/main.bc direct/main.exe direct/main.cmi \
    direct/main.cmo direct/main.cmx direct/main.o
done < findlib-names

if [ "$OXCAML_EXPECT_SHIPPED_LIBRARIES" = 1 ]; then
  printf '%s\n' 'let () = print_endline Jsoo_runtime.Sys.version' \
    > direct/runtime_stub.ml
  printf '%s\n' 'let () = ()' > direct/js_stub.ml
  for package in js_of_ocaml-runtime js_of_ocaml; do
    case "$package" in
      js_of_ocaml-runtime) source=runtime_stub ;;
      js_of_ocaml) source=js_stub ;;
    esac
    (cd direct && ocamlfind ocamlc -package "$package" -linkpkg \
      -o "$source.bc" "$source.ml")
    "$prefix/bin/ocamlrun" "direct/$source.bc" > /dev/null
  done
  "$prefix/bin/ocamlobjinfo" direct/js_stub.bc > js_stub.objinfo
  grep -Fq dlljs_of_ocaml_stubs js_stub.objinfo
  grep -Fq dlljsoo_runtime_stubs js_stub.objinfo
fi
