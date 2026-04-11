#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
experiment_dir="$repo_root/experiments/web_bytecode"
build_dir="$repo_root/_build/default/experiments/web_bytecode"
release_tree_default="$(cd "$repo_root/.." && pwd)/oxcaml-5.2.0minus-25"
release_opam_root="${OXBROWSER_OPAM_ROOT:-$release_tree_default/.opam-release-root}"
release_opam_switch="${OXBROWSER_OPAM_SWITCH:-oxrelease}"
js_of_ocaml_bin="${JS_OF_OCAML_BIN:-}"
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

toolchain_prefix=(
  env
  OPAMROOT="$release_opam_root"
  OPAMSWITCH="$release_opam_switch"
  opam exec --
)

run_tool() {
  "${toolchain_prefix[@]}" "$@"
}

have_oxcaml_driver() {
  run_tool ocamlc -help 2>/dev/null | grep -q -- '-extension-universe'
}

require_dir() {
  local path=$1
  local label=$2
  if [ ! -d "$path" ]; then
    cat >&2 <<EOF
build_browser_switch.sh: missing $label:
  $path

Expected official browser build switch:
  OPAMROOT=$release_opam_root
  OPAMSWITCH=$release_opam_switch
EOF
    exit 1
  fi
}

require_package() {
  local package_name=$1
  if ! run_tool ocamlfind query "$package_name" >/dev/null 2>&1; then
    missing_packages+=("$package_name")
  fi
}

require_dir "$release_opam_root" "OPAM root"
require_dir "$release_opam_root/$release_opam_switch" "OPAM switch"

missing_packages=()
for package_name in findlib js_of_ocaml js_of_ocaml-toplevel stdlib_stable base core; do
  require_package "$package_name"
done

if [ "${#missing_packages[@]}" -gt 0 ]; then
  printf 'build_browser_switch.sh: missing required packages in %s/%s:\n' \
    "$release_opam_root" "$release_opam_switch" >&2
  printf '  %s\n' "${missing_packages[@]}" >&2
  exit 1
fi

if ! have_oxcaml_driver; then
  cat >&2 <<EOF
build_browser_switch.sh: refusing to build with a non-OxCaml compiler.

Current toolchain:
  OPAMROOT=$release_opam_root
  OPAMSWITCH=$release_opam_switch

This browser experiment must be compiled with an ocamlc that exposes
-extension / -extension-universe. Otherwise syntax such as stack_, local_,
and @ local will silently stop being OxCaml.
EOF
  exit 1
fi

if [ -z "$js_of_ocaml_bin" ]; then
  js_of_ocaml_bin=$(run_tool which js_of_ocaml 2>/dev/null || true)
fi

if [ -z "$js_of_ocaml_bin" ]; then
  cat >&2 <<EOF
build_browser_switch.sh: could not find js_of_ocaml in:
  OPAMROOT=$release_opam_root
  OPAMSWITCH=$release_opam_switch

Install js_of_ocaml in the official release switch or set JS_OF_OCAML_BIN explicitly.
EOF
  exit 1
fi

mkdir -p "$build_dir" "$tmpdir/cmis"

js_packages="compiler-libs.common,compiler-libs.bytecomp,compiler-libs.toplevel,findlib,findlib.top,js_of_ocaml,js_of_ocaml-toplevel,stdlib_stable,base,core"
install_lib_root=$(run_tool ocamlc -where)
package_lib_root=$(dirname "$(run_tool ocamlfind query base)")
js_of_ocaml_compiler_dir="$(run_tool ocamlfind query js_of_ocaml-compiler)"

copy_cmis_from_dir() {
  local dir=$1
  find "$dir" -maxdepth 1 -type f -name '*.cmi' -exec cp -f {} "$tmpdir/cmis/" \;
}

copy_cmis_from_dir "$install_lib_root"
compilerlibs_dir="$(run_tool ocamlfind query compiler-libs 2>/dev/null || true)"
if [ -n "$compilerlibs_dir" ] && [ -d "$compilerlibs_dir" ]; then
  copy_cmis_from_dir "$compilerlibs_dir"
fi

run_tool ocamlfind ocamlc -g -package findlib -linkpkg \
  "$experiment_dir/generate_browser_package_manifest.ml" \
  -o "$tmpdir/generate_browser_package_manifest.byte"

run_tool ocamlrun "$tmpdir/generate_browser_package_manifest.byte" \
  "$install_lib_root" \
  "$package_lib_root" \
  "$tmpdir/browser_switch_package_manifest.ml" \
  "$tmpdir/browser_packages.map" \
  "$tmpdir/browser_package_runtimes.list"

compile() {
  local source=$1
  local output=$2
  shift 2
  run_tool ocamlfind ocamlc -g -package "$js_packages" "$@" -c "$source" -o "$output"
}

compile "$tmpdir/browser_switch_package_manifest.ml" \
  "$tmpdir/browser_switch_package_manifest.cmo"

compile "$experiment_dir/browser_switch_common.ml" \
  "$tmpdir/browser_switch_common.cmo" \
  -I "$tmpdir"

compile "$experiment_dir/browser_switch_check.ml" \
  "$tmpdir/browser_switch_check.cmo" \
  -I "$tmpdir"

compile "$experiment_dir/browser_switch_run.ml" \
  "$tmpdir/browser_switch_run.cmo" \
  -I "$tmpdir"

compile "$experiment_dir/browser_switch_js.ml" \
  "$tmpdir/browser_switch_js.cmo" \
  -I "$tmpdir"

run_tool ocamlfind ocamlc -g -no-check-prims -linkall -package "$js_packages" -linkpkg \
  "$tmpdir/browser_switch_package_manifest.cmo" \
  "$tmpdir/browser_switch_common.cmo" \
  "$tmpdir/browser_switch_check.cmo" \
  "$tmpdir/browser_switch_run.cmo" \
  "$tmpdir/browser_switch_js.cmo" \
  -o "$build_dir/web_bytecode_js.bc"

standard_runtime_js_files=()
while IFS= read -r runtime_file; do
  case "$runtime_file" in
    */graphics.js|*/runtime_events.js) ;;
    *) standard_runtime_js_files+=("$runtime_file") ;;
  esac
done < <(find "$js_of_ocaml_compiler_dir" -maxdepth 1 -type f -name '*.js' | sort)

browser_runtime_files=()
if [ -s "$tmpdir/browser_package_runtimes.list" ]; then
  while IFS= read -r runtime_file; do
    browser_runtime_files+=("$runtime_file")
  done < "$tmpdir/browser_package_runtimes.list"
fi

js_of_ocaml_args=(
  --effects=cps
  --toplevel
  --no-cmis
  --no-runtime
  --linkall
  --enable=with-js-error
  --disable
  genprim
)

js_of_ocaml_args+=(
  "${standard_runtime_js_files[@]}"
  "${browser_runtime_files[@]}"
  "$build_dir/web_bytecode_js.bc"
  -o
  "$build_dir/web_bytecode_js.bc.js"
)

run_tool "$js_of_ocaml_bin" "${js_of_ocaml_args[@]}"

rm -f "$build_dir/browser_fs.js" "$build_dir/browser_fs_manifest.json"
rm -rf "$build_dir/browser_fs"

find "$tmpdir/cmis" -maxdepth 1 -type f -name '*.cmi' | sort | while read -r file; do
  base=$(basename "$file")
  printf '%s:/static/cmis/%s\n' "$file" "$base"
done > "$tmpdir/browser_fs.map"
cat "$tmpdir/browser_packages.map" >> "$tmpdir/browser_fs.map"

python3 - "$tmpdir/browser_fs.map" "$build_dir" <<'PY'
import gzip
import json
import os
import shutil
import sys

map_path, build_dir = sys.argv[1:]
asset_root = os.path.join(build_dir, "browser_fs")
manifest_path = os.path.join(build_dir, "browser_fs_manifest.json")
os.makedirs(asset_root, exist_ok=True)
entries = []

with open(map_path) as map_file:
    for raw_line in map_file:
        line = raw_line.rstrip("\n")
        if not line:
            continue
        src, fs_path = line.split(":", 1)
        rel = fs_path.lstrip("/")
        asset_rel = f"browser_fs/{rel}.gz"
        asset_path = os.path.join(build_dir, asset_rel)
        os.makedirs(os.path.dirname(asset_path), exist_ok=True)
        with open(src, "rb") as src_file, open(asset_path, "wb") as dst_file:
            with gzip.GzipFile(
                filename="",
                mode="wb",
                fileobj=dst_file,
                compresslevel=6,
                mtime=0,
            ) as gz_file:
                shutil.copyfileobj(src_file, gz_file)
        entries.append(
            {
                "fs_path": fs_path,
                "asset_path": asset_rel,
                "compression": "gzip",
            }
        )

with open(manifest_path, "w") as manifest_file:
    json.dump(entries, manifest_file, indent=2)
    manifest_file.write("\n")
PY
