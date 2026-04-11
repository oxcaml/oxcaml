#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  echo "usage: $0 OUTPUT_JS [CMI_DIR]" >&2
  exit 1
fi

output_js=$1
cmi_dir=${2:-"$repo_root/_build/web_ocamllib"}

case "$output_js" in
  /*) ;;
  *) output_js="$PWD/$output_js" ;;
esac

case "$cmi_dir" in
  /*) ;;
  *) cmi_dir="$repo_root/$cmi_dir" ;;
esac

js_of_ocaml_bin=${JS_OF_OCAML_BIN:-js_of_ocaml}

if ! command -v "$js_of_ocaml_bin" >/dev/null 2>&1; then
  echo "js_of_ocaml not found in PATH" >&2
  exit 1
fi

if [ ! -d "$cmi_dir" ]; then
  echo "CMI directory not found: $cmi_dir" >&2
  exit 1
fi

tmp_args=$(mktemp)
trap 'rm -f "$tmp_args"' EXIT

find "$cmi_dir" -maxdepth 1 -type f -name '*.cmi' | sort | while read -r file; do
  base=$(basename "$file")
  printf '%s:/static/cmis/%s\n' "$file" "$base"
done > "$tmp_args"

if [ ! -s "$tmp_args" ]; then
  echo "no .cmi files found in $cmi_dir" >&2
  exit 1
fi

mkdir -p "$(dirname "$output_js")"
"$js_of_ocaml_bin" build-fs -o "$output_js" $(cat "$tmp_args")
