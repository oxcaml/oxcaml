#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
output_js=${1:-"$repo_root/_build/default/experiments/web_bytecode/stdlib.cmis.js"}
cmi_dir=${2:-"$repo_root/_build/web_ocamllib"}

mkdir -p "$(dirname "$output_js")"
"$repo_root/experiments/web_bytecode/build_cmi_fs.sh" "$output_js" "$cmi_dir"

printf '%s\n' "$output_js"
