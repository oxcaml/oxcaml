#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$repo_root"

demo_js=${1:-/tmp/web_bytecode_check_demo_with_fs.js}
fs_js=$(mktemp /tmp/web_bytecode_cmis.XXXXXX.js)
trap 'rm -f "$fs_js"' EXIT

RUNTIME_DIR=runtime dune build --display=quiet -j1 \
  experiments/web_bytecode/web_bytecode_check_demo.bc

./experiments/web_bytecode/build_cmi_fs.sh "$fs_js"
js_of_ocaml _build/default/experiments/web_bytecode/web_bytecode_check_demo.bc \
  -o "${demo_js}.raw"
cat "$fs_js" "${demo_js}.raw" > "$demo_js"
rm -f "${demo_js}.raw"

node "$demo_js"
