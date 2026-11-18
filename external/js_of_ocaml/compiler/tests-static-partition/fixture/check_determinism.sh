# Split the same inputs twice, from different directories, and compare the outputs.
set -e
js_of_ocaml=$(realpath "$1")
wasm_of_ocaml=$(realpath "$2")
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
mkdir "$scratch/inputs"
cp shared.cma numeric_buckets.cmo main.cmo lazy.cma runtime_stubs.js runtime_stubs.wat "$scratch/inputs"
inputs="../inputs/shared.cma ../inputs/numeric_buckets.cmo ../inputs/main.cmo ../inputs/lazy.cma"
for dir in run-a run-b; do
  mkdir "$scratch/$dir"
  (cd "$scratch/$dir" &&
     "$js_of_ocaml" split $inputs ../inputs/runtime_stubs.js --main Main --root Lazy_root -o modules &&
     "$js_of_ocaml" split --strategy late $inputs ../inputs/runtime_stubs.js --main Main --root Lazy_root -o late &&
     "$wasm_of_ocaml" split $inputs ../inputs/runtime_stubs.js ../inputs/runtime_stubs.wat --main Main --root Lazy_root -o wasm &&
     "$wasm_of_ocaml" split --strategy late $inputs ../inputs/runtime_stubs.js ../inputs/runtime_stubs.wat --main Main --root Lazy_root -o late-wasm)
done
diff -r "$scratch/run-a" "$scratch/run-b"
echo "determinism: ok"
