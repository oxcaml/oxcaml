# Package the fixture splits, twice each for generated packages, into $3.
set -e
js_of_ocaml=$(realpath "$1")
wasm_of_ocaml=$(realpath "$2")
output=$3
fixture=../fixture
mkdir -p "$output"
"$wasm_of_ocaml" build-runtime --effects jspi --no-source-map \
  $fixture/runtime_stubs.js $fixture/runtime_stubs.wat -o "$output/runtime.wasma"
wasm_package() {
  "$wasm_of_ocaml" package "$@" $fixture/runtime_stubs.js --runtime-archive "$output/runtime.wasma" > /dev/null
}
for split in modules-wasm late-wasm; do
  wasm_package $fixture/$split --raw -o "$output/raw-$split-merged"
  wasm_package $fixture/$split --raw --separate-eager -o "$output/raw-$split-separate"
done
for repeat in a b; do
  for split in late-js multi-js; do
    mkdir "$output/$split-$repeat"
    "$js_of_ocaml" package $fixture/$split \
      --entry "$output/$split-$repeat/entry.js" -o "$output/$split-$repeat/assets"
  done
  for split in late-wasm multi-wasm; do
    mkdir "$output/$split-$repeat"
    wasm_package $fixture/$split \
      --entry "$output/$split-$repeat/entry.js" -o "$output/$split-$repeat/assets"
  done
done
# Packages are deterministic.
for split in late-js multi-js late-wasm multi-wasm; do
  diff -r "$output/$split-a" "$output/$split-b"
done
