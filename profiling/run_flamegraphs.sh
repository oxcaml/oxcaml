#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run_flamegraphs.sh [--duration SECONDS] [--skip-build]

Rebuilds the boot compiler (unless --skip-build is passed), profiles
`large_typecheck.ml` compilation under `ocamlc`, collapses the collected
samples, and regenerates all flamegraph SVG variants.

Environment overrides:
  OCAMLC           Path to the ocamlc executable to profile
  OCAMLLIB         Stdlib directory used by ocamlc during the run
  SAMPLE_BIN       Profiling sampler (default: /usr/bin/sample)
  COLLAPSE_BIN     inferno-collapse-sample executable
  FLAMEGRAPH_BIN   inferno-flamegraph executable
  TITLE_PREFIX     Prefix for generated flamegraph titles
USAGE
}

log() {
  printf '[run-flamegraphs] %s\n' "$*"
}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

DURATION=120
SKIP_BUILD=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --duration)
      shift
      [[ $# -gt 0 ]] || { echo 'Missing value for --duration' >&2; exit 1; }
      DURATION=$1
      ;;
    --skip-build)
      SKIP_BUILD=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
  shift
done

OCAMLC=${OCAMLC:-"$ROOT_DIR/_build/_bootinstall/bin/ocamlc"}
OCAMLLIB=${OCAMLLIB:-"$ROOT_DIR/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib"}
SAMPLE_BIN=${SAMPLE_BIN:-/usr/bin/sample}
COLLAPSE_BIN=${COLLAPSE_BIN:-"$HOME/.cargo/bin/inferno-collapse-sample"}
FLAMEGRAPH_BIN=${FLAMEGRAPH_BIN:-"$HOME/.cargo/bin/inferno-flamegraph"}
TITLE_PREFIX=${TITLE_PREFIX:-"oxcaml type checking flamegraph"}

REQUIRED_FILES=(
  "$SCRIPT_DIR/clean_folded.py"
  "$SCRIPT_DIR/clean_folded_inline.py"
  "$SCRIPT_DIR/generate_nameattrs.py"
  "$SCRIPT_DIR/inject_flamegraph_css.py"
  "$SCRIPT_DIR/large_typecheck.ml"
)

for path in "${REQUIRED_FILES[@]}"; do
  [[ -f $path ]] || { echo "Missing required file: $path" >&2; exit 1; }
done

[[ -x $OCAMLC ]] || { echo "Required executable not found or not executable: ocamlc ($OCAMLC)" >&2; exit 1; }
[[ -x $SAMPLE_BIN ]] || { echo "Required executable not found or not executable: sample ($SAMPLE_BIN)" >&2; exit 1; }
[[ -x $COLLAPSE_BIN ]] || { echo "Required executable not found or not executable: inferno-collapse-sample ($COLLAPSE_BIN)" >&2; exit 1; }
[[ -x $FLAMEGRAPH_BIN ]] || { echo "Required executable not found or not executable: inferno-flamegraph ($FLAMEGRAPH_BIN)" >&2; exit 1; }

[[ -d $OCAMLLIB ]] || { echo "OCAMLLIB directory not found: $OCAMLLIB" >&2; exit 1; }

if [[ $SKIP_BUILD -eq 0 ]]; then
  log "Building boot compiler"
  (cd "$ROOT_DIR" && make boot-compiler >/dev/null)
fi

cd "$SCRIPT_DIR"

ARTIFACTS=(
  ocamlc.sample
  ocamlc.folded
  ocamlc_clean.folded
  ocamlc_clean_inline.folded
  ocamlc_clean_gcside.folded
  ocamlc_leafgroup.folded
  ocamlc_nameattrs.txt
  ocamlc_nameattrs_inline.txt
  ocamlc_nameattrs_inline_full.txt
  ocamlc_nameattrs_gcside.txt
  ocamlc_nameattrs_leafgroup.txt
  ocamlc_nameattrs_ikinds.txt
  ocamlc_flamegraph_leafgroup.svg
  ocamlc_flamegraph_inline.svg
  ocamlc_flamegraph_gcside.svg
  ocamlc_flamegraph_modes.svg
  ocamlc_flamegraph_ikinds.svg
  large_typecheck.cmi
  large_typecheck.cmo
)

rm -f "${ARTIFACTS[@]}"

log "Capturing sampler data (duration=${DURATION}s)"
OCAMLLIB="$OCAMLLIB" "$OCAMLC" -c large_typecheck.ml &
OCAMLC_PID=$!

sleep 1
"$SAMPLE_BIN" "$OCAMLC_PID" "$DURATION" -mayDie -file ocamlc.sample >/tmp/run-flamegraphs-sample.log 2>&1 || true
wait "$OCAMLC_PID"

log "Collapsing samples"
"$COLLAPSE_BIN" ocamlc.sample > ocamlc.folded

log "Cleaning folded stacks"
python3 clean_folded.py < ocamlc.folded > ocamlc_clean.folded
python3 clean_folded_inline.py < ocamlc.folded > ocamlc_clean_inline.folded
cp ocamlc_clean.folded ocamlc_clean_gcside.folded
cp ocamlc_clean.folded ocamlc_leafgroup.folded

log "Generating nameattrs"
python3 generate_nameattrs.py --scheme prefix \
  ocamlc.folded ocamlc_nameattrs_inline_full.txt
python3 generate_nameattrs.py --scheme prefix \
  ocamlc_clean.folded ocamlc_nameattrs.txt
python3 generate_nameattrs.py --scheme prefix \
  ocamlc_clean.folded ocamlc_nameattrs_leafgroup.txt
python3 generate_nameattrs.py --scheme prefix \
  ocamlc_clean.folded ocamlc_nameattrs_gcside.txt
python3 generate_nameattrs.py --scheme prefix \
  ocamlc_clean_inline.folded ocamlc_nameattrs_inline.txt
python3 generate_nameattrs.py --scheme ikinds \
  --ikinds-dir "$ROOT_DIR/typing/ikinds" \
  ocamlc_clean.folded ocamlc_nameattrs_ikinds.txt

log "Rendering SVGs"
"$FLAMEGRAPH_BIN" --width 1200 --minwidth 0.0005 \
  --nameattr ocamlc_nameattrs_leafgroup.txt \
  --title "$TITLE_PREFIX (~20x input, leaf-grouped icicle)" \
  ocamlc_clean.folded > ocamlc_flamegraph_leafgroup.svg

"$FLAMEGRAPH_BIN" --width 1200 --minwidth 0.0005 \
  --nameattr ocamlc_nameattrs_inline.txt \
  --title "$TITLE_PREFIX (~20x input, inline view)" \
  ocamlc_clean_inline.folded > ocamlc_flamegraph_inline.svg

"$FLAMEGRAPH_BIN" --width 1200 --minwidth 0.0005 \
  --nameattr ocamlc_nameattrs_gcside.txt \
  --title "$TITLE_PREFIX (~20x input, GC focus)" \
  ocamlc_clean_gcside.folded > ocamlc_flamegraph_gcside.svg

"$FLAMEGRAPH_BIN" --width 1200 --minwidth 0.0005 \
  --nameattr ocamlc_nameattrs_ikinds.txt \
  --title "$TITLE_PREFIX (~20x input, ikinds vs rest)" \
  ocamlc_clean.folded > ocamlc_flamegraph_ikinds.svg

if [[ -f ocamlc_nameattrs_modes.txt ]]; then
  "$FLAMEGRAPH_BIN" --width 1200 --minwidth 0.0005 \
    --nameattr ocamlc_nameattrs_modes.txt \
    --title "$TITLE_PREFIX (~20x input, modes spotlight)" \
    ocamlc_clean.folded > ocamlc_flamegraph_modes.svg
fi

log "Injecting custom CSS"
python3 inject_flamegraph_css.py ocamlc_flamegraph_leafgroup.svg
python3 inject_flamegraph_css.py ocamlc_flamegraph_inline.svg
python3 inject_flamegraph_css.py ocamlc_flamegraph_gcside.svg
python3 inject_flamegraph_css.py ocamlc_flamegraph_ikinds.svg
if [[ -f ocamlc_flamegraph_modes.svg ]]; then
  python3 inject_flamegraph_css.py ocamlc_flamegraph_modes.svg
fi

log "Flamegraphs ready in $SCRIPT_DIR"
