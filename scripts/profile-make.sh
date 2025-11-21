#!/usr/bin/env bash

# Profiles a full `make` build (including descendant processes) with perf.
# Produces perf.data suitable for use with FlameGraph utilities.
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/profile-make.sh [options] [-- [make arguments...]]

Options:
  -o, --output PATH     Path for the perf.data output (default: repo_root/perf-make-<timestamp>.data)
  -F, --freq HZ         Sampling frequency passed to perf record (default: 400)
      --call-graph MODE Call graph mode for perf record (default: dwarf)
      --keep-build      Skip removing the _build directory before running make
  -h, --help            Show this help message

Any arguments after "--" (or any positional arguments that do not match the
options above) are forwarded to make.
USAGE
}

scripts_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )
repo_root="${scripts_dir}/.."

if ! command -v perf >/dev/null 2>&1; then
  echo "error: perf is not available on PATH" >&2
  exit 1
fi

freq=400
call_graph="dwarf"
perf_output=""
clean_build=true
make_args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      if [[ $# -lt 2 ]]; then
        echo "error: --output requires a value" >&2
        exit 1
      fi
      perf_output="$2"
      shift 2
      ;;
    -F|--freq)
      if [[ $# -lt 2 ]]; then
        echo "error: --freq requires a value" >&2
        exit 1
      fi
      freq="$2"
      shift 2
      ;;
    --call-graph)
      if [[ $# -lt 2 ]]; then
        echo "error: --call-graph requires a value" >&2
        exit 1
      fi
      call_graph="$2"
      shift 2
      ;;
    --keep-build)
      clean_build=false
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      make_args+=("$@")
      break
      ;;
    *)
      make_args+=("$1")
      shift
      ;;
  esac
done

if [[ -z "$perf_output" ]]; then
  timestamp=$(date +%Y%m%d-%H%M%S)
  perf_output="${repo_root}/perf-make-${timestamp}.data"
fi

build_dir="${repo_root}/_build"

if [[ "$clean_build" == true && -d "$build_dir" ]]; then
  echo "Removing ${build_dir} before profiling..."
  rm -rf -- "$build_dir"
fi

mkdir -p "$(dirname "$perf_output")"

echo "Recording perf profile to ${perf_output}" \
  "(freq=${freq}Hz, call-graph=${call_graph})"

cd "$repo_root"
perf record -F "$freq" --call-graph "$call_graph" -o "$perf_output" -- make "${make_args[@]}"

echo "Perf data saved to ${perf_output}"
echo "Generate a flame graph with e.g.:
  perf script -i ${perf_output} | stackcollapse-perf.pl > stacks.folded
  flamegraph.pl stacks.folded > flamegraph.svg"
