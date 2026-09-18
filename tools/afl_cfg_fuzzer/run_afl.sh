#!/usr/bin/env bash
#
# Launch an AFL fuzzing campaign against the CFG-validator harness.
#
# Builds the instrumented harness on demand (via build_afl.sh) and starts
# afl-fuzz with the flags this target needs:
#   * -m none          the ocamloptcomp-linked binary exceeds AFL's default
#                      memory limit, so the limit must be disabled;
#   * crash handling   the harness signals findings with SIGABRT, so if the
#                      kernel pipes core dumps to a handler (core_pattern starts
#                      with '|') we fall back to
#                      AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1 and warn. A plain
#                      core_pattern (settable only as root) is more reliable:
#                        echo core | sudo tee /proc/sys/kernel/core_pattern
#
# Usage:
#   tools/afl_cfg_fuzzer/run_afl.sh [--build] [--resume] [afl-fuzz options...]
#
# Options:
#   --build            Force a rebuild before launching.
#   --resume           Resume a previous campaign (afl-fuzz -i -).
#   Anything else is forwarded to afl-fuzz verbatim (placed before the target).
#
# Environment:
#   CORPUS             Seed input directory (default: tools/afl_cfg_fuzzer/corpus).
#   FINDINGS           Output directory (default: tools/afl_cfg_fuzzer/findings).
#   MAX_NODES          If set, passed to the harness as -max-nodes.
#   AFL_BUILD_DIR      dune build directory (default: _build_afl).

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)
cd "$repo_root"

build_dir=${AFL_BUILD_DIR:-_build_afl}
exe="$build_dir/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe"
corpus=${CORPUS:-tools/afl_cfg_fuzzer/corpus}
findings=${FINDINGS:-tools/afl_cfg_fuzzer/findings}

force_build=0
input_flag=("-i" "$corpus")
afl_extra=()
for arg in "$@"; do
  case "$arg" in
    --build) force_build=1 ;;
    --resume) input_flag=("-i" "-") ;;
    -h | --help)
      grep '^#' "$0" | sed '1d;s/^# \{0,1\}//'
      exit 0
      ;;
    *) afl_extra+=("$arg") ;;
  esac
done

if [ "$force_build" -eq 1 ] || [ ! -x "$exe" ]; then
  echo "Building instrumented harness..."
  "$script_dir/build_afl.sh"
fi
if [ ! -x "$exe" ]; then
  echo "error: $exe not found; run tools/afl_cfg_fuzzer/build_afl.sh" >&2
  exit 1
fi
if ! command -v afl-fuzz >/dev/null 2>&1; then
  echo "error: afl-fuzz not found in PATH" >&2
  exit 1
fi

# The harness aborts (SIGABRT) on a finding. AFL only reports crashes reliably
# when the kernel writes cores to a file rather than piping them to a handler.
export AFL_SKIP_CPUFREQ=1
core_pattern=$(cat /proc/sys/kernel/core_pattern 2>/dev/null || echo "")
case "$core_pattern" in
"|"*)
  echo "warning: core_pattern pipes cores to a handler:" >&2
  echo "           $core_pattern" >&2
  echo "         For reliable crash detection, run as root:" >&2
  echo "           echo core | sudo tee /proc/sys/kernel/core_pattern" >&2
  echo "         Proceeding with AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1." >&2
  export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
  ;;
esac

target=("$exe")
if [ -n "${MAX_NODES:-}" ]; then
  target+=("-max-nodes" "$MAX_NODES")
fi
target+=("@@")

echo "Launching afl-fuzz:"
echo "  target:   ${target[*]}"
echo "  input:    ${input_flag[*]}"
echo "  findings: $findings"
exec afl-fuzz -m none "${input_flag[@]}" -o "$findings" \
  ${afl_extra[@]+"${afl_extra[@]}"} -- "${target[@]}"
