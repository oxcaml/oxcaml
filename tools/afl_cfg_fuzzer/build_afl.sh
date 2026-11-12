#!/usr/bin/env bash
#
# Build the AFL-instrumented CFG-validator fuzzer harness.
#
# Instrumentation is delivered through OCAMLPARAM, following the repo's
# BUILD_OCAMLPARAM convention (see Makefile.common-ox). The catch is that the
# boot dune workspace (dune-workspace) statically clears OCAMLPARAM, and
# BUILD_OCAMLPARAM is only injected into the "main" context --
# not the boot context that actually compiles the in-tree ocamloptcomp (where
# the validators live) with the opam compiler. So this script derives an
# instrumented copy of the boot workspace, replacing
#     ("OCAMLPARAM" "")
# with
#     ("OCAMLPARAM" "$BUILD_OCAMLPARAM")
# and builds the harness under it.
#
# A dedicated build directory is mandatory: dune does not treat OCAMLPARAM as a
# dependency, so reusing the root _build would leave instrumented objects
# behind and silently poison a later `make` of the compiler.
#
# Usage:
#   tools/afl_cfg_fuzzer/build_afl.sh [--clean]
#
# Options:
#   --clean            Remove the AFL build directory before building.
#
# Environment:
#   BUILD_OCAMLPARAM   OCAMLPARAM value injected into the build
#                      (default: "_,afl-instrument=1").
#   AFL_BUILD_DIR      dune build directory (default:
#                      tools/afl_cfg_fuzzer/_build/instrumented); must be new,
#                      empty, or previously marked by this script.

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd -P)
cd "$repo_root"

artifact_dir=tools/afl_cfg_fuzzer/_build
build_ocamlparam=${BUILD_OCAMLPARAM:-_,afl-instrument=1}
build_dir=${AFL_BUILD_DIR:-$artifact_dir/instrumented}
workspace=duneconf/afl.ws
target=tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe
corpus="$artifact_dir/corpus"
findings="$artifact_dir/findings"

clean=0
for arg in "$@"; do
  case "$arg" in
    --clean) clean=1 ;;
    -h | --help)
      grep '^#' "$0" | sed '1d;s/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "error: unknown argument: $arg" >&2
      exit 2
      ;;
  esac
done

# Resolve aliases before checking isolation, and before writing the workspace or
# cleaning anything. Neither the source tree nor the normal build may be removed
# or reused as the AFL build directory.
build_dir=$(realpath -m -- "$build_dir")
normal_build_dir=$(realpath -m -- "$repo_root/_build")
if [[ "$build_dir" == / ||
      "$repo_root/" == "$build_dir/"* ||
      "$normal_build_dir/" == "$build_dir/"* ||
      "$build_dir/" == "$normal_build_dir/"* ]]; then
  echo "error: AFL_BUILD_DIR must not contain the source tree" \
    "or overlap the root _build: $build_dir" >&2
  exit 2
fi

# Only claim new or empty directories. A marker from a previous invocation is
# required before reusing or deleting a non-empty directory.
build_dir_marker="$build_dir/.afl-cfg-fuzzer-build-dir"
if [ -e "$build_dir" ]; then
  if [ ! -d "$build_dir" ]; then
    echo "error: AFL_BUILD_DIR is not a directory: $build_dir" >&2
    exit 2
  fi
  if [ ! -f "$build_dir_marker" ] || [ -L "$build_dir_marker" ]; then
    nonempty=$(find "$build_dir" -mindepth 1 -maxdepth 1 -printf x -quit)
    if [ -n "$nonempty" ]; then
      echo "error: refusing to use an unmarked non-empty AFL_BUILD_DIR:" \
        "$build_dir" >&2
      echo "Choose a new or empty directory; old builds are not adopted." >&2
      exit 2
    fi
  fi
fi
exe="$build_dir/default/tools/afl_cfg_fuzzer/afl_cfg_fuzzer.exe"

# Locate dune the same way the Makefile does.
dune=$(sed -n 's/^DUNE=//p' Makefile.config 2>/dev/null | head -1 || true)
if [ -z "${dune:-}" ] || [ ! -x "$dune" ]; then
  dune=$(command -v dune || true)
fi
if [ -z "${dune:-}" ]; then
  echo "error: could not find dune (checked Makefile.config DUNE= and PATH)" >&2
  exit 1
fi

# The instrumented workspace reuses the boot context's prerequisites verbatim.
missing=0
for f in dune-project dune-workspace duneconf/dirs-to-ignore.inc \
  duneconf/ox-extra.inc boot_oc_cflags.sexp; do
  if [ ! -e "$f" ]; then
    echo "error: missing build prerequisite: $f" >&2
    missing=1
  fi
done
if [ "$missing" -ne 0 ]; then
  echo "Run a normal build first (e.g. 'make -s boot-compiler') to generate" >&2
  echo "the workspace prerequisites, then re-run this script." >&2
  exit 1
fi

# Derive the instrumented workspace from the real boot workspace so we track any
# future change to the boot context, swapping only the OCAMLPARAM value. awk's
# literal replacement avoids regex surprises (the only caveat is a bare '&').
awk -v val="$build_ocamlparam" '
  { sub(/\("OCAMLPARAM" ""\)/, "(\"OCAMLPARAM\" \"" val "\")"); print }
' dune-workspace >"$workspace"

if ! grep -qF "(\"OCAMLPARAM\" \"$build_ocamlparam\")" "$workspace"; then
  echo "error: failed to inject OCAMLPARAM into $workspace" >&2
  echo "       (does dune-workspace still contain (\"OCAMLPARAM\" \"\") ?)" >&2
  exit 1
fi

echo "Building $target"
echo "  dune:       $dune"
echo "  workspace:  $workspace"
echo "  build dir:  $build_dir"
echo "  OCAMLPARAM: $build_ocamlparam"
if [ "$clean" -eq 1 ]; then
  echo "  cleaning:   rm -rf $build_dir"
  rm -rf -- "$build_dir"
fi
mkdir -p -- "$build_dir"
touch -- "$build_dir_marker"

"$dune" build --root=. --workspace="$workspace" --build-dir="$build_dir" "$target"

if [ ! -x "$exe" ]; then
  echo "error: build reported success but $exe was not produced" >&2
  exit 1
fi

mkdir -p "$corpus"
"$exe" -write-corpus "$corpus"

# Confirm the binary is actually instrumented. afl-showmap aborts with "No
# instrumentation detected" on an uninstrumented target, which is the exact
# failure mode a stale build dir would produce.
seed="$corpus/diamond.bin"
if command -v afl-showmap >/dev/null 2>&1; then
  log=$(mktemp)
  map=$(mktemp)
  showmap_status=0
  AFL_QUIET=1 afl-showmap -m none -t 5000 -o "$map" -- "$exe" "$seed" \
    >"$log" 2>&1 || showmap_status=$?
  if grep -q "No instrumentation detected" "$log"; then
    echo "error: $exe is NOT instrumented" >&2
    echo "       BUILD_OCAMLPARAM was: $build_ocamlparam" >&2
    echo "       try re-running with --clean" >&2
    rm -f "$log" "$map"
    exit 1
  fi
  # Exit 2 means the seed crashed, which is expected during mutation testing.
  # Timeouts and other probe failures must not be reported as successful checks.
  if [ "$showmap_status" -ne 0 ] && [ "$showmap_status" -ne 2 ]; then
    echo "error: afl-showmap failed or timed out (5s limit):" >&2
    cat "$log" >&2
    rm -f "$log" "$map"
    exit 1
  fi
  tuples=$(grep -oE "Captured [0-9]+ tuples" "$log" | grep -oE "[0-9]+" | head -1 || true)
  if [ -n "${tuples:-}" ]; then
    echo "instrumentation OK: afl-showmap captured $tuples tuples on $(basename "$seed")"
  else
    echo "warning: could not confirm instrumentation; see afl-showmap output:" >&2
    cat "$log" >&2
  fi
  rm -f "$log" "$map"
else
  echo "note: skipping instrumentation check (afl-showmap not found)"
fi

cat <<EOF

Done. Instrumented harness:
  $exe

This harness signals a finding by raising SIGABRT, so AFL must be able to see
crashes. If core dumps are piped to a handler (core_pattern starts with '|', as
with systemd-coredump), point it at a plain file first (needs root):
  echo core | sudo tee /proc/sys/kernel/core_pattern
Without root you can proceed with AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1, but
crash detection is less reliable.

Start a coverage-guided campaign (classic AFL 2.52b; -m none is required
because the ocamloptcomp-linked binary exceeds AFL's default memory limit):
  AFL_SKIP_CPUFREQ=1 afl-fuzz -m none \\
    -i $corpus -o $findings \\
    -- $exe @@

Replay a finding as a dot graph:
  $exe -to-dot $findings/crashes/<id>
EOF
